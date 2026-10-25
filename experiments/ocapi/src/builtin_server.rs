use std::sync::Arc;

use axum::{
    extract::{Request, State},
    middleware::Next,
    response::IntoResponse,
    routing::{any, delete, get},
    serve as axum_serve,
    Router,
};
use tokio::net::TcpListener;
use tokio::sync::oneshot;
use tracing::{info, warn};

use crate::logger::TrafficLogger;
use crate::routes;

#[derive(Clone)]
struct AppState {
    logger: Arc<dyn TrafficLogger>,
}

async fn traffic_logger_middleware(
    State(state): State<AppState>,
    request: Request,
    next: Next,
) -> axum::response::Response {
    let id = state.logger.log_arrival().await;
    let method = request.method().clone();
    let uri = request.uri().clone();
    let headers = request.headers().clone();
    info!(method = %method, uri = %uri, id = %id, "request arrival");

    let response = next.run(request).await;

    let status = response.status();
    info!(id = %id, status = %status, "request completion");

    // Request-dump-on-failure: mirror php-dev-server's "log the full request
    // when a request fails" debugging aid. Headers + uri are captured above
    // (the query string rides in the uri); the body is deliberately not
    // buffered here (consuming it would change request semantics) and is
    // instead available on demand via the `/echo` reflect route.
    if !status.is_success() {
        warn!(id = %id, method = %method, uri = %uri, status = %status, headers = ?headers, "request failed");
    }

    state.logger.log_completion(id).await;
    response.into_response()
}

/// Runs the HTTP SUT. `logger` is either the file-backed logger or the TUI
/// logger; `shutdown` is `Some` in TUI mode so quitting the TUI stops the
/// server gracefully.
pub async fn serve(
    port: u16,
    logger: Arc<dyn TrafficLogger>,
    shutdown: Option<oneshot::Receiver<()>>,
) {
    let state = AppState { logger };

    let addr = std::net::SocketAddr::from(([0, 0, 0, 0], port));
    info!("ocapi listening on address={addr}");

    let app = build_router(state);

    let listener = TcpListener::bind(addr).await.unwrap();

    match shutdown {
        Some(rx) => axum_serve(listener, app)
            .with_graceful_shutdown(async move {
                // Ok(()) on user quit, Err(_) if the TUI thread died first.
                let _ = rx.await;
            })
            .await
            .unwrap(),
        None => axum_serve(listener, app).await.unwrap(),
    }
}

fn build_router(state: AppState) -> Router {
    Router::new()
        .route("/health", get(routes::health))
        .route("/fixed", get(routes::fixed))
        .route("/delay/{delay}", delete(routes::delay))
        .route("/status/{code}", get(routes::status))
        .route("/bytes/{n}", get(routes::bytes))
        .route("/echo", any(routes::echo))
        .route("/sse", get(routes::sse))
        .fallback(routes::not_found)
        .layer(axum::middleware::from_fn_with_state(
            state.clone(),
            traffic_logger_middleware,
        ))
        .with_state(state)
}

#[cfg(test)]
mod request_harness {
    use super::*;
    use std::path::PathBuf;

    use axum::body::Body;
    use axum::http::{Request, StatusCode};
    use http_body_util::BodyExt;
    use tower::ServiceExt;

    use crate::logger::FileLogger;

    /// Builds a router backed by a throwaway traffic log, returning both the
    /// state (already consumed by the router) and the log path for cleanup.
    fn temp_state() -> (AppState, PathBuf) {
        let path = std::env::temp_dir().join(format!("ocapi-test-{}.dump", uuid::Uuid::new_v4()));
        let state = AppState {
            logger: Arc::new(FileLogger::new(&path)),
        };
        (state, path)
    }

    fn get(uri: &str) -> Request<Body> {
        Request::builder()
            .uri(uri)
            .body(Body::empty())
            .expect("valid request")
    }

    async fn body_json(response: axum::response::Response) -> serde_json::Value {
        let bytes = response
            .into_body()
            .collect()
            .await
            .expect("body should be collectable")
            .to_bytes();
        serde_json::from_slice(&bytes).expect("body should be json")
    }

    async fn body_text(response: axum::response::Response) -> String {
        let bytes = response
            .into_body()
            .collect()
            .await
            .expect("body should be collectable")
            .to_bytes();
        String::from_utf8_lossy(&bytes).into_owned()
    }

    #[tokio::test]
    async fn health_returns_ok() {
        let (state, path) = temp_state();
        let response = build_router(state)
            .oneshot(get("/health"))
            .await
            .expect("response");

        assert_eq!(response.status(), StatusCode::OK);
        std::fs::remove_file(path).ok();
    }

    #[tokio::test]
    async fn status_route_returns_requested_code() {
        let (state, path) = temp_state();
        let response = build_router(state)
            .oneshot(get("/status/201"))
            .await
            .expect("response");

        assert_eq!(response.status(), StatusCode::CREATED);
        std::fs::remove_file(path).ok();
    }

    #[tokio::test]
    async fn unknown_path_returns_json_404() {
        let (state, path) = temp_state();
        let response = build_router(state)
            .oneshot(get("/does/not/exist"))
            .await
            .expect("response");

        assert_eq!(response.status(), StatusCode::NOT_FOUND);
        assert_eq!(
            body_json(response).await,
            serde_json::json!({ "error": "not found", "path": "/does/not/exist" })
        );
        std::fs::remove_file(path).ok();
    }

    #[tokio::test]
    async fn echo_returns_request_details() {
        let (state, path) = temp_state();
        let request = Request::builder()
            .method("GET")
            .uri("/echo?x=1")
            .header("x-test", "hello")
            .body(Body::empty())
            .expect("valid request");

        let response = build_router(state)
            .oneshot(request)
            .await
            .expect("response");

        assert_eq!(response.status(), StatusCode::OK);
        let body = body_json(response).await;
        assert_eq!(body["method"], serde_json::json!("GET"));
        assert_eq!(body["uri"], serde_json::json!("/echo?x=1"));
        assert_eq!(body["query"]["x"], serde_json::json!("1"));
        assert_eq!(body["headers"]["x-test"], serde_json::json!(["hello"]));
        assert_eq!(body["body"], serde_json::json!(""));
        std::fs::remove_file(path).ok();
    }

    #[tokio::test]
    async fn sse_streams_message_and_done_events() {
        let (state, path) = temp_state();
        let response = build_router(state)
            .oneshot(get("/sse?events=1&interval=1"))
            .await
            .expect("response");

        assert_eq!(response.status(), StatusCode::OK);
        let content_type = response
            .headers()
            .get("content-type")
            .and_then(|v| v.to_str().ok())
            .unwrap_or_default()
            .to_string();
        assert!(
            content_type.starts_with("text/event-stream"),
            "unexpected content-type: {content_type:?}"
        );

        let text = body_text(response).await;
        assert!(text.contains("event: message"), "missing message event: {text:?}");
        assert!(text.contains("event: done"), "missing done event: {text:?}");
        assert!(text.contains("\"total\":1"), "done event should total 1: {text:?}");
        std::fs::remove_file(path).ok();
    }

    #[tokio::test]
    async fn non_success_status_flows_through_middleware() {
        let (state, path) = temp_state();
        let response = build_router(state)
            .oneshot(get("/status/500"))
            .await
            .expect("response");

        assert_eq!(response.status(), StatusCode::INTERNAL_SERVER_ERROR);
        std::fs::remove_file(path).ok();
    }
}
