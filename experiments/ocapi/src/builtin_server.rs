use std::sync::Arc;

use axum::{
    extract::{Request, State},
    middleware::Next,
    response::IntoResponse,
    routing::{delete, get},
    serve as axum_serve,
    Router,
};
use tokio::net::TcpListener;
use tokio::sync::oneshot;
use tracing::info;

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
    info!(method = %request.method(), uri = %request.uri(), id = %id, "request arrival");

    let response = next.run(request).await;

    info!(id = %id, status = %response.status(), "request completion");
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
}
