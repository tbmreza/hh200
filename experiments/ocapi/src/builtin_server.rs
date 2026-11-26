use std::sync::Arc;

use axum::{
    extract::{DefaultBodyLimit, Request, State},
    middleware::Next,
    response::IntoResponse,
    routing::{any, delete, get},
    serve as axum_serve,
    Router,
};
use tokio::net::TcpListener;
use tokio::sync::oneshot;
use tracing::{info, warn};

use crate::echo;
use crate::logger::TrafficLogger;
use crate::routes;

#[derive(Clone)]
struct AppState {
    logger: Arc<dyn TrafficLogger>,
    /// Body size cap for the `/echo` routes only; every other route keeps
    /// axum's 2 MB default.
    echo_max_bytes: usize,
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
/// server gracefully. `echo_max_bytes` caps `/echo` request bodies.
pub async fn serve(
    port: u16,
    logger: Arc<dyn TrafficLogger>,
    echo_max_bytes: usize,
    shutdown: Option<oneshot::Receiver<()>>,
) {
    let state = AppState { logger, echo_max_bytes };

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
        // `/api/echo` matches php-dev-server's path, which hh200's examples use.
        .route(
            "/echo",
            any(echo::echo).layer(DefaultBodyLimit::max(state.echo_max_bytes)),
        )
        .route(
            "/api/echo",
            any(echo::echo).layer(DefaultBodyLimit::max(state.echo_max_bytes)),
        )
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
            echo_max_bytes: echo::DEFAULT_MAX_BYTES,
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

    fn post(uri: &str, content_type: &str, body: impl Into<Body>) -> Request<Body> {
        Request::builder()
            .method("POST")
            .uri(uri)
            .header("content-type", content_type)
            .body(body.into())
            .expect("valid request")
    }

    async fn echo_json(state: AppState, request: Request<Body>) -> (StatusCode, serde_json::Value) {
        let response = build_router(state).oneshot(request).await.expect("response");
        let status = response.status();
        (status, body_json(response).await)
    }

    const PNG: &[u8] = b"\x89PNG\r\n\x1a\n\x00\x00\x00\rIHDR\xff";

    #[tokio::test]
    async fn echo_keeps_binary_body_lossless() {
        use base64::Engine;
        let (state, path) = temp_state();
        let (status, body) = echo_json(state, post("/echo", "image/png", PNG.to_vec())).await;

        assert_eq!(status, StatusCode::OK);
        assert_eq!(body["body_len"], serde_json::json!(PNG.len()));
        assert_eq!(body["body_utf8"], serde_json::json!(false));
        let round_trip = base64::engine::general_purpose::STANDARD
            .decode(body["body_base64"].as_str().unwrap())
            .unwrap();
        assert_eq!(round_trip, PNG);
        assert_eq!(body["content_type"], serde_json::json!("image/png"));
        std::fs::remove_file(path).ok();
    }

    #[tokio::test]
    async fn echo_sha256_matches_known_vector() {
        let (state, path) = temp_state();
        let (_, body) = echo_json(state, post("/echo", "text/plain", "abc")).await;
        assert_eq!(
            body["body_sha256"],
            serde_json::json!("ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
        );
        assert_eq!(body["body"], serde_json::json!("abc"));
        assert_eq!(body["json"], serde_json::Value::Null);
        assert_eq!(body["base64"], serde_json::Value::Null);
        std::fs::remove_file(path).ok();
    }

    #[tokio::test]
    async fn echo_reports_base64_field_of_json_body() {
        use base64::Engine;
        let (state, path) = temp_state();
        let encoded = base64::engine::general_purpose::STANDARD.encode(PNG);
        let payload = serde_json::json!({ "image": format!("data:image/png;base64,{encoded}") });
        let request = post("/echo?b64=/image", "application/json", payload.to_string());
        let (status, body) = echo_json(state, request).await;

        assert_eq!(status, StatusCode::OK);
        assert_eq!(body["json"], payload);
        let report = &body["base64"];
        assert_eq!(report["source"], serde_json::json!("/image"));
        assert_eq!(report["data_uri"]["mime"], serde_json::json!("image/png"));
        assert_eq!(report["decoded"]["len"], serde_json::json!(PNG.len()));
        assert_eq!(report["decoded"]["sniffed_type"], serde_json::json!("image/png"));
        assert_eq!(report["warnings"].as_array().unwrap().len(), 1, "{report}");
        std::fs::remove_file(path).ok();
    }

    #[tokio::test]
    async fn echo_base64_target_errors_are_reported() {
        let (state, path) = temp_state();
        let request = post("/echo?b64=/missing", "application/json", r#"{"image": 1}"#);
        let (status, body) = echo_json(state, request).await;
        assert_eq!(status, StatusCode::OK);
        assert_eq!(body["base64"]["error"], serde_json::json!("no value at /missing"));

        let (state, path2) = temp_state();
        let request = post("/echo?b64=/image", "application/json", r#"{"image": 1}"#);
        let (_, body) = echo_json(state, request).await;
        assert_eq!(body["base64"]["error"], serde_json::json!("value at /image is a number, not a string"));
        std::fs::remove_file(path).ok();
        std::fs::remove_file(path2).ok();
    }

    #[tokio::test]
    async fn echo_lists_multipart_parts_and_reports_a_part() {
        use base64::Engine;
        let (state, path) = temp_state();
        let encoded = base64::engine::general_purpose::STANDARD.encode(PNG);
        let mut multipart = Vec::new();
        multipart.extend_from_slice(
            b"--XyZ\r\nContent-Disposition: form-data; name=\"file\"; filename=\"a.png\"\r\n\
              Content-Type: image/png\r\n\r\n",
        );
        multipart.extend_from_slice(PNG);
        multipart.extend_from_slice(b"\r\n--XyZ\r\nContent-Disposition: form-data; name=\"image_b64\"\r\n\r\n");
        multipart.extend_from_slice(encoded.as_bytes());
        multipart.extend_from_slice(b"\r\n--XyZ--\r\n");

        let request = post("/echo?b64=part:image_b64", "multipart/form-data; boundary=XyZ", multipart);
        let (status, body) = echo_json(state, request).await;

        assert_eq!(status, StatusCode::OK);
        let mp = &body["multipart"];
        assert_eq!(mp["error"], serde_json::Value::Null, "{mp}");
        let parts = mp["parts"].as_array().unwrap();
        assert_eq!(parts.len(), 2);
        assert_eq!(parts[0]["name"], serde_json::json!("file"));
        assert_eq!(parts[0]["filename"], serde_json::json!("a.png"));
        assert_eq!(parts[0]["content_type"], serde_json::json!("image/png"));
        assert_eq!(parts[0]["len"], serde_json::json!(PNG.len()));
        assert_eq!(parts[0]["sniffed_type"], serde_json::json!("image/png"));
        assert_eq!(parts[0]["text"], serde_json::Value::Null);
        assert_eq!(parts[1]["text"], serde_json::json!(encoded));
        assert_eq!(body["base64"]["source"], serde_json::json!("part:image_b64"));
        assert_eq!(body["base64"]["decoded"]["sha256"], parts[0]["sha256"]);
        std::fs::remove_file(path).ok();
    }

    #[tokio::test]
    async fn echo_accepts_bodies_over_axums_2mb_default() {
        let (state, path) = temp_state();
        let big = vec![b'A'; 3 * 1024 * 1024];
        let (status, body) = echo_json(state, post("/echo", "text/plain", big)).await;
        assert_eq!(status, StatusCode::OK);
        assert_eq!(body["body_len"], serde_json::json!(3 * 1024 * 1024));
        std::fs::remove_file(path).ok();
    }

    #[tokio::test]
    async fn echo_over_limit_returns_json_413() {
        let (mut state, path) = temp_state();
        state.echo_max_bytes = 10;
        let (status, body) = echo_json(state, post("/echo", "text/plain", "0123456789A")).await;
        assert_eq!(status, StatusCode::PAYLOAD_TOO_LARGE);
        assert!(body["error"].as_str().unwrap().contains("length limit"), "{body}");
        assert!(body["hint"].as_str().unwrap().contains("--echo-max-bytes"));
        std::fs::remove_file(path).ok();
    }

    #[tokio::test]
    async fn echo_keeps_repeated_query_values_and_non_ascii_headers() {
        let (state, path) = temp_state();
        let request = Request::builder()
            .uri("/api/echo?a=1&a=2&b=3")
            .header("x-name", axum::http::HeaderValue::from_bytes("café".as_bytes()).unwrap())
            .body(Body::empty())
            .expect("valid request");
        let (status, body) = echo_json(state, request).await;

        assert_eq!(status, StatusCode::OK);
        assert_eq!(body["query"], serde_json::json!({ "a": "2", "b": "3" }));
        assert_eq!(body["query_all"], serde_json::json!({ "a": ["1", "2"], "b": ["3"] }));
        assert_eq!(body["headers"]["x-name"], serde_json::json!(["café"]));
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
