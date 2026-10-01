mod arrival_classifier;
mod control;
mod logger;
mod routes;

use tracing_subscriber::fmt::init;
use tracing::info;
use tokio::net::TcpListener;
use tokio::main;
use axum::{
    serve as axum_serve,
    Router,
    response::IntoResponse,
    http::StatusCode,
    routing::{get, delete},
    middleware::{Next},
    extract::{Request, State},
};
use std::sync::Arc;
use std::path::PathBuf;
use crate::logger::TrafficLogger;
use uuid::Uuid;
use clap::{Parser, Subcommand};
use etcetera::BaseStrategy;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Run the controllable HTTP system-under-test.
    Serve {
        /// Path to append the traffic dump to (written by the server).
        /// Defaults to an XDG data directory when omitted.
        #[arg(short, long)]
        dump_path: Option<PathBuf>,

        /// Port to listen on.
        #[arg(short, long, default_value_t = 8080)]
        port: u16,
    },

    /// Read a traffic dump and run the arrival classifier.
    Analyze {
        /// Path to the traffic dump to read.
        /// Defaults to the same XDG data directory as `serve` when omitted.
        #[arg(short, long)]
        dump_path: Option<PathBuf>,
    },

    /// Check configuration and environment health.
    Doctor,
}

#[derive(Clone)]
struct AppState {
    logger: Arc<TrafficLogger>,
}

async fn traffic_logger_middleware(
    State(state): State<AppState>,
    request: Request,
    next: Next,
) -> axum::response::Response {
    let id = Uuid::new_v4();
    info!(method = %request.method(), uri = %request.uri(), id = %id, "request arrival");
    state.logger.log_arrival(id).await;

    let response = next.run(request).await;

    info!(id = %id, status = %response.status(), "request completion");
    state.logger.log_completion(id).await;
    response.into_response()
}

#[main]
async fn main() {
    init();

    let cli = Cli::parse();

    match cli.command {
        Commands::Serve { dump_path, port } => {
            serve(dump_path.unwrap_or_else(default_dump_path), port).await
        }
        Commands::Analyze { dump_path } => analyze(dump_path.unwrap_or_else(default_dump_path)),
        Commands::Doctor => doctor(),
    }
}

async fn serve(dump_path: PathBuf, port: u16) {
    if let Some(parent) = dump_path.parent() {
        std::fs::create_dir_all(parent)
            .unwrap_or_else(|e| panic!("failed to create dump dir {parent:?}: {e}"));
    }

    let logger = Arc::new(TrafficLogger::new(&dump_path));
    let state = AppState { logger };

    let addr = std::net::SocketAddr::from(([0, 0, 0, 0], port));
    info!("ocapi listening on address={addr}");

    let app = build_router(state);

    let listener = TcpListener::bind(addr).await.unwrap();

    axum_serve(listener, app).await.unwrap()
}

fn build_router(state: AppState) -> Router {
    Router::new()
        .route("/health", get(health))
        .route("/fixed", get(health))
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

fn analyze(dump_path: PathBuf) {
    let text = std::fs::read_to_string(&dump_path)
        .unwrap_or_else(|e| panic!("failed to read dump {dump_path:?}: {e}"));

    let events = arrival_classifier::parse_dump(&text)
        .unwrap_or_else(|e| panic!("failed to parse dump {dump_path:?}: {e:?}"));

    let (_, gaps) = arrival_classifier::events_to_inter_arrivals(&events);

    let completed = events.iter().filter(|e| e.completion.is_some()).count();
    let dangling = events.len() - completed;

    println!("events:     {}", events.len());
    println!("completed:  {completed}");
    println!("dangling:   {dangling}");

    if gaps.is_empty() {
        println!("inter-arrival gaps: none");
        return;
    }

    let sum_ns: u128 = gaps.iter().map(|g| g.as_nanos()).sum();
    let min = gaps.iter().min().unwrap();
    let max = gaps.iter().max().unwrap();
    let mean = std::time::Duration::from_nanos((sum_ns / gaps.len() as u128) as u64);

    println!("gaps:       {}", gaps.len());
    println!("gap min:    {min:?}");
    println!("gap max:    {max:?}");
    println!("gap mean:   {mean:?}");
}

fn doctor() {
    println!("config path: {:?}", config_path());
    println!(
        "config exists: {}",
        config_path().exists()
    );

    let dump_path = default_dump_path();
    println!("dump path: {:?}", dump_path);

    let writable = dump_path.parent().is_some_and(|parent| {
        if std::fs::create_dir_all(parent).is_err() {
            return false;
        }
        let probe = parent.join(".ocapi-write-probe");
        let ok = std::fs::write(&probe, b"").is_ok();
        let _ = std::fs::remove_file(&probe);
        ok
    });

    println!("dump dir writable: {writable}");
}

async fn health() -> impl IntoResponse {
    StatusCode::OK
}

/// Directory holding ocapi's per-user data (traffic dumps), per XDG.
fn data_dir() -> PathBuf {
    let dirs = etcetera::base_strategy::choose_base_strategy()
        .expect("failed to determine platform base directories");

    dirs.data_dir().join("ocapi")
}

fn default_dump_path() -> PathBuf {
    data_dir().join("traffic.dump")
}

fn config_path() -> PathBuf {
    if cfg!(debug_assertions) {
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("config.toml")
    } else {
        let dirs = etcetera::base_strategy::choose_base_strategy()
            .expect("failed to determine platform base directories");

        dirs.config_dir()
            .join("ocapi")
            .join("config.toml")
    }
}

#[test]
fn config_path_uses_expected_location() {
    let path = config_path();

    // unless --release, read from cargo root.
    if cfg!(debug_assertions) {
        assert!(
            path.starts_with(env!("CARGO_MANIFEST_DIR")),
            "debug build should read from repository: {path:?}"
        );
    } else {
        let dirs = etcetera::base_strategy::choose_base_strategy().unwrap();

        assert!(
            path.starts_with(dirs.config_dir()),
            "release build should read from XDG config directory: {path:?}"
        );
    }
}

#[cfg(test)]
mod request_harness {
    use super::*;
    use axum::body::Body;
    use axum::http::Request;
    use http_body_util::BodyExt;
    use tower::ServiceExt;

    /// Builds a router backed by a throwaway traffic log, returning both the
    /// state (already consumed by the router) and the log path for cleanup.
    fn temp_state() -> (AppState, PathBuf) {
        let path = std::env::temp_dir().join(format!("ocapi-test-{}.dump", Uuid::new_v4()));
        let state = AppState {
            logger: Arc::new(TrafficLogger::new(&path)),
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
