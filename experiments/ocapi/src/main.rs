mod arrival_classifier;
mod control;
mod routes;
mod logger;

use tracing_subscriber::fmt::init;
use tracing::info;
use tokio::net::TcpListener;
use tokio::main;
use axum::{
    serve,
    Router,
    response::IntoResponse,
    http::StatusCode,
    routing::{get, delete},
    middleware::{Next},
    extract::{Request, State},
};
use std::sync::Arc;
use crate::logger::TrafficLogger;
use uuid::Uuid;
use clap::Parser;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(short, long)]
    dump_path: String,
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
    state.logger.log_arrival(id).await;
    
    let response = next.run(request).await;
    
    state.logger.log_completion(id).await;
    response.into_response()
}

#[main]
async fn main() {
    init();

    let args = Args::parse();
    let logger = Arc::new(TrafficLogger::new(&args.dump_path));
    let state = AppState { logger };

    let addr = std::net::SocketAddr::from(([0, 0, 0, 0], 8080));
    info!("ocapi listening on address={addr}");

    let app = Router::new().route("/health", get(health))
                           .route("/fixed", get(health))
                           .route("/delay/{delay}", delete(routes::delay))
                           .route("/status/{code}", get(routes::status))
                           .route("/bytes/{n}", get(routes::bytes))
                           .layer(axum::middleware::from_fn_with_state(state.clone(), traffic_logger_middleware))
                           .with_state(state);

    let listener = TcpListener::bind(addr).await.unwrap();

    serve(listener, app).await.unwrap()
}

async fn health() -> impl IntoResponse {
    StatusCode::OK
}
