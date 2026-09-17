mod arrival_classifier;
mod control;
mod routes;

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
};


#[main]
async fn main() -> () {
    init();

    let addr = std::net::SocketAddr::from(([0, 0, 0, 0], 8080));
    info!("ocapi listening on address={addr}");

    let app = Router::new().route("/health", get(health))
                           .route("/fixed", get(health))
                           .route("/delay/{delay}", delete(routes::delay));

    let listener = TcpListener::bind(addr).await.unwrap();

    serve(listener, app).await.unwrap()
}

async fn health() -> impl IntoResponse {
    StatusCode::OK
}
