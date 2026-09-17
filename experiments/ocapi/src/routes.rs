use axum::{extract::Path, http::StatusCode, response::Json};
use serde_json::json;
use std::time::Duration;

pub async fn delay(Path(delay): Path<f64>) -> (StatusCode, Json<serde_json::Value>) {
    let capped = delay.min(10.0);
    tokio::time::sleep(Duration::from_secs_f64(capped)).await;
    (StatusCode::OK, Json(json!({ "delay": capped })))
}
