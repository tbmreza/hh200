use axum::{extract::Path, http::StatusCode, response::Json};
use rand::Rng;
use serde_json::json;
use std::time::Duration;

// ??: write inter_arrivals data
pub async fn delay(Path(delay): Path<f64>) -> (StatusCode, Json<serde_json::Value>) {
    let capped = delay.min(10.0);
    tokio::time::sleep(Duration::from_secs_f64(capped)).await;
    (StatusCode::OK, Json(json!({ "delay": capped })))
}

pub async fn status(Path(codes): Path<String>) -> StatusCode {
    let valid: Vec<StatusCode> = codes
        .split(',')
        .filter_map(|c| c.trim().parse::<u16>().ok())
        .filter_map(|c| StatusCode::from_u16(c).ok())
        .collect();

    match valid.as_slice() {
        [] => StatusCode::BAD_REQUEST,
        [single] => *single,
        many => many[rand::rng().random_range(0..many.len())],
    }
}

pub async fn bytes(Path(n): Path<usize>) -> Result<Vec<u8>, (StatusCode, String)> {
    const MAX_BYTES: usize = 1024 * 1024;
    if n > MAX_BYTES {
        return Err((
            StatusCode::PAYLOAD_TOO_LARGE,
            format!("n exceeds max of {MAX_BYTES} bytes"),
        ));
    }

    let mut buf = vec![0u8; n];
    rand::rng().fill(&mut buf[..]);
    Ok(buf)
}
