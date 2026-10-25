use axum::{
    body::Bytes,
    extract::{OriginalUri, Path, Query},
    http::{HeaderMap, Method, StatusCode},
    response::{
        sse::{Event, KeepAlive, Sse},
        Json,
    },
};
use rand::Rng;
use serde_json::json;
use std::collections::HashMap;
use std::convert::Infallible;
use std::time::Duration;
use tokio_stream::{wrappers::ReceiverStream, Stream};

pub async fn health() -> StatusCode {
    StatusCode::OK
}

/// Fixed-latency endpoint. The open/closed fixtures target this so service
/// time is a stable, known quantity that sits comfortably above the
/// classifier's 1ms quantization (an instant handler would collapse a
/// request's own completion onto its own arrival, blurring open vs closed).
pub async fn fixed() -> StatusCode {
    const FIXED_LATENCY: Duration = Duration::from_millis(5);
    tokio::time::sleep(FIXED_LATENCY).await;
    StatusCode::OK
}

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

pub async fn not_found(uri: OriginalUri) -> (StatusCode, Json<serde_json::Value>) {
    (
        StatusCode::NOT_FOUND,
        Json(json!({ "error": "not found", "path": uri.path() })),
    )
}

/// Reflect the request back as JSON: method, uri, headers, query, and raw
/// body. Mirrors php-dev-server's `EchoController` (`ANY /api/echo`): a
/// "reflect everything" fixture whose body lets a client distinguish its own
/// response without session identity.
pub async fn echo(
    method: Method,
    uri: OriginalUri,
    headers: HeaderMap,
    query: Query<HashMap<String, String>>,
    body: Bytes,
) -> Json<serde_json::Value> {
    Json(json!({
        "method": method.as_str(),
        "uri": uri.to_string(),
        "headers": headers_to_json(&headers),
        "query": query.0,
        "body": String::from_utf8_lossy(&body).into_owned(),
    }))
}

/// Collapse a `HeaderMap` into `name -> [values]`, matching php-dev-server's
/// `$request->headers->all()` shape (multi-value headers become arrays).
fn headers_to_json(headers: &HeaderMap) -> serde_json::Value {
    let mut map = serde_json::Map::new();
    for name in headers.keys() {
        let values: Vec<String> = headers
            .get_all(name)
            .iter()
            .map(|v| v.to_str().unwrap_or_default().to_string())
            .collect();
        map.insert(
            name.as_str().to_string(),
            serde_json::Value::Array(
                values.into_iter().map(serde_json::Value::String).collect(),
            ),
        );
    }
    serde_json::Value::Object(map)
}

/// Controllable SSE stream. `?events=N` caps the message count (`0` =
/// unbounded), `?interval=S` sets the pause between messages in seconds (min
/// 1). Mirrors php-dev-server's `SseController` (`GET /api/sse`): a long-lived
/// streaming fixture whose completion only lands once the stream closes, so
/// its `C` record is deferred the same way the traffic logger promises.
pub async fn sse(
    Query(params): Query<HashMap<String, String>>,
) -> Sse<impl Stream<Item = Result<Event, Infallible>>> {
    let max_events = params
        .get("events")
        .and_then(|v| v.parse::<u32>().ok())
        .unwrap_or(0);
    let interval = params
        .get("interval")
        .and_then(|v| v.parse::<u64>().ok())
        .unwrap_or(1)
        .max(1);

    let (tx, rx) = tokio::sync::mpsc::channel::<Result<Event, Infallible>>(16);

    tokio::spawn(async move {
        let mut count = 0u32;
        loop {
            count += 1;
            // php-dev-server also stamped each event with an ISO-8601 `time`;
            // we drop it to avoid pulling in a chrono dependency.
            let msg = Event::default()
                .event("message")
                .id(count.to_string())
                .data(json!({ "id": count, "message": format!("Event #{}", count) }).to_string());

            if tx.send(Ok(msg)).await.is_err() {
                return;
            }

            if max_events > 0 && count >= max_events {
                let done = Event::default()
                    .event("done")
                    .data(json!({ "total": count }).to_string());
                let _ = tx.send(Ok(done)).await;
                return;
            }

            tokio::time::sleep(Duration::from_secs(interval)).await;
        }
    });

    Sse::new(ReceiverStream::new(rx)).keep_alive(KeepAlive::default())
}
