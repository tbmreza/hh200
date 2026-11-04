use std::net::SocketAddr;
use std::path::{Path, PathBuf};

use serde_json::Value;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::net::{TcpListener, TcpStream};
use tracing::{info, warn};

/// Raw byte-replay responder for curl's test `<reply>` data.
///
/// This deliberately bypasses axum so empty / malformed / split responses
/// survive byte-for-byte. For each request it:
///   1. reads the request head + body,
///   2. looks the request-target up in `manifest.json` (written by CurlSpec),
///   3. records the normalized request to `requests/<N>.txt`,
///   4. writes `N.part0` back raw and closes the connection.
pub async fn serve(addr: SocketAddr, replay_dir: PathBuf) {
    std::fs::create_dir_all(replay_dir.join("requests"))
        .expect("failed to create requests dir");

    let listener = TcpListener::bind(addr)
        .await
        .expect("failed to bind replay listener");
    info!("replay listening on address={addr} dir={replay_dir:?}");

    loop {
        match listener.accept().await {
            Ok((stream, _)) => {
                let dir = replay_dir.clone();
                tokio::spawn(async move {
                    if let Err(e) = handle(stream, dir).await {
                        warn!("replay connection error: {e}");
                    }
                });
            }
            Err(e) => warn!("replay accept error: {e}"),
        }
    }
}

async fn handle(mut stream: TcpStream, dir: PathBuf) -> std::io::Result<()> {
    let mut buf = Vec::new();
    let mut tmp = [0u8; 8192];

    // Read until we have the full head (`\r\n\r\n`), up to 64 KiB.
    let head_end = loop {
        let n = stream.read(&mut tmp).await?;
        if n == 0 {
            break buf.len();
        }
        buf.extend_from_slice(&tmp[..n]);
        if let Some(pos) = find_subslice(&buf, b"\r\n\r\n") {
            break pos + 4;
        }
        if buf.len() > 64 * 1024 {
            break buf.len();
        }
    };

    let head = &buf[..head_end];
    let (request_line, headers) = parse_head(head);

    // Read the body per Content-Length (chunked uploads are not used by hh200).
    let mut body = buf[head_end..].to_vec();
    let body_len = content_length(&headers);
    while body.len() < body_len {
        let n = stream.read(&mut tmp).await?;
        if n == 0 {
            break;
        }
        body.extend_from_slice(&tmp[..n]);
    }
    body.truncate(body_len);

    let method = request_line.split_whitespace().next().unwrap_or("").to_string();
    let target = request_line.split_whitespace().nth(1).unwrap_or("").to_string();

    let Some(testnum) = route(&dir, &method, &target) else {
        // Unknown route: close without responding (mirrors a dead endpoint).
        return Ok(());
    };

    record_request(&dir, testnum, &request_line, &headers, &body).await?;

    let reply = tokio::fs::read(dir.join(format!("{testnum}.part0")))
        .await
        .unwrap_or_default();
    stream.write_all(&reply).await?;
    let _ = stream.shutdown().await;
    Ok(())
}

fn route(dir: &Path, method: &str, target: &str) -> Option<u64> {
    let manifest = std::fs::read_to_string(dir.join("manifest.json")).ok()?;
    let v: Value = serde_json::from_str(&manifest).ok()?;
    let key = format!("{method} {target}");
    v.get("routes")?.as_object()?.get(&key)?.as_u64()
}

async fn record_request(
    dir: &Path,
    testnum: u64,
    request_line: &str,
    headers: &[(String, String)],
    body: &[u8],
) -> std::io::Result<()> {
    let mut out = Vec::new();
    out.extend_from_slice(request_line.as_bytes());
    out.extend_from_slice(b"\r\n");
    for (name, value) in headers {
        out.extend_from_slice(format!("{name}: {value}\r\n").as_bytes());
    }
    out.extend_from_slice(b"\r\n");
    out.extend_from_slice(body);
    let path = dir.join("requests").join(format!("{testnum}.txt"));
    tokio::fs::write(path, out).await
}

fn parse_head(head: &[u8]) -> (String, Vec<(String, String)>) {
    let text = String::from_utf8_lossy(head);
    let mut lines = text.split("\r\n");
    let request_line = lines.next().unwrap_or("").to_string();
    let headers = lines
        .filter(|l| !l.is_empty())
        .filter_map(|l| {
            let (name, value) = l.split_once(':')?;
            Some((name.trim().to_string(), value.trim().to_string()))
        })
        .collect();
    (request_line, headers)
}

fn content_length(headers: &[(String, String)]) -> usize {
    headers
        .iter()
        .find(|(n, _)| n.eq_ignore_ascii_case("content-length"))
        .and_then(|(_, v)| v.parse::<usize>().ok())
        .unwrap_or(0)
}

fn find_subslice(haystack: &[u8], needle: &[u8]) -> Option<usize> {
    if needle.is_empty() || haystack.len() < needle.len() {
        return None;
    }
    haystack
        .windows(needle.len())
        .position(|window| window == needle)
}
