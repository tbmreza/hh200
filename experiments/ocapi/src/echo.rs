//! The `/echo` reflect route and the payload diagnostics it reports.
//!
//! The response keeps the original php-dev-server-shaped fields (`method`,
//! `uri`, `headers`, `query`, `body`) and adds lossless and diagnostic views
//! of the body, aimed at debugging clients that send binary payloads (for
//! instance a base64-encoded image) in a request:
//!
//! - `body_len`, `body_base64`, `body_sha256`, `body_utf8`: the body exactly
//!   as received, so binary bytes are never lost to `body`'s lossy decoding.
//! - `json`: the parsed body when the content type says JSON.
//! - `multipart`: per-part details for `multipart/form-data`.
//! - `base64`: a report on one base64 value, chosen with `?b64=` (see
//!   [`B64Target`]).
//! - `query_all`: every value of every query key (`query` keeps one value
//!   per key, the last).

use std::collections::BTreeMap;
use std::convert::Infallible;

use axum::{
    body::Bytes,
    extract::{OriginalUri, Query, rejection::BytesRejection},
    http::{HeaderMap, Method, StatusCode, header::CONTENT_TYPE},
    response::Json,
};
use base64::{
    Engine, alphabet,
    engine::{DecodePaddingMode, GeneralPurpose, GeneralPurposeConfig, general_purpose::STANDARD},
};
use serde_json::{Value, json};
use sha2::{Digest, Sha256};

/// Default cap on an `/echo` request body. Axum's own default is 2 MB, which
/// rejects any image over ~1.5 MB once base64 has added its third.
pub const DEFAULT_MAX_BYTES: usize = 25 * 1024 * 1024;

pub async fn echo(
    method: Method,
    uri: OriginalUri,
    headers: HeaderMap,
    Query(query): Query<Vec<(String, String)>>,
    body: Result<Bytes, BytesRejection>,
) -> (StatusCode, Json<Value>) {
    let body = match body {
        Ok(body) => body,
        Err(rejection) => {
            let status = rejection.status();
            let mut err = json!({ "error": rejection.body_text() });
            if status == StatusCode::PAYLOAD_TOO_LARGE {
                err["hint"] = json!("raise the limit with `serve --echo-max-bytes N`");
            }
            return (status, Json(err));
        }
    };

    let content_type = headers
        .get(CONTENT_TYPE)
        .map(|v| String::from_utf8_lossy(v.as_bytes()).into_owned());
    let mime = content_type.as_deref().map(|ct| {
        ct.split(';')
            .next()
            .unwrap_or("")
            .trim()
            .to_ascii_lowercase()
    });

    let parsed_json = match mime.as_deref() {
        Some(m) if m == "application/json" || m.ends_with("+json") => {
            serde_json::from_slice::<Value>(&body).ok()
        }
        _ => None,
    };

    let parts = match (mime.as_deref(), content_type.as_deref()) {
        (Some("multipart/form-data"), Some(ct)) => Some(parse_multipart(ct, body.clone()).await),
        _ => None,
    };

    let base64 = query
        .iter()
        .rev()
        .find(|(k, _)| k == "b64")
        .map(|(_, v)| B64Target::parse(v))
        .map(|target| target.report(&body, parsed_json.as_ref(), parts.as_ref()));

    // `query` keeps its original one-value-per-key shape (last value wins);
    // `query_all` keeps every value in order.
    let mut query_last = serde_json::Map::new();
    let mut query_all: BTreeMap<&str, Vec<&str>> = BTreeMap::new();
    for (k, v) in &query {
        query_last.insert(k.clone(), json!(v));
        query_all.entry(k).or_default().push(v);
    }

    let response = json!({
        "method": method.as_str(),
        "uri": uri.to_string(),
        "headers": headers_to_json(&headers),
        "query": query_last,
        "query_all": query_all,
        "body": String::from_utf8_lossy(&body),
        "body_len": body.len(),
        "body_base64": STANDARD.encode(&body),
        "body_sha256": sha256_hex(&body),
        "body_utf8": std::str::from_utf8(&body).is_ok(),
        "content_type": content_type,
        "json": parsed_json,
        "multipart": parts.as_ref().map(Parts::to_json),
        "base64": base64,
    });
    (StatusCode::OK, Json(response))
}

/// Collapse a `HeaderMap` into `name -> [values]`, matching php-dev-server's
/// `$request->headers->all()` shape (multi-value headers become arrays).
/// Values are decoded as UTF-8 leniently rather than dropped when they are
/// not visible ASCII.
fn headers_to_json(headers: &HeaderMap) -> Value {
    let mut map = serde_json::Map::new();
    for name in headers.keys() {
        let values: Vec<Value> = headers
            .get_all(name)
            .iter()
            .map(|v| Value::String(String::from_utf8_lossy(v.as_bytes()).into_owned()))
            .collect();
        map.insert(name.as_str().to_string(), Value::Array(values));
    }
    Value::Object(map)
}

fn sha256_hex(bytes: &[u8]) -> String {
    Sha256::digest(bytes)
        .iter()
        .map(|b| format!("{b:02x}"))
        .collect()
}

/// Guess a file type from its leading bytes.
fn sniff(bytes: &[u8]) -> Option<&'static str> {
    if bytes.starts_with(b"\x89PNG\r\n\x1a\n") {
        Some("image/png")
    } else if bytes.starts_with(b"\xff\xd8\xff") {
        Some("image/jpeg")
    } else if bytes.starts_with(b"GIF87a") || bytes.starts_with(b"GIF89a") {
        Some("image/gif")
    } else if bytes.len() >= 12 && &bytes[..4] == b"RIFF" && &bytes[8..12] == b"WEBP" {
        Some("image/webp")
    } else if bytes.starts_with(b"%PDF-") {
        Some("application/pdf")
    } else {
        None
    }
}

// ---------------------------------------------------------------------------
// multipart/form-data

struct Part {
    name: Option<String>,
    filename: Option<String>,
    content_type: Option<String>,
    data: Bytes,
}

struct Parts {
    parts: Vec<Part>,
    error: Option<String>,
}

impl Parts {
    fn to_json(&self) -> Value {
        let parts: Vec<Value> = self
            .parts
            .iter()
            .map(|p| {
                let text = std::str::from_utf8(&p.data).ok();
                json!({
                    "name": p.name,
                    "filename": p.filename,
                    "content_type": p.content_type,
                    "len": p.data.len(),
                    "sha256": sha256_hex(&p.data),
                    "sniffed_type": sniff(&p.data),
                    "utf8": text.is_some(),
                    "text": text,
                })
            })
            .collect();
        json!({ "parts": parts, "error": self.error })
    }
}

async fn parse_multipart(content_type: &str, body: Bytes) -> Parts {
    let mut out = Parts {
        parts: Vec::new(),
        error: None,
    };
    let boundary = match multer::parse_boundary(content_type) {
        Ok(b) => b,
        Err(e) => {
            out.error = Some(e.to_string());
            return out;
        }
    };
    let stream = tokio_stream::once(Ok::<_, Infallible>(body));
    let mut multipart = multer::Multipart::new(stream, boundary);
    loop {
        match multipart.next_field().await {
            Ok(Some(field)) => {
                let name = field.name().map(str::to_owned);
                let filename = field.file_name().map(str::to_owned);
                let content_type = field.content_type().map(|m| m.to_string());
                match field.bytes().await {
                    Ok(data) => out.parts.push(Part {
                        name,
                        filename,
                        content_type,
                        data,
                    }),
                    Err(e) => {
                        out.error = Some(e.to_string());
                        break;
                    }
                }
            }
            Ok(None) => break,
            Err(e) => {
                out.error = Some(e.to_string());
                break;
            }
        }
    }
    out
}

// ---------------------------------------------------------------------------
// base64 diagnostics

/// Where `?b64=` points:
///
/// - empty or `body`: the whole request body.
/// - `/a/b`: a JSON Pointer (RFC 6901) into the JSON body, e.g. `/image`.
/// - `part:NAME`: the multipart part named `NAME`.
#[derive(Debug, PartialEq)]
enum B64Target {
    Body,
    Pointer(String),
    Part(String),
}

impl B64Target {
    fn parse(raw: &str) -> Self {
        if let Some(name) = raw.strip_prefix("part:") {
            B64Target::Part(name.to_string())
        } else if raw.starts_with('/') {
            B64Target::Pointer(raw.to_string())
        } else {
            B64Target::Body
        }
    }

    fn label(&self) -> String {
        match self {
            B64Target::Body => "body".to_string(),
            B64Target::Pointer(p) => p.clone(),
            B64Target::Part(n) => format!("part:{n}"),
        }
    }

    fn report(&self, body: &[u8], json: Option<&Value>, parts: Option<&Parts>) -> Value {
        let source = self.label();
        let fail = |error: String| json!({ "source": source, "error": error });
        let input: Vec<u8> = match self {
            B64Target::Body => body.to_vec(),
            B64Target::Pointer(p) => {
                let Some(doc) = json else {
                    return fail("body is not JSON (or the content type is not JSON)".into());
                };
                match doc.pointer(p) {
                    Some(Value::String(s)) => s.as_bytes().to_vec(),
                    Some(other) => {
                        return fail(format!(
                            "value at {p} is {}, not a string",
                            json_kind(other)
                        ));
                    }
                    None => return fail(format!("no value at {p}")),
                }
            }
            B64Target::Part(name) => {
                let Some(parts) = parts else {
                    return fail("body is not multipart/form-data".into());
                };
                match parts.parts.iter().find(|p| p.name.as_deref() == Some(name)) {
                    Some(part) => part.data.to_vec(),
                    None => return fail(format!("no part named {name:?}")),
                }
            }
        };
        let mut report = analyze_base64(&input);
        report["source"] = json!(source);
        report
    }
}

fn json_kind(v: &Value) -> &'static str {
    match v {
        Value::Null => "null",
        Value::Bool(_) => "a boolean",
        Value::Number(_) => "a number",
        Value::String(_) => "a string",
        Value::Array(_) => "an array",
        Value::Object(_) => "an object",
    }
}

fn is_b64_char(b: u8) -> bool {
    b.is_ascii_alphanumeric() || matches!(b, b'+' | b'/' | b'-' | b'_' | b'=')
}

/// Accepts either alphabet once normalized to standard, with or without
/// padding, and non-canonical trailing bits.
const LENIENT: GeneralPurpose = GeneralPurpose::new(
    &alphabet::STANDARD,
    GeneralPurposeConfig::new()
        .with_decode_padding_mode(DecodePaddingMode::Indifferent)
        .with_decode_allow_trailing_bits(true),
);

/// Report on how a client encoded `input` as base64, and decode it as
/// leniently as possible so the decoded bytes can be checked.
///
/// `strict_valid` says whether a strict RFC 4648 decoder (standard
/// alphabet, canonical padding, no whitespace, no `data:` prefix) accepts
/// the value as sent. `warnings` lists each deviation in plain words.
fn analyze_base64(input: &[u8]) -> Value {
    let mut warnings: Vec<String> = Vec::new();
    let strict_valid = STANDARD.decode(input).is_ok();

    // A data URI prefix: `data:[<mime>][;params][;base64],`.
    let mut offset = 0usize;
    let mut data_uri = Value::Null;
    let mut declared_mime: Option<String> = None;
    if input.starts_with(b"data:")
        && let Some(comma) = input.iter().position(|&b| b == b',')
    {
        let header = String::from_utf8_lossy(&input[5..comma]).into_owned();
        let mut fields = header.split(';');
        let mime = fields.next().unwrap_or("").to_string();
        let is_base64 = header.split(';').any(|f| f.eq_ignore_ascii_case("base64"));
        warnings.push(format!(
            "starts with a data URI prefix ({:?}); most APIs want the bare base64",
            String::from_utf8_lossy(&input[..=comma])
        ));
        if !is_base64 {
            warnings.push("data URI prefix lacks `;base64`".into());
        }
        if !mime.is_empty() {
            declared_mime = Some(mime.clone());
        }
        data_uri = json!({ "mime": mime, "base64_flag": is_base64, "prefix_len": comma + 1 });
        offset = comma + 1;
    }

    // Strip whitespace, remembering original offsets so errors point into
    // the input as sent.
    let mut chars: Vec<(usize, u8)> = Vec::with_capacity(input.len() - offset);
    let (mut spaces, mut newlines) = (0usize, 0usize);
    let mut max_line = 0usize;
    let mut line = 0usize;
    for (i, &b) in input.iter().enumerate().skip(offset) {
        match b {
            b'\n' => {
                newlines += 1;
                max_line = max_line.max(line);
                line = 0;
            }
            b' ' | b'\t' | b'\r' => spaces += 1,
            _ => {
                chars.push((i, b));
                line += 1;
            }
        }
    }
    max_line = max_line.max(line);
    if newlines > 0 {
        warnings.push(format!(
            "contains {newlines} line break(s) (longest line {max_line} chars); \
             MIME-style wrapping is rejected by strict decoders"
        ));
    }
    if spaces > 0 {
        warnings.push(format!("contains {spaces} space/tab/CR character(s)"));
    }

    let invalid = chars
        .iter()
        .find(|(_, b)| !is_b64_char(*b))
        .map(|&(i, b)| json!({ "offset": i, "byte": b, "char": String::from_utf8_lossy(&[b]) }));

    let has_standard = chars.iter().any(|(_, b)| matches!(b, b'+' | b'/'));
    let has_url_safe = chars.iter().any(|(_, b)| matches!(b, b'-' | b'_'));
    let alphabet = match (has_standard, has_url_safe) {
        (true, true) => "mixed",
        (true, false) => "standard",
        (false, true) => "url_safe",
        (false, false) => "either",
    };
    match alphabet {
        "url_safe" => {
            warnings.push("uses the URL-safe alphabet (`-` `_`), not standard (`+` `/`)".into())
        }
        "mixed" => {
            warnings.push("mixes standard (`+` `/`) and URL-safe (`-` `_`) characters".into())
        }
        _ => {}
    }

    // Padding: `=` only at the end, as many as the data length needs.
    let data_len = chars.iter().take_while(|(_, b)| *b != b'=').count();
    let pad = chars.len() - data_len;
    let pad_misplaced = chars[data_len..].iter().any(|(_, b)| *b != b'=');
    let expected_pad = (4 - data_len % 4) % 4;
    let truncated = data_len % 4 == 1;
    let padding = if pad_misplaced {
        "misplaced"
    } else if truncated {
        "invalid_length"
    } else if pad == expected_pad {
        "ok"
    } else if pad == 0 {
        "missing"
    } else if pad < expected_pad {
        "short"
    } else {
        "excess"
    };
    match padding {
        "misplaced" => warnings.push("`=` appears before the end".into()),
        "invalid_length" => warnings.push(format!(
            "{data_len} base64 characters leaves one dangling character; the value is likely truncated"
        )),
        "missing" => warnings.push(format!("missing {expected_pad} `=` padding character(s)")),
        "short" => warnings.push(format!("has {pad} `=` but needs {expected_pad}")),
        "excess" => warnings.push(format!("has {pad} `=` but needs {expected_pad}")),
        _ => {}
    }

    let mut report = json!({
        "input_len": input.len(),
        "strict_valid": strict_valid,
        "data_uri": data_uri,
        "whitespace": { "newlines": newlines, "other": spaces, "longest_line": max_line },
        "alphabet": alphabet,
        "padding": padding,
        "invalid_char": invalid,
    });

    if invalid.is_some() {
        warnings.push("contains a character outside both base64 alphabets".into());
    } else if !pad_misplaced {
        let normalized: Vec<u8> = chars[..data_len]
            .iter()
            .map(|&(_, b)| match b {
                b'-' => b'+',
                b'_' => b'/',
                b => b,
            })
            .collect();
        match LENIENT.decode(&normalized) {
            Ok(decoded) => {
                let sniffed = sniff(&decoded);
                if let (Some(declared), Some(sniffed)) = (&declared_mime, sniffed)
                    && !declared.eq_ignore_ascii_case(sniffed)
                {
                    warnings.push(format!(
                        "data URI says {declared} but the decoded bytes look like {sniffed}"
                    ));
                }
                let double = looks_like_base64(&decoded);
                if double {
                    warnings.push("decoded bytes are themselves valid base64 text; the value may have been encoded twice".into());
                }
                report["decoded"] = json!({
                    "len": decoded.len(),
                    "sha256": sha256_hex(&decoded),
                    "sniffed_type": sniffed,
                    "is_base64_again": double,
                });
            }
            Err(e) => report["decode_error"] = json!(e.to_string()),
        }
    }

    report["warnings"] = json!(warnings);
    report
}

/// True when `bytes` is non-trivial text that itself decodes as base64.
fn looks_like_base64(bytes: &[u8]) -> bool {
    let trimmed: Vec<u8> = bytes
        .iter()
        .copied()
        .filter(|b| !b.is_ascii_whitespace())
        .collect();
    trimmed.len() >= 8
        && trimmed.iter().all(|&b| is_b64_char(b))
        && (STANDARD.decode(&trimmed).is_ok()
            || LENIENT
                .decode(
                    trimmed
                        .iter()
                        .copied()
                        .filter(|&b| b != b'=')
                        .collect::<Vec<_>>(),
                )
                .is_ok())
}

#[cfg(test)]
mod tests {
    use super::*;

    const PNG: &[u8] = b"\x89PNG\r\n\x1a\n\x00\x00\x00\rIHDR";

    fn warnings(report: &Value) -> Vec<String> {
        report["warnings"]
            .as_array()
            .unwrap()
            .iter()
            .map(|w| w.as_str().unwrap().to_string())
            .collect()
    }

    #[test]
    fn clean_standard_base64_has_no_warnings() {
        let encoded = STANDARD.encode(PNG);
        let r = analyze_base64(encoded.as_bytes());
        assert_eq!(r["strict_valid"], json!(true));
        assert_eq!(r["padding"], json!("ok"));
        assert_eq!(r["decoded"]["len"], json!(PNG.len()));
        assert_eq!(r["decoded"]["sha256"], json!(sha256_hex(PNG)));
        assert_eq!(r["decoded"]["sniffed_type"], json!("image/png"));
        assert_eq!(r["decoded"]["is_base64_again"], json!(false));
        assert!(warnings(&r).is_empty(), "{r}");
    }

    #[test]
    fn data_uri_url_safe_unpadded_still_decodes() {
        // Bytes chosen so the encoding contains `+` and `/` in standard form.
        let raw: Vec<u8> = PNG.iter().copied().chain([0xfb, 0xff, 0xbf]).collect();
        let url_safe = base64::engine::general_purpose::URL_SAFE_NO_PAD.encode(&raw);
        assert!(url_safe.contains('-') || url_safe.contains('_'));
        let input = format!("data:image/jpeg;base64,{url_safe}");

        let r = analyze_base64(input.as_bytes());
        assert_eq!(r["strict_valid"], json!(false));
        assert_eq!(r["data_uri"]["mime"], json!("image/jpeg"));
        assert_eq!(r["alphabet"], json!("url_safe"));
        assert_eq!(r["padding"], json!("missing"));
        assert_eq!(r["decoded"]["sha256"], json!(sha256_hex(&raw)));
        let w = warnings(&r).join("\n");
        assert!(w.contains("data URI prefix"), "{w}");
        assert!(w.contains("URL-safe"), "{w}");
        assert!(w.contains("padding"), "{w}");
        assert!(w.contains("says image/jpeg but"), "{w}");
    }

    #[test]
    fn mime_wrapped_lines_are_reported() {
        let raw = vec![7u8; 200];
        let encoded = STANDARD.encode(&raw);
        let wrapped: String = encoded
            .as_bytes()
            .chunks(76)
            .map(|c| format!("{}\r\n", std::str::from_utf8(c).unwrap()))
            .collect();
        let r = analyze_base64(wrapped.as_bytes());
        assert_eq!(r["strict_valid"], json!(false));
        assert_eq!(r["whitespace"]["newlines"], json!(4));
        assert_eq!(r["whitespace"]["longest_line"], json!(76));
        assert_eq!(r["decoded"]["len"], json!(200));
    }

    #[test]
    fn invalid_char_offset_points_into_input() {
        let r = analyze_base64(b"data:,QUJD*EFG");
        assert_eq!(r["invalid_char"]["offset"], json!(10));
        assert_eq!(r["invalid_char"]["char"], json!("*"));
        assert!(r.get("decoded").is_none());
    }

    #[test]
    fn truncated_value_is_flagged() {
        let mut encoded = STANDARD.encode(PNG);
        encoded.truncate(encoded.trim_end_matches('=').len() / 4 * 4 + 1);
        let r = analyze_base64(encoded.as_bytes());
        assert_eq!(r["padding"], json!("invalid_length"));
        assert!(r.get("decode_error").is_some(), "{r}");
    }

    #[test]
    fn double_encoding_is_flagged() {
        let twice = STANDARD.encode(STANDARD.encode(PNG));
        let r = analyze_base64(twice.as_bytes());
        assert_eq!(r["decoded"]["is_base64_again"], json!(true));
        assert!(warnings(&r).join("\n").contains("encoded twice"));
    }

    #[test]
    fn excess_and_misplaced_padding() {
        assert_eq!(analyze_base64(b"QUJD==")["padding"], json!("excess"));
        assert_eq!(analyze_base64(b"QUI=Q===")["padding"], json!("misplaced"));
        assert_eq!(analyze_base64(b"QQ=")["padding"], json!("short"));
    }

    #[test]
    fn target_parsing() {
        assert_eq!(B64Target::parse(""), B64Target::Body);
        assert_eq!(B64Target::parse("body"), B64Target::Body);
        assert_eq!(
            B64Target::parse("/image"),
            B64Target::Pointer("/image".into())
        );
        assert_eq!(
            B64Target::parse("part:file"),
            B64Target::Part("file".into())
        );
    }

    #[test]
    fn sniffs_common_types() {
        assert_eq!(sniff(PNG), Some("image/png"));
        assert_eq!(sniff(b"\xff\xd8\xff\xe0"), Some("image/jpeg"));
        assert_eq!(sniff(b"GIF89a.."), Some("image/gif"));
        assert_eq!(sniff(b"RIFF\0\0\0\0WEBPVP8 "), Some("image/webp"));
        assert_eq!(sniff(b"hello"), None);
    }
}
