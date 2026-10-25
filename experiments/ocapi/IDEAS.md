# Ideas preserved from `php-dev-server`

`php-dev-server` (a Laravel 13 "system-under-test under our control") was
removed from the repository. Before deleting it, its *controllable endpoint
surface* was mined for ideas worth folding into ocapi. This file is that
record, so the "why" survives the delete.

## The mapping

| php-dev-server | Idea | ocapi disposition |
| --- | --- | --- |
| `ANY /api/echo` (`EchoController`) | Reflect method/uri/headers/query/body | **Ported** → `/echo` (`routes::echo`) |
| `GET /api/sse` (`SseController`) | Controllable SSE stream (`events`, `interval`) | **Ported** → `/sse` (`routes::sse`) |
| `LogRequestToConsole` + `XlsUploadController` 422 dump | Dump full request on failure | **Ported** → `warn!` in `traffic_logger_middleware` on non-2xx |
| `/delay/{delay}` (capped), `/fixed` | Configurable / fixed latency | already present (`routes::delay`, `routes::fixed`) |
| `/status/{code}` (comma list) | Random status from a list | already present (`routes::status`) |
| `DELETE /some-response-body/{toggle}/{status}` | Arbitrary status + body-present toggle | **Dropped** — `/status` covers the code, `/echo`+`/bytes` cover the body |
| `POST /api/xls`, `/register` (multipart + validation) | Controlled 422/400 upload failures | **Dropped** — ocapi is stateless; persistence layer not ported |
| `PROPFIND` / `MKCOL` (WebDAV) | 207 multi-status, controlled 404/405/409 | **Dropped** — niche, irrelevant to open/closed classification |
| `POST /api/login` + JWT | Auth flow | **Dropped** — out of scope for a traffic SUT |
| `ForceJsonResponse` middleware | Force `Accept: application/json` | **Dropped** — axum returns JSON by design |

## Porting notes

- **`control.rs`** is an empty `mod control;` stub. It looks like the original
  intended home for controllable-SUT behavior, but the existing controllable
  routes all live in `routes.rs`, so the new handlers were added there for
  consistency rather than activating the stub.
- **New dependency:** `tokio-stream` (feature `time`) was added to build the
  SSE stream (`ReceiverStream`). This is the only dependency the ports needed.
- **Dropped `time` field:** `SseController` stamped each event with an ISO-8601
  `time`; ocapi omits it to avoid pulling in `chrono`. The `id`/`message` and
  `done`/`total` shapes are kept.
- **Body dump on failure is header/uri only.** The middleware does not buffer
  the request body (consuming it would change request semantics). Full body
  reflection is available on demand via `/echo`.

## Deliberately not ported

The Laravel framework scaffolding (config, migrations, models, seeders, blade
views, vite, composer/npm manifests) and the file-storage layer
(`XlsFile`/`xls_files`) were **not** carried over — ocapi is a stateless Rust
SUT and these were framework artifacts, not ideas.
