use std::fs::OpenOptions;
use std::io::Write;
use std::path::Path;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::{SystemTime, UNIX_EPOCH};

use async_trait::async_trait;
use tokio::sync::Mutex;

/// Abstraction over traffic-logging backends. The middleware only ever sees
/// this trait, so the file-backed logger and the TUI logger are
/// interchangeable through `AppState`.
#[async_trait]
pub trait TrafficLogger: Send + Sync {
    /// Writes an arrival record and returns the `EventId` it assigned. The
    /// caller must pass this id back to `log_completion` once the response is
    /// sent.
    async fn log_arrival(&self) -> u64;
    async fn log_completion(&self, id: u64);
}

/// Nanoseconds since the Unix epoch, as a `u64` (fits until the year ~2554).
pub fn now_nanos() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("clock before unix epoch")
        .as_nanos() as u64
}

/// Append-only `traffic.dump` logger. Emits the `A <id> <ns>` / `C <id> <ns>`
/// line grammar that `ocapi-core::parse_dump` reads.
pub struct FileLogger {
    file: Arc<Mutex<std::fs::File>>,
    next_id: AtomicU64,
}

impl FileLogger {
    pub fn new(path: &Path) -> Self {
        let file = OpenOptions::new()
            .create(true)
            .append(true)
            .open(path)
            .expect("Failed to open traffic log file");
        Self {
            file: Arc::new(Mutex::new(file)),
            next_id: AtomicU64::new(0),
        }
    }
}

#[async_trait]
impl TrafficLogger for FileLogger {
    async fn log_arrival(&self) -> u64 {
        let id = self.next_id.fetch_add(1, Ordering::Relaxed);
        let log = format!("A {} {}\n", id, now_nanos());
        let mut file = self.file.lock().await;
        let _ = file.write_all(log.as_bytes());
        let _ = file.flush();
        id
    }

    async fn log_completion(&self, id: u64) {
        let log = format!("C {} {}\n", id, now_nanos());
        let mut file = self.file.lock().await;
        let _ = file.write_all(log.as_bytes());
        let _ = file.flush();
    }
}
