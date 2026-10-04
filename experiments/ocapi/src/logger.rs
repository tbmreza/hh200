use std::fs::OpenOptions;
use std::io::Write;
use std::path::Path;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::{SystemTime, UNIX_EPOCH};
use tokio::sync::Mutex;

pub struct TrafficLogger {
    file: Arc<Mutex<std::fs::File>>,
    next_id: AtomicU64,
}

impl TrafficLogger {
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

    /// Writes an arrival line and returns the EventId it assigned. The caller
    /// must pass this id back to `log_completion` once the response is sent.
    pub async fn log_arrival(&self) -> u64 {
        let id = self.next_id.fetch_add(1, Ordering::Relaxed);
        let now = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_nanos();
        let log = format!("A {} {}\n", id, now);
        let mut file = self.file.lock().await;
        let _ = file.write_all(log.as_bytes());
        let _ = file.flush();
        id
    }

    pub async fn log_completion(&self, id: u64) {
        let now = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_nanos();
        let log = format!("C {} {}\n", id, now);
        let mut file = self.file.lock().await;
        let _ = file.write_all(log.as_bytes());
        let _ = file.flush();
    }
}
