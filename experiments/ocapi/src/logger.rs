use std::fs::OpenOptions;
use std::io::Write;
use std::path::Path;
use std::sync::Arc;
use tokio::sync::Mutex;
use uuid::Uuid;
use std::time::{SystemTime, UNIX_EPOCH};

pub struct TrafficLogger {
    file: Arc<Mutex<std::fs::File>>,
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
        }
    }

    pub async fn log_arrival(&self, id: Uuid) {
        let now = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_nanos();
        let log = format!("A {} {}\n", id, now);
        let mut file = self.file.lock().await;
        let _ = file.write_all(log.as_bytes());
        let _ = file.flush();
    }

    pub async fn log_completion(&self, id: Uuid) {
        let now = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_nanos();
        let log = format!("C {} {}\n", id, now);
        let mut file = self.file.lock().await;
        let _ = file.write_all(log.as_bytes());
        let _ = file.flush();
    }
}
