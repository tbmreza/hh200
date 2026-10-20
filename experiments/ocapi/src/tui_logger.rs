//! The TUI traffic logger: mirrors the `traffic.dump` to disk (so offline
//! `analyze` keeps working) while also forwarding each event to the live TUI.

use std::path::Path;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;

use async_trait::async_trait;
use tokio::sync::mpsc;

use crate::logger::{now_nanos, FileLogger, TrafficLogger};
use crate::tui::TuiEvent;

pub struct TuiLogger {
    /// Keeps the on-disk dump identical to the file-backed logger.
    file: FileLogger,
    tx: mpsc::Sender<TuiEvent>,
    /// Bumped when the display channel is full and an event is dropped. The
    /// dump on disk is unaffected; only the live monitor misses the event.
    dropped: Arc<AtomicU64>,
}

impl TuiLogger {
    pub fn new(path: &Path, tx: mpsc::Sender<TuiEvent>, dropped: Arc<AtomicU64>) -> Self {
        Self {
            file: FileLogger::new(path),
            tx,
            dropped,
        }
    }

    fn forward(&self, event: TuiEvent) {
        if self.tx.try_send(event).is_err() {
            self.dropped.fetch_add(1, Ordering::Relaxed);
        }
    }
}

#[async_trait]
impl TrafficLogger for TuiLogger {
    async fn log_arrival(&self) -> u64 {
        let id = self.file.log_arrival().await;
        // A second clock read only for the monitor; the on-disk timestamp is
        // the authoritative one. Sub-microsecond skew is irrelevant here.
        self.forward(TuiEvent::Arrival { id, ts: now_nanos() });
        id
    }

    async fn log_completion(&self, id: u64) {
        self.file.log_completion(id).await;
        self.forward(TuiEvent::Completion { id, ts: now_nanos() });
    }
}
