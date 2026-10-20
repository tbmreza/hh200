//! The live traffic TUI: owns the terminal, renders on a fixed tick, and
//! hands the server a graceful-shutdown signal when the user quits.

use std::collections::VecDeque;
use std::io;
use std::path::Path;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};

use crossterm::{
    cursor,
    event::{self, Event, KeyCode, KeyEventKind, KeyModifiers},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use ratatui::{
    backend::CrosstermBackend,
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Cell, Paragraph, Row, Sparkline, Table},
    Frame, Terminal,
};
use tokio::sync::{mpsc, oneshot};

use ocapi_core::{Kind, Verdict, VerdictEngine};

use crate::logger::now_nanos;
use crate::tui_logger::TuiLogger;

const TICK_MS: u64 = 250;
const CHANNEL_CAPACITY: usize = 8192;
const LOG_CAP: usize = 500;
const GAP_WINDOW: usize = 96; // sparkline points
const TABLE_ROWS: usize = 16;
const RATE_WINDOW_SECS: f64 = 5.0;
const RATE_WINDOW_NS: u64 = (RATE_WINDOW_SECS * 1e9) as u64;

/// What the logger sends to the render thread.
#[derive(Debug, Clone, Copy)]
pub enum TuiEvent {
    Arrival { id: u64, ts: u64 },
    Completion { id: u64, ts: u64 },
}

/// Handle to the running TUI thread, plus a signal that resolves when the user
/// quits (or the thread dies), for axum's graceful shutdown.
pub struct TuiRuntime {
    pub handle: std::thread::JoinHandle<()>,
    pub shutdown: oneshot::Receiver<()>,
}

/// Build a TUI logger (mirrors the dump to disk, forwards events to the TUI)
/// and spawn the render thread. Returns the logger for `AppState` and the
/// runtime handle for `serve`.
pub fn build(dump_path: &Path) -> (Arc<TuiLogger>, TuiRuntime) {
    let (tx, rx) = mpsc::channel(CHANNEL_CAPACITY);
    let dropped = Arc::new(AtomicU64::new(0));
    let logger = Arc::new(TuiLogger::new(dump_path, tx, dropped.clone()));
    let runtime = spawn(rx, dropped);
    (logger, runtime)
}

fn spawn(rx: mpsc::Receiver<TuiEvent>, dropped: Arc<AtomicU64>) -> TuiRuntime {
    install_panic_hook();
    let (shutdown_tx, shutdown_rx) = oneshot::channel();
    let handle = std::thread::Builder::new()
        .name("ocapi-tui".into())
        .spawn(move || {
            // If the terminal can't be taken over, drop the shutdown sender so
            // the server still stops instead of hanging.
            let _ = run(rx, dropped, shutdown_tx);
        })
        .expect("failed to spawn TUI thread");
    TuiRuntime {
        handle,
        shutdown: shutdown_rx,
    }
}

/// Restore the terminal on any unwind path out of `run`.
struct TerminalGuard;

impl Drop for TerminalGuard {
    fn drop(&mut self) {
        let _ = disable_raw_mode();
        let mut out = io::stdout();
        let _ = execute!(out, LeaveAlternateScreen, cursor::Show);
    }
}

/// Best-effort terminal restore if some other thread panics while the TUI owns
/// the screen. Crossterm's raw/alternate-screen state is process-global, so
/// this is safe from any thread.
fn install_panic_hook() {
    let default = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        let _ = disable_raw_mode();
        let mut out = io::stdout();
        let _ = execute!(out, LeaveAlternateScreen, cursor::Show);
        default(info);
    }));
}

fn run(
    mut rx: mpsc::Receiver<TuiEvent>,
    dropped: Arc<AtomicU64>,
    shutdown: oneshot::Sender<()>,
) -> io::Result<()> {
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, cursor::Hide)?;
    let _guard = TerminalGuard;

    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    let mut state = TuiState::new();
    let tick = Duration::from_millis(TICK_MS);
    let mut next_draw = Instant::now();

    loop {
        // 1. ingest, decoupled from rendering.
        while let Ok(ev) = rx.try_recv() {
            state.ingest(ev);
        }

        // 2. input: block until the next draw tick, or a key arrives.
        let wait = next_draw.saturating_duration_since(Instant::now());
        if event::poll(wait)? {
            if let Event::Key(key) = event::read()? {
                if key.kind == KeyEventKind::Press {
                    let quit = match key.code {
                        KeyCode::Char('q') => true,
                        KeyCode::Char('c') if key.modifiers.contains(KeyModifiers::CONTROL) => true,
                        KeyCode::Char('p') => {
                            state.paused = !state.paused;
                            false
                        }
                        KeyCode::Char('c') => {
                            state.log.clear();
                            false
                        }
                        _ => false,
                    };
                    if quit {
                        break;
                    }
                }
            }
        }

        // 3. render on a fixed tick (skipped while paused, ingest still runs).
        if Instant::now() >= next_draw {
            state.refresh(&dropped);
            if !state.paused {
                terminal.draw(|f| draw(f, &state))?;
            }
            next_draw += tick;
            if next_draw <= Instant::now() {
                next_draw = Instant::now() + tick;
            }
        }
    }

    // Tell axum to shut down, then restore the terminal via the guard.
    let _ = shutdown.send(());
    Ok(())
}

// ─────────────────────────── state ─────────────────────────────────────────

struct TuiState {
    engine: VerdictEngine,
    verdict: Verdict,
    last_change: Option<Instant>,
    arrival_times: VecDeque<u64>, // epoch ns, sliding window for rate
    gaps: VecDeque<u64>,          // inter-arrival gaps in ns
    last_arrival: Option<u64>,
    log: VecDeque<String>,
    start: Instant,
    rate: f64,
    dropped: u64,
    paused: bool,
}

impl TuiState {
    fn new() -> Self {
        Self {
            engine: VerdictEngine::new(),
            verdict: Verdict::undetermined(),
            last_change: None,
            arrival_times: VecDeque::new(),
            gaps: VecDeque::new(),
            last_arrival: None,
            log: VecDeque::new(),
            start: Instant::now(),
            rate: 0.0,
            dropped: 0,
            paused: false,
        }
    }

    fn ingest(&mut self, ev: TuiEvent) {
        match ev {
            TuiEvent::Arrival { id, ts } => {
                self.engine.record_arrival(id, ts);
                if let Some(prev) = self.last_arrival {
                    self.gaps.push_back(ts.saturating_sub(prev));
                    if self.gaps.len() > GAP_WINDOW {
                        self.gaps.pop_front();
                    }
                }
                self.last_arrival = Some(ts);
                self.arrival_times.push_back(ts);
                self.push_log(format!("A id={id}"));
            }
            TuiEvent::Completion { id, ts } => {
                self.engine.record_completion(id, ts);
                self.push_log(format!("C id={id}"));
            }
        }
    }

    fn refresh(&mut self, dropped: &AtomicU64) {
        self.dropped = dropped.load(Ordering::Relaxed);

        let now = now_nanos();
        let cutoff = now.saturating_sub(RATE_WINDOW_NS);
        while self.arrival_times.front().is_some_and(|&t| t < cutoff) {
            self.arrival_times.pop_front();
        }
        self.rate = self.arrival_times.len() as f64 / RATE_WINDOW_SECS;

        let verdict = self.engine.classify();
        if verdict.kind != self.verdict.kind {
            self.push_log(format!(
                "verdict {} -> {}",
                kind_label(self.verdict.kind),
                kind_label(verdict.kind)
            ));
            self.verdict = verdict;
            self.last_change = Some(Instant::now());
        } else {
            // keep the freshest basis even when the kind is unchanged.
            self.verdict = verdict;
        }
    }

    fn push_log(&mut self, msg: String) {
        let stamp = format!("{:>8.3}", self.start.elapsed().as_secs_f64());
        self.log.push_back(format!("{stamp}  {msg}"));
        while self.log.len() > LOG_CAP {
            self.log.pop_front();
        }
    }
}

// ─────────────────────────── rendering ─────────────────────────────────────

fn draw(f: &mut Frame, state: &TuiState) {
    let area = f.area();
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Length(4), Constraint::Min(1)])
        .split(area);

    draw_verdict(f, chunks[0], state);

    let lower = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Percentage(45), Constraint::Percentage(55)])
        .split(chunks[1]);

    let mid = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Percentage(50), Constraint::Percentage(50)])
        .split(lower[0]);

    draw_table(f, mid[0], state);
    draw_sparkline(f, mid[1], state);
    draw_log(f, lower[1], state);
}

fn draw_verdict(f: &mut Frame, area: Rect, state: &TuiState) {
    let (glyph, label, color) = kind_display(state.verdict.kind);
    let since = state
        .last_change
        .map(|i| format!("{:.1}s", i.elapsed().as_secs_f64()))
        .unwrap_or_else(|| "—".to_string());

    let line1 = Line::from(vec![
        Span::raw("verdict "),
        Span::styled(
            format!("{glyph} {label}"),
            Style::default().fg(color).add_modifier(Modifier::BOLD),
        ),
        Span::raw(format!("   samples {}", state.engine.sample_count())),
    ]);
    let line2 = Line::from(vec![
        Span::raw(format!(
            "rate {:.1}/s   changed {} ago   dropped {}",
            state.rate, since, state.dropped
        )),
        if state.paused {
            Span::styled("   [PAUSED]", Style::default().fg(Color::Red).add_modifier(Modifier::BOLD))
        } else {
            Span::raw("")
        },
    ]);

    let para = Paragraph::new(vec![line1, line2])
        .block(Block::default().borders(Borders::ALL).title(" ocapi live "));
    f.render_widget(para, area);
}

fn draw_table(f: &mut Frame, area: Rect, state: &TuiState) {
    let header = Row::new(vec!["id", "verdict", "samples", "last seen"])
        .style(Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD));

    let now = now_nanos();
    let rows: Vec<Row> = state
        .engine
        .recent(TABLE_ROWS)
        .iter()
        .map(|(id, event)| {
            // One request per id (no session identity): a lone sample can't be
            // classified, so the per-connection verdict is honest "?".
            let (_, label, color) = kind_display(Kind::Undetermined);
            let seen = event.completion.unwrap_or(event.arrival);
            Row::new(vec![
                Cell::from(id.to_string()),
                Cell::from(label).style(Style::default().fg(color)),
                Cell::from("1"),
                Cell::from(fmt_ago(now.saturating_sub(seen))),
            ])
        })
        .collect();

    let table = Table::new(rows, [
        Constraint::Length(6),
        Constraint::Length(12),
        Constraint::Length(8),
        Constraint::Min(10),
    ])
    .header(header)
    .block(Block::default().borders(Borders::ALL).title(" connections "));
    f.render_widget(table, area);
}

fn draw_sparkline(f: &mut Frame, area: Rect, state: &TuiState) {
    let data: Vec<u64> = state.gaps.iter().copied().collect();
    let mean_ms = if data.is_empty() {
        None
    } else {
        let sum: u128 = data.iter().map(|&g| g as u128).sum();
        Some((sum / data.len() as u128 / 1_000_000) as u64)
    };

    let block = Block::default()
        .borders(Borders::ALL)
        .title(format!(
            " inter-arrival ({}) ",
            mean_ms.map(|m| format!("mean {m}ms")).unwrap_or_else(|| "no gaps".into())
        ));

    if data.is_empty() {
        f.render_widget(Paragraph::new("no inter-arrival gaps yet").block(block), area);
        return;
    }

    let sparkline = Sparkline::default()
        .block(block)
        .data(&data)
        .style(Style::default().fg(Color::Cyan));
    f.render_widget(sparkline, area);
}

fn draw_log(f: &mut Frame, area: Rect, state: &TuiState) {
    let text: Vec<Line> = state
        .log
        .iter()
        .map(|l| Line::raw(l.clone()))
        .collect();

    // Auto-scroll to the tail: skip everything above the visible window.
    let content_height = area.height.saturating_sub(2) as usize;
    let offset = state.log.len().saturating_sub(content_height) as u16;

    let para = Paragraph::new(text)
        .block(Block::default().borders(Borders::ALL).title(" log (q quit · p pause · c clear) "))
        .scroll((offset, 0));
    f.render_widget(para, area);
}

// ─────────────────────────── helpers ───────────────────────────────────────

/// Distinct glyph AND label per verdict, so color is never the only signal.
fn kind_display(kind: Kind) -> (&'static str, &'static str, Color) {
    match kind {
        Kind::Open => ("◉", "open", Color::Magenta),
        Kind::Closed => ("◯", "closed", Color::Green),
        Kind::Undetermined => ("?", "undetermined", Color::Yellow),
        Kind::Conflict => ("⚠", "conflict", Color::Red),
    }
}

fn kind_label(kind: Kind) -> &'static str {
    kind_display(kind).1
}

fn fmt_ago(ns: u64) -> String {
    if ns < 1_000_000 {
        format!("{}μs", ns / 1_000)
    } else if ns < 1_000_000_000 {
        format!("{}ms", ns / 1_000_000)
    } else {
        format!("{:.1}s", ns as f64 / 1e9)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// 10 rounds of a single closed client: arrive, service 10ms, re-arrive
    /// 1ms after completion. The `InterArrival` strategy reads this as closed.
    fn ingest_closed_loop(state: &mut TuiState, rounds: u64) {
        let base = now_nanos(); // realistic epoch base so the rate window keeps them
        for id in 0..rounds {
            let arrive = base + id * 11_000_000;
            state.ingest(TuiEvent::Arrival { id, ts: arrive });
            state.ingest(TuiEvent::Completion {
                id,
                ts: arrive + 10_000_000,
            });
        }
    }

    #[test]
    fn refresh_detects_closed_verdict_and_logs_transition() {
        let mut state = TuiState::new();
        let dropped = AtomicU64::new(0);

        ingest_closed_loop(&mut state, 10);
        state.refresh(&dropped);

        assert_eq!(state.verdict.kind, Kind::Closed);
        assert_eq!(state.engine.sample_count(), 10);
        assert!(state.last_change.is_some());
        assert!(
            state.log.iter().any(|l| l.contains("undetermined -> closed")),
            "expected a verdict-transition line, got: {:?}",
            state.log
        );
    }

    #[test]
    fn ingest_builds_inter_arrival_gaps_and_rate_window() {
        let mut state = TuiState::new();
        let dropped = AtomicU64::new(0);

        ingest_closed_loop(&mut state, 10);
        state.refresh(&dropped);

        // 10 arrivals -> 9 gaps, all 11ms apart.
        assert_eq!(state.gaps.len(), 9);
        assert!(state.gaps.iter().all(|&g| g == 11_000_000));
        assert_eq!(state.arrival_times.len(), 10);
        assert!(state.rate > 0.0);
    }

    #[test]
    fn pause_and_clear_are_reflected_in_state() {
        let mut state = TuiState::new();
        let dropped = AtomicU64::new(0);

        ingest_closed_loop(&mut state, 5);
        state.refresh(&dropped);
        assert!(!state.log.is_empty());

        state.paused = true;
        assert!(state.paused);
        state.log.clear();
        assert!(state.log.is_empty());
    }
}
