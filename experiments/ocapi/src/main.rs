//! Ocapi is a controllable, open/closed-model workload aware, HTTP system-under-test binary. Use
//! `ocapi-core` lib to analyze workload traffics without spinning Ocapi's builtin HTTP server.
mod builtin_server;
mod control;
mod logger;
mod routes;
mod tui;
mod tui_logger;

use std::io::IsTerminal;
use std::path::PathBuf;
use std::sync::Arc;

use tokio::main;
use clap::{Parser, Subcommand};
use etcetera::BaseStrategy;

use ocapi_core::{classify_events, events_to_inter_arrivals, parse_dump};

use crate::logger::FileLogger;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// HTTP system-under-test.
    Serve {
        #[arg(short, long)]
        dump_path: Option<PathBuf>,
        #[arg(short, long, default_value_t = 8080)]
        port: u16,
        /// Run the live traffic TUI instead of the plain file logger.
        #[arg(long)]
        tui: bool,
    },

    /// Classify traffic dump at path.
    Analyze {
        #[arg(short, long)]
        dump_path: Option<PathBuf>,
    },

    /// Check installation.
    Doctor,
}

#[main]
async fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::Serve {
            dump_path,
            port,
            tui,
        } => {
            // A TUI owns the terminal, so only run it on a real TTY. Tracing
            // moves to a file in that case; otherwise it stays on stdout.
            let tui = tui && std::io::stdout().is_terminal();
            let _guard = init_tracing(tui);

            let dump_path = dump_path.unwrap_or_else(default_dump_path);
            if let Some(parent) = dump_path.parent() {
                std::fs::create_dir_all(parent)
                    .unwrap_or_else(|e| panic!("failed to create dump dir {parent:?}: {e}"));
            }

            if tui {
                let (logger, tui::TuiRuntime { handle, shutdown }) = tui::build(&dump_path);
                builtin_server::serve(port, logger, Some(shutdown)).await;
                let _ = handle.join();
            } else {
                let logger = Arc::new(FileLogger::new(&dump_path));
                builtin_server::serve(port, logger, None).await;
            }
        }
        Commands::Analyze { dump_path } => {
            init_tracing(false);
            analyze(dump_path.unwrap_or_else(default_dump_path));
        }
        Commands::Doctor => {
            init_tracing(false);
            doctor();
        }
    }
}

/// Route `tracing` output: to stdout by default, or to a fixed file under the
/// XDG state directory while the TUI owns the terminal. The file is opened in
/// append mode, which is what makes an external `logrotate` `copytruncate`
/// rotation safe (O_APPEND re-seeks to EOF after truncation).
fn init_tracing(to_file: bool) -> Option<tracing_appender::non_blocking::WorkerGuard> {
    if !to_file {
        tracing_subscriber::fmt::init();
        return None;
    }

    let dir = state_dir();
    std::fs::create_dir_all(&dir)
        .unwrap_or_else(|e| panic!("failed to create state dir {dir:?}: {e}"));

    let appender = tracing_appender::rolling::never(&dir, "ocapi.log");
    let (non_blocking, guard) = tracing_appender::non_blocking(appender);
    tracing_subscriber::fmt().with_writer(non_blocking).init();
    Some(guard)
}

fn analyze(dump_path: PathBuf) {
    let text = std::fs::read_to_string(&dump_path)
        .unwrap_or_else(|e| panic!("failed to read dump {dump_path:?}: {e}"));

    let events = parse_dump(&text)
        .unwrap_or_else(|e| panic!("failed to parse dump {dump_path:?}: {e:?}"));

    let verdict = classify_events(&events);
    println!("Verdict={:?}", verdict);

    let (_, gaps) = events_to_inter_arrivals(&events);

    let completed = events.iter().filter(|e| e.completion.is_some()).count();
    let dangling = events.len() - completed;

    println!("events:     {}", events.len());
    println!("completed:  {completed}");
    println!("dangling:   {dangling}");

    if gaps.is_empty() {
        println!("inter-arrival gaps: none");
        return;
    }

    let sum_ns: u128 = gaps.iter().map(|g| g.as_nanos()).sum();
    let min = gaps.iter().min().unwrap();
    let max = gaps.iter().max().unwrap();
    let mean = std::time::Duration::from_nanos((sum_ns / gaps.len() as u128) as u64);

    println!("gaps:       {}", gaps.len());
    println!("gap min:    {min:?}");
    println!("gap max:    {max:?}");
    println!("gap mean:   {mean:?}");
}

fn doctor() {
    println!("config path: {:?}", config_path());
    println!(
        "config exists: {}",
        config_path().exists()
    );

    let dump_path = default_dump_path();
    println!("dump path: {:?}", dump_path);

    let writable = dump_path.parent().is_some_and(|parent| {
        if std::fs::create_dir_all(parent).is_err() {
            return false;
        }
        let probe = parent.join(".ocapi-write-probe");
        let ok = std::fs::write(&probe, b"").is_ok();
        let _ = std::fs::remove_file(&probe);
        ok
    });

    println!("dump dir writable: {writable}");
}

/// Directory holding ocapi's per-user data (traffic dumps), per XDG.
fn data_dir() -> PathBuf {
    let dirs = etcetera::base_strategy::choose_base_strategy()
        .expect("failed to determine platform base directories");

    dirs.data_dir().join("ocapi")
}

/// Directory holding ocapi's per-user state (the TUI tracing log), per XDG.
fn state_dir() -> PathBuf {
    let dirs = etcetera::base_strategy::choose_base_strategy()
        .expect("failed to determine platform base directories");

    dirs.state_dir()
        .expect("failed to determine state directory")
        .join("ocapi")
}

fn default_dump_path() -> PathBuf {
    data_dir().join("traffic.dump")
}

fn config_path() -> PathBuf {
    if cfg!(debug_assertions) {
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("config.toml")
    } else {
        let dirs = etcetera::base_strategy::choose_base_strategy()
            .expect("failed to determine platform base directories");

        dirs.config_dir()
            .join("ocapi")
            .join("config.toml")
    }
}

#[test]
fn config_path_uses_expected_location() {
    let path = config_path();

    // unless --release, read from cargo root.
    if cfg!(debug_assertions) {
        assert!(
            path.starts_with(env!("CARGO_MANIFEST_DIR")),
            "debug build should read from repository: {path:?}"
        );
    } else {
        let dirs = etcetera::base_strategy::choose_base_strategy().unwrap();

        assert!(
            path.starts_with(dirs.config_dir()),
            "release build should read from XDG config directory: {path:?}"
        );
    }
}
