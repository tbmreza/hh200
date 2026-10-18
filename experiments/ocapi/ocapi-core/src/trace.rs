//! A trace: a time-ordered sequence of events.

// ───────────────────────── 7. trace ──────────────────────────────────────

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Ev {
    Arrive,
    Complete,
}

/// (time in ms, event), sorted by time.
#[derive(Debug)]
pub struct Trace(pub Vec<(u64, Ev)>);

impl Trace {
    pub(crate) fn times(&self, which: Ev) -> Vec<u64> {
        self.0.iter().filter(|(_, e)| *e == which).map(|(t, _)| *t).collect()
    }
    pub fn peak_outstanding(&self) -> u32 {
        let (mut cur, mut peak) = (0i64, 0i64);
        for &(_, e) in &self.0 {
            cur += if e == Ev::Arrive { 1 } else { -1 };
            peak = peak.max(cur);
        }
        peak as u32
    }
}
