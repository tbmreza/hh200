use std::time::Duration;


pub enum Verdict {
    Open,
    Close,
    Unknown,
}

// ??: document what we mean by "inter-arrivals" of http traffic
// fn classify(inter_arrivals) -> Verdict
