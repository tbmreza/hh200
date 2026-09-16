## Alternatif Judul
1. Reproducible open- and closed model declaration evaluation in Grafana k6 and Python Locust on a controllable HTTP system-under-test
2. A Controllable System-Under-Test for Evaluating Open- and Closed-Model HTTP Load Generation

## Konteks

HTTP API testers must continually reason about whether their load generator's traffic model (open or closed). 
Regardless of whether system-under-test is behaving as expected or not, the traffic model might invalidate their testing results in the most subtle of manners.

When a single tester's traffic is the only traffic on the system-under-test, this burden is at least tractable: resolving the open/closed question is a matter of loosening 
one person's cognitive load, i.e. brainpower for such judgment calls can be invested somewhere else as productive.
But when traffic is not isolated to one testing session (e.g. other clients, background jobs, or production traffic share the system), accounting for the open/closed distinction becomes impractical, 
even given a load generator that is a correct implementation of its intended model.

No amount of client-side correctness can recover a distinction that depends on the aggregate behavior of traffic sources.
Total client-side correctness might be needed to survive aggregate behavior of traffic sources, but such thing is not found in real-world systems.

We propose Ocapi, a controllable, HTTP server system-under-test that **diagnoses open- vs. closed-model traffic directly from observed inter-arrival timing**.
Ocapi takes inter-arrival timeseries data [and kernel-level retransmission statistics] as inputs to characterize realistic systems exhibiting network jitter and packet loss.
Ocapi is, to our knowledge, the first system-under-test designed explicitly to make load-generator traffic models empirically falsifiable.


## List of tables

1. Taxonomy of traffic models (open, closed, bounded-open, adaptive-concurrency, retry-storm)
1. Experiment matrix summary: traffic model, generator tools, network condition
1. Confusion matrix (classifier verdict vs. ground-truth label)
1. Real-world trace characteristics (source, duration, request count, assumed traffic model)


## List of figures

1. System architecture diagram of Ocapi (ConnectionRegistry, RequestTimestamper, ProtocolAdapter, ArrivalClassifier pipeline)
1. Inter-arrival histograms, side-by-side, for open vs. closed vs. bounded-open traffic
1. Ambiguity spectrum: accuracy/verdict distribution as concurrency limit shifts
1. Cross-generator consistency: same traffic model, verdict distribution across Locust and k6
1. Real-world trace result (timeline over the replayed production/public trace)

## Prototype
Module 1: Control (jitter, latency) on [github.com/tokio-rs/axum](https://github.com/tokio-rs/axum) application

Module 2: Classifier (decides whether observed traffic is open/closed/unknown)

## Literature Review

#### Industrial HTTP Load Generators
- https://github.com/grafana/k6
- https://github.com/locustio/locust

#### Workload Design Principles
1. Use open model for public traffic; closed for bounded populations.
1. Correct for Coordinated Omission using HdrHistogram when using closed model

#### Prior work (2026) fundamental limitation
