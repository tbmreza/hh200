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

Total client-side correctness might be needed to survive aggregate behavior of traffic sources, but such correctness is simply not found in real-world systems.

We propose Ocapi, a controllable, HTTP server system-under-test that **diagnoses open- vs. closed-model traffic directly from observed inter-arrival timing**.
Ocapi takes inter-arrival timeseries data [and kernel-level retransmission statistics] as inputs to characterize realistic systems exhibiting network jitter and packet loss.
Ocapi is, to our knowledge, the first system-under-test designed explicitly to make load-generator traffic models empirically falsifiable.


## List of tables

1. Taxonomy of traffic models (open, closed, bounded-open, adaptive-concurrency, retry-storm)
2. Experiment matrix summary: traffic model, generator tools, network condition
3. Confusion matrix (classifier verdict vs. ground-truth label)
4. Real-world trace characteristics (source, duration, request count, assumed traffic model)


## List of figures

1. System architecture diagram of Ocapi (ConnectionRegistry, RequestTimestamper, ProtocolAdapter, ArrivalClassifier pipeline)
2. Inter-arrival histograms, side-by-side, for open vs. closed vs. bounded-open traffic
3. Ambiguity spectrum: accuracy/verdict distribution as concurrency limit shifts
4. Cross-generator consistency: same traffic model, verdict distribution across Locust and k6
5. Real-world trace result (timeline over the replayed production/public trace)

## Prototype
Module 1: Control (jitter, latency) on [github.com/tokio-rs/axum](https://github.com/tokio-rs/axum) application

Module 2: Classifier (decides whether observed traffic is open/closed/unknown)

## Literature Review

#### Industrial HTTP Load Generators
- https://github.com/grafana/k6
- https://github.com/locustio/locust

#### Workload Design Principles
1. Use open model for public traffic; closed for bounded populations.
2. Closed model induced Coordinated Omission can be corrected using HdrHistogram

#### Prior work (2026) limitations
"Closed-loop and distributed load testing of web applications using Kubernetes" [(2026)] was motivated by load-generator resource limits of single machine, hence they
engineered a Kubernetes cluster of machines.
This is a real motivation, but only adds urgency to the research question of ours: how can we evaluate whether a workload model is well-characterized or not, in a way that
isn't obtrusive to how a load generator was engineered?

Furthermore, the Kubernetes cluster is generator-centric (distributes generators -> collect metrics -> adjust traffic).
The technique is what you reach for when you need _HTTP client side auto-scaling_, but frankly I don't know a scenario where it's useful.
You are fundamentally further away from understanding "what load" you actually applied, only knowing whether or not "the requests-per-second met the KPI."

[(2026)]: https://www.researchgate.net/publication/413621368_Closed-loop_and_distributed_load_testing_of_web_applications_using_Kubernetes
