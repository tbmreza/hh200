## Contributing
The project is in ideation phase. `DRAFT.md` is where I dump my thoughts. `hh200/` works if you want to play with what I got so far.

## Features
The following defining features sum up hh200 in trade-off terms.

#### 1. Fail fast (compromising test percentage)
Well-functioning system-under-test is the only thing that should matter; we're dodging the need for skipping cases in test scripts.
Furthermore, such systems developer can only work on one bug at a time. We'd rather them start working on it sooner!

#### 2. Regex, random, time batteries (compromising binary size)
hh200 comes bundled with a full expression language BEL evaluator.
  
## See also

<details>
<summary>
hurl https://github.com/Orange-OpenSource/hurl
</summary>
Requests in "simple plain text format". You could invoke hurl HTTP client
binary from your favorite general purpose language to achieve, for example,
parallel test execution.
</details>

<details>
<summary>
httpie https://github.com/httpie/cli
</summary>
"Make CLI interaction with web services as human-friendly as possible".
httpie resonates with people who have worked with curl or wget and find
their flags and quote escapes unpleasant.
</details>

<details>
<summary>
grafana/k6 https://github.com/grafana/k6
</summary>
Load testing engine providing JavaScript programming interface. To fully
live the term "load testing" (say, 6-digit number of virtual users), it can
act a the runner in an orchestrated, distributed load testing grid to
generate the traffic.
</details>
