Highlight:
Architecturally decisive tasks include  happy-flow interpreter (post_json example); hh200 exe binary bundles hh200d (separate parsers), mcp etc remain satellites; bel complete pratt parser

# PRIMARY (OR INDUSTRY) SPEC SOURCES
## Syntax
- https://hh200-docs.pages.dev/#structure  https://hurl.dev/docs/request.html#structure
```
@.headers  $.headers  %.params.start
```
## Cookies
- https://www.rfc-editor.org/rfc/rfc6265.html "HTTP State Management Mechanism"
- https://curl.se/docs/http-cookies.html      "curl HTTP Cookies"


# Design decisions
## General
- "Connection: close" header sent by default; close the connection after the current request/response pair.

# DONE LIST
all protocols/specs about cookies
server vs remote ports? remote == client
httpie sends Connection: close header by default, curl doesn't
plan a self-maintaining fork of httpLbs: do sync using gh actions
Set-Cookie attribute enum, implementing Secure attr "Sends cookie only over HTTPS."
http(s) secure
    user story: distinct tls https (https://www.stackage.org/package/http-client-tls) and insecure http (https://hackage-content.haskell.org/package/http-client-0.7.19/docs/Network-HTTP-Client.html#g:4).
    in curl, tls connection may be determined by specified url, not headers
https + Secure cookie attr
`ls ~/.local/bin/` checks if `stack install` succeed.
auto multi line braced: gemini %x JSON hallucination, scanBalanced
incremental parsing is non-goal if not free or very cheap


# STASH

┌───────────┬──────────────────────────────────────┬─────────────────────────┐
│ Location  │ Cause                                │ Result                  │
├───────────┼──────────────────────────────────────┼─────────────────────────┤
│ buildFrom │ Http.parseRequest (Malformed URL)    │ Exception (IO)          │ checked: handled by alex/happy
│ h         │ Http.httpLbs (Connection/Timeout)    │ Just ci (Captured Left) │ checked: some handled on the spot, some returned
│ h         │ assertsAreOk (Logic/Status mismatch) │ Just ci (Logic Branch)  │ checked: exceptions if any will be falsified
│ h         │ BEL.render / evalCaptures            │ Exception (IO)          │ checked: never throws
└───────────┴──────────────────────────────────────┴─────────────────────────┘

  3. Static Analysis for Load Configuration
   * Description: Implement validation rules in Scanner.hs to ensure load profiles make sense before execution starts.
       * Checks: Ensure ramp-down doesn't go below zero, total duration fits within global timeouts, and syntax for profile chains is valid.
   * Labels: static-analyzer

  Phase 2: Execution Engine Refactor

Refactor `Execution.hs` to Worker Pool Model
Description: Deprecate the current mapConcurrentlyBounded approach. Implement a "Worker Pool" architecture where N virtual users (VUs) are spawned, but their execution loops
are controlled by a central "Pacer" or "Scheduler" that utilizes the Rate Limiter from Task 2.
   * Labels: load-testing-tool

Implement Dynamic Load Scheduler
Description: Connect the DSL profile (Task 1) to the Rate Limiter (Task 2). The scheduler needs to calculate the target RPS for the current millisecond (e.g., if ramping
from 0 to 100 over 10s, at t=5s the target is 50 RPS) and adjust the Rate Limiter's bucket refill rate dynamically.
   * Labels: load-testing-tool

  Phase 3: Monitoring & reporting

  6. Centralized Metrics Aggregator
   * Description: With many threads running, writing to stdout or a single SQLite file naively will cause contention. Create a high-performance, in-memory aggregator (using
     atomic counters or a bounded queue) to collect stats (latency histograms, error counts) from workers.
   * Labels: load-testing-tool

  Summary Table for GitHub


  ┌─────────┬────────────────────────────────────────────┬────────────────────────────────────┬────────────┐
  │ Task ID │ Title                                      │ Labels                             │ Dependency │
  ├─────────┼────────────────────────────────────────────┼────────────────────────────────────┼────────────┤
  │ 1       │ [DSL] Add Load Profile Grammar             │ static-analyzer, load-testing-tool │ -          │
  │ 2       │ [Core] Implement Token Bucket Rate Limiter │ load-testing-tool                  │ -          │
  │ 3       │ [Linter] Validate Load Profile Logic       │ static-analyzer                    │ Task 1     │
  │ 4       │ [Engine] Switch to Paced Worker Pool       │ load-testing-tool                  │ Task 2     │
  │ 5       │ [Engine] Dynamic Scheduler Implementation  │ load-testing-tool                  │ Task 1, 4  │
  │ 6       │ [Stats] In-Memory Metrics Aggregator       │ load-testing-tool                  │ Task 4     │
  │ 7       │ [CLI] Real-time Load Dashboard             │ load-testing-tool                  │ Task 5, 6  │
  └─────────┴────────────────────────────────────────────┴────────────────────────────────────┴────────────┘


oftenBodyless :: UppercaseString -> Bool  -- starting point for webdav or lints
oftenBodyless (UppercaseString s) = elem s ["GET", "HEAD", "OPTIONS", "TRACE"]


Debug traceStack and stack run --profile -- ../examples/asserts_block.hhs +RTS -xc
[ ScriptStart 1
, ItemStart ""
, HttpError "HttpExceptionRequest Request {\n  host = \"example.io\"
                                           \n  port = 443
                                           \n  secure = True
                                           \n  requestHeaders = [(\"authorization\",\"<REDACTED>\")]
                                           \n  path = \"/v2/target-template\"
                                           \n  queryString          = \"?project_id=24& year=2025&month=12&salesarea=5\"
                                           \n  method               = \"GET\"
                                           \n  proxy                = Nothing
                                           \n  rawBody              = False
                                           \n  redirectCount        = 10
                                           \n  responseTimeout      = ResponseTimeoutDefault
                                           \n  requestVersion       = HTTP/1.1
                                           \n  proxySecureMode      = ProxySecureWithConnect
                                           \n}
                                \n (InternalException
                                       (HandshakeFailed
                                           (Error_Protocol \"certificate rejected: [NameMismatch \\\"example.io\\\"]\"
                                            CertificateUnknown)))"
]

hh200-docs page for hh200d
POST http://localhost:9999/echo.php
{ "foo": "bar", "baz": 123 }
HTTP 200
[Asserts]
>debug $.key

Based on the "MAIN THESIS" comments in app/Main.hs and the structure I verified in src/Hh200/Cli.hs, here are the questions an academic reviewer might have, focused on the
  claims and methodology implied by your code:

   1. Methodological Novelty: "You describe a 3-step pipeline (Scanner $\rightarrow$ Execution $\rightarrow$ Presentation) as the core thesis. How does this architecture differ
      conceptually from existing HTTP client test runners? Is the innovation in the coupling of static analysis (Scanner) with runtime verification?" >It's HTTP testing expert opinions enforced via a test runner and an LSP.
   2. Formal Semantics: "The Scanner.analyze step is described as providing 'linter hints' and 'static checks'. Does the DSL have a formal specification or type system that
      guarantees certain properties before execution? What class of errors can be caught statically versus dynamically?" >We'll prove certain protocols/idioms disobedience are checkable statically.
   3. Correctness vs. Performance: "The thesis lists 'testRps' (requests per second) and 'testShotgun' (concurrency) alongside correctness checks ('status codes mismatch'). Is
      the primary contribution of hh200 a correctness verifier or a load testing tool? How do you balance the trade-off between the overhead of correctness assertions and the
      speed required for load generation?" >The trade-off is in the hands of test case designer. The more complex the single script is, the lower should we anticipate the maximum capacity is. We doubt there's immense reward in defining what's complex and benchmarking it, though.
   4. Deterministic "Reality": "The code mentions handling 'thread cancelled' and 'offline' states under the 'Reality' section. How does your execution model ensure
      reproducibility when dealing with these non-deterministic network conditions? Do you simulate these states or rely on real-world flakiness?" >We extend the pens of test case designers: they want real-world results, they get real-world results.
   5. Compliance Standards: "You mention checking for 'HTTP idioms' and 'WebDAV status code misuse'. Which specific RFCs or standards are you strictly adhering to? Are these
      checks opinionated/heuristic or strictly standards-compliant?" >We'll show how the standards map to our tests.

  These questions target the gap between what the code does (runs tests) and why it is structured this way (the "Thesis").
   * Figure 1: `hh200` System Architecture Overview (Illustrating the 3-step pipeline: Scanner, Execution, Presentation)
   * Figure 2: Static Analysis Workflow for HTTP Idioms (Detailing the process of Scanner.analyze)
   * Figure 3: Example of Detected HTTP Protocol Violation (Linter Hint)
   * Figure 4: Request Latency Distribution under Concurrent Load (Derived from testShotgun results)
   * Figure 5: Throughput (RPS) vs. Number of Concurrent Workers (Derived from testShotgun or testRps results)
   * Figure 6: HTTP Status Code Mismatch Detection Example
   * Figure 7: Time-series Plot of Test Metrics (e.g., Latency, Errors) over Duration (Output from Graph.plot or similar)


what to do about: Special-Use Domain Names, webdav semantics,

## Syntax ideas

- evaluate expr with {{ }} like hurl
- unless replaces previous expression with arms
- scoped variables; let stmt that isn't followed by indentation is global
- for loop with let-until-step
- first class callables
- hurl doesn't have syntax for output file, only cli option https://github.com/Orange-OpenSource/hurl/discussions/2078
- statement is a tree that can have unevaled expressions in its leaf

POST https://httpbin.org/anything?page=2&lim={{LIMIT}} write out.png
{ "username": "admin", "id": 8 } unless
    isStaging -> { "username": "admin", "id": 79 }
    isProd    -> { "username": "admin", "id": 100 }

let httpBadRequest = 400
    { "statusCode": {{httpBadRequest}} }

let i = 0
    POST https://httpbin.org/anything?page=2&lim=10
    { "key": {{i}} }
until 4 step 1


let url = https://httpbin.org/anything?page=2&lim=10
let method = GET

callable login =
    { "body": 12 }

{{login}}


callable f =
    POST http://localhost
    { "payload": 12 }
    return response.data.token

let token = {{f}}

callable g = copy f with
    body = {}
    method = GET

mut f with
    method = PATCH

[Asserts]
> jsonpath "$.json.kkk" == "vvv"

POST https://host.com/v2/login
GET http://localhost:9999/echo.php
[Captures]
RELAY: TOKEN


POST https://host.com/login.php
{ "username": "testing1", "password": "paragon" }
HTTP 200
[Captures]
NNN: 9
TOKEN: jsonpath "$.jwt"

POST  https://host.com/sfa2/web/index.php?r=delivery-order%2Fcreate-order
jwt: eyabcd98
HTTP 203


-- matches("1234", "[0-9-]+")    "1234".matches("[0-9-]+")
-- size("") == 0
-- timestamp("") - timestamp("") = duration("")
-- randInt(lo, hi)
-- today() == dateIsoDate() dateIsoDateTime() dateIsoDateTimeZone() == dateIso()



[Captures]
BT = `I am {2 + 2} years old`
ID = $.data.id
START = today()
Q_STR = "salt and pepper"
[Asserts]
> 12 != 120
> false




POST http://localhost:9999/echo.php
{ "data": "ck" }
HTTP 200
[Captures]
Q_STR = "salt and pepper"
ID = $.data.id
START =   today()
[Asserts]
> 12 == 12  # early return
# jsonpath "$.data.name" == "alice"
# implicit status == 200 assertion at the end

POST http://localhost:9999/echo.php
{ "data": "ck" }
HTTP 200
[Captures]
Q_STR = "salt and pepper"
ID = $.data.id
START =   today()
[Asserts]
> 12 == 12  # early return
> jsonpath "$.data.method" == request method
# jsonpath "$.data.name" == "alice"
# implicit status == 200 assertion at the end

# goal on $ @ %
use  jsonpath "$." for response body as it's the norm
then jsonpath "%." can be used for all other response fields
requests are statically checked, so it make sense in this block that request is treated "natively"


POST http://localhost:9999/{{START}}&q={{Q_STR}}
{ "data": "a" }
HTTP 404

POST  http://localhost:9999/echo.php
{ "kkk": "vvv" }
HTTP 200
[Captures]
NNN: 9
[Asserts]
> true




opus:
2. Metrics gathering — forward-looking plan
Here's where runScriptM vs runProcM matters. Currently worker calls runScriptM which returns IO () and discards everything. For metrics, the workers need to produce data. Here's a layered plan:

Layer 1: Per-worker result channel (immediate next step)
Introduce a TChan WorkerResult (or TQueue) that workers write to after each script execution:

haskell
data WorkerResult = WorkerResult
  { wrWorkerId    :: Int
  , wrSuccess     :: Bool           -- Nothing from runProcM means success
  , wrLog         :: Log            -- TraceEvent list
  , wrElapsedUs   :: Int            -- microseconds for this execution
  }
The worker would call runProcM (instead of runScriptM), time it with getCurrentTime / diffUTCTime, then writeTChan results (WorkerResult ...).

Layer 2: Aggregator thread
A dedicated thread drains the TChan WorkerResult and maintains running aggregates in a TVar:

haskell
data Metrics = Metrics
  { mTotalRequests :: !Int
  , mSuccesses     :: !Int
  , mFailures      :: !Int
  , mTotalDuration :: !Double       -- seconds
  , mMinLatency    :: !Double
  , mMaxLatency    :: !Double
  -- , mLatencies  :: [Double]      -- for percentile calculation
  }
Layer 3: Reporting
When the main thread exits (after forM_ doneSignals takeMVar), it reads the final TVar Metrics and prints a summary:

── testSimple report ──────────────────────
  Workers:       2
  Duration:      10.0s
  Total reqs:    47
  Successes:     45  (95.7%)
  Failures:      2   (4.3%)
  Avg latency:   212ms
  Min latency:   98ms
  Max latency:   1204ms
───────────────────────────────────────────
This is the same pattern load testing tools like k6/vegeta use.


Metrics Plan (next iteration)
The -- ??: building on worker doneSignals, design how to report results/metrics comment is addressed by this design:

Layer	    What                                                                	                                                Where
Data type	WorkerResult { wrSuccess :: Bool, wrLog :: Log, wrElapsedUs :: Int }	                                                New in TokenBucketWorkerPool or Types
Producer	Worker calls runProcM (not runScriptM), times it, writes WorkerResult to a shared TChan                             	worker in TokenBucketWorkerPool.hs
Aggregator	A forked thread drains the TChan, maintains running Metrics (total, success/fail counts, min/max/avg latency) in a TVar	testSimple in Cli.hs
Reporter	After forM_ doneSignals takeMVar, read the final TVar Metrics and print a summary	                                    testSimple in Cli.hs

The key prerequisite is switching worker from runScriptM (returns ()) to runProcM (returns (Maybe CallItem, Env, Log)) so that per-execution results are available.
