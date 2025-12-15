# PRIMARY (OR INDUSTRY) SPEC SOURCES
## Syntax
- https://hurl.dev/docs/request.html#structure
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
    in curl, tls connection is determined by specified url, not headers (unverified chatgpt)
https + Secure cookie attr


# STASH
          | request response             { trace ("call_itemC: " ++ show $2) pCallItem defaultDepsClause $1 (Just $2) }
[Asserts]
> debug $.data
Based on the "MAIN THESIS" comments in app/Main.hs and the structure I verified in src/Hh200/Cli.hs, here are the questions an academic reviewer might have, focused on the
  claims and methodology implied by your code:

   1. Methodological Novelty: "You describe a 3-step pipeline (Scanner $\rightarrow$ Execution $\rightarrow$ Presentation) as the core thesis. How does this architecture differ
      conceptually from existing HTTP client test runners? Is the innovation in the coupling of static analysis (Scanner) with runtime verification?"
   2. Formal Semantics: "The Scanner.analyze step is described as providing 'linter hints' and 'static checks'. Does the DSL have a formal specification or type system that
      guarantees certain properties before execution? What class of errors can be caught statically versus dynamically?"
   3. Correctness vs. Performance: "The thesis lists 'testRps' (requests per second) and 'testShotgun' (concurrency) alongside correctness checks ('status codes mismatch'). Is
      the primary contribution of hh200 a correctness verifier or a load testing tool? How do you balance the trade-off between the overhead of correctness assertions and the
      speed required for load generation?"
   4. Deterministic "Reality": "The code mentions handling 'thread cancelled' and 'offline' states under the 'Reality' section. How does your execution model ensure
      reproducibility when dealing with these non-deterministic network conditions? Do you simulate these states or rely on real-world flakiness?"
   5. Compliance Standards: "You mention checking for 'HTTP idioms' and 'WebDAV status code misuse'. Which specific RFCs or standards are you strictly adhering to? Are these
      checks opinionated/heuristic or strictly standards-compliant?"

  These questions target the gap between what the code does (runs tests) and why it is structured this way (the "Thesis").
   * Figure 1: `hh200` System Architecture Overview (Illustrating the 3-step pipeline: Scanner, Execution, Presentation)
   * Figure 2: Static Analysis Workflow for HTTP Idioms (Detailing the process of Scanner.analyze)
   * Figure 3: Example of Detected HTTP Protocol Violation (Linter Hint)
   * Figure 4: Request Latency Distribution under Concurrent Load (Derived from testShotgun results)
   * Figure 5: Throughput (RPS) vs. Number of Concurrent Workers (Derived from testShotgun or testRps results)
   * Figure 6: HTTP Status Code Mismatch Detection Example
   * Figure 7: Time-series Plot of Test Metrics (e.g., Latency, Errors) over Duration (Output from Graph.plot or similar)


  import qualified Network.HTTP.Client as Prim
  ( newManager, parseRequest, httpLbs, method, requestBody, requestHeaders, responseStatus, responseBody
  , Manager, Response, Request, RequestBody(..)
  , closeManager
  )

-- -- thread-based parallelism we name shotgun: based on async + QSemN
-- Thread-based parallelism based on async and QSemN.
-- Unlike testOutsideWorld, testShotgun is aware of what the (usually two) variables are.
-- Almost like testOutsideWorld can step in a running testShotgun and contribute one number.
-- testOutsideWorld (pre alpha) appends Pct to output/history.dat

-- -- counter-example presentation based on mvar
-- -- in the normal testOutsideWorld-present combo
-- -- Script { callItems = HM.HashMap name [CallItem] }
--
-- -- duration-bound virtual user we name rps (in reference to locust RPS)
-- testOutsideWorld & testShotgun grow hand in hand, while testRps
-- starts a web js server.
-- inserts to sqlite db.

    -- let envNew :: Env = HM.fromList
    --         [ ("yyyymmdd", Aeson.String "19700101")
    --         , ("undefined", Aeson.String "falsy")
    --         ]
`ls ~/.local/bin/` checks if `stack install` succeed.
simple but useful actual script
    release date of host's linux kernel version  uname -r
    GET https://kernel.org/releases.json
    [Captures]
    KERNEL_VER: "6.6"
    # Capture the object that contains the string KERNEL_VER
    [Asserts]
    > (jsonpath "$.data.timestamp") has KERNEL_VER  # has or contains ??depends on CEL or hurl
    > eyeball KERNEL_VER (jsonpath "$.data.timestamp") "date"
    # eyeball needle haystack dfs_else_bubble
thread-based parallelism we name shotgun: based on async + QSemN
    plotter: data point (N, %)
counter-example presentation based on mvar
duration-bound virtual user we name rps (in reference to locust RPS)
whether DataPoint should generalize for all modes of Execution with pro cons
after above three, decide if doing 1 mill parallel requests is already in sight


                    -- Asserts.
                    -- nice to have bel lines statically checked regardless of the outside
                    -- world but full evaluation of bel requires outside world results.
                    --
                    -- a line doesn't mutate the Env.
                    --
                    -- world's and spec's status matching

    the mainline branch of the linux kernel last released on (isodate), the date when "event in past <year>"

https://en.wikipedia.org/w/rest.php/v1/search/page?q=earth&limit=1
https://en.wikipedia.org/wiki/November_5
GET http://localhost:8080/get
    {
    "releases": [
        {
            "moniker": "mainline",
            "version": "6.18-rc5",
            "iseol": false,
            "released": {
                "timestamp": 1762729819,
                "isodate": "2025-11-09"
            },
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

POST https://bivi-backend.pti-cosmetics.com/v2/login
GET http://localhost:9999/echo.php
[Captures]
RELAY: TOKEN


POST https://sfa-api-testing.pti-cosmetics.com/login.php
{ "username": "testing1", "password": "paragon" }
HTTP 200
[Captures]
NNN: 9
TOKEN: jsonpath "$.jwt"

POST  https://sfa.pti-cosmetics.com/sfa2/web/index.php?r=delivery-order%2Fcreate-order
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
# jsonpath "$.data.name" == "alice"
# implicit status == 200 assertion at the end

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
