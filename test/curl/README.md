# Vendored curl tests

The selected files in `data/` are copied from curl tag `curl-8_23_0` and are
parsed by `Hh200.CurlTest`. `COPYING` is the upstream curl license notice.
Refresh the corpus with:

```sh
./scripts/sync-curl-tests.sh
```

Only HTTP request bytes and response parsing are in scope. The skip list is
deliberately explicit so unsupported curl CLI, protocol, TLS, authentication,
proxy, cookie, and libcurl-only tests remain auditable.
