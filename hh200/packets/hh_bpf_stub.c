// Stub shim used when the `ebpf` build flag is off (the default). Defines the
// same symbols so the Haskell FFI always links; every call reports that eBPF
// support was not compiled in.
#include <errno.h>
#include <stdint.h>

#include "hh_bpf.h"

static char g_last_error[256] = "eBPF support not compiled in";

struct hh_bpf {
    int unused;
};

int hh_bpf_open(struct hh_bpf **out)
{
    *out = NULL;
    return -ENOTSUP;
}

int hh_bpf_poll(struct hh_bpf *h, uint64_t *count, int timeout_ms)
{
    (void)h;
    (void)timeout_ms;
    if (count)
        *count = 0;
    return -ENOTSUP;
}

uint64_t hh_bpf_count(const struct hh_bpf *h)
{
    (void)h;
    return 0;
}

void hh_bpf_close(struct hh_bpf *h)
{
    (void)h;
}

const char *hh_bpf_last_error(void)
{
    return g_last_error;
}
