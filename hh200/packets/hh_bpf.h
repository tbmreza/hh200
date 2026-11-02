// C shim API for the hh200 eBPF collector. Hides libbpf behind an opaque
// handle so the Haskell FFI never touches libbpf types.
#ifndef HH_BPF_H
#define HH_BPF_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

struct hh_bpf;

// Open + load + attach the embedded eBPF program. Returns 0 on success and
// stores a handle in *out (owned by the caller; release with hh_bpf_close).
// On failure returns a negative errno and sets the message returned by
// hh_bpf_last_error().
int hh_bpf_open(struct hh_bpf **out);

// Drain the ring buffer, blocking up to timeout_ms. Accumulates the event
// count internally. Returns 0 on success (including a clean timeout), or a
// negative errno on error. If count is non-NULL it is set to the current
// accumulated count.
int hh_bpf_poll(struct hh_bpf *h, uint64_t *count, int timeout_ms);

// The current accumulated event count (e.g. retransmits observed so far).
uint64_t hh_bpf_count(const struct hh_bpf *h);

// Tear down and free the handle. Safe to call on NULL.
void hh_bpf_close(struct hh_bpf *h);

// Last error message (empty when the last call succeeded).
const char *hh_bpf_last_error(void);

#ifdef __cplusplus
}
#endif

#endif
