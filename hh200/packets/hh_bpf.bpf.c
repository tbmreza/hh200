// SPDX-License-Identifier: GPL-2.0
// hh200 eBPF network monitor — minimal slice.
//
// Counts TCP retransmits (tracepoint/tcp/tcp_retransmit_skb) and pushes a
// small event into a ring buffer for the userspace collector.
#include <linux/bpf.h>
#include <bpf/bpf_helpers.h>

struct {
    __uint(type, BPF_MAP_TYPE_RINGBUF);
    __uint(max_entries, 256 * 1024);
} events SEC(".maps");

// One event per observed retransmit.
struct event {
    __u64 ts;
    __u32 pid;
};

SEC("tracepoint/tcp/tcp_retransmit_skb")
int on_retransmit(void *ctx)
{
    struct event *e = bpf_ringbuf_reserve(&events, sizeof(struct event), 0);
    if (!e)
        return 0;
    e->ts = bpf_ktime_get_ns();
    e->pid = (__u32)(bpf_get_current_pid_tgid() >> 32);
    bpf_ringbuf_submit(e, 0);
    return 0;
}

char LICENSE[] SEC("license") = "GPL";
