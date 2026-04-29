# libbpf-bootstrap: demo BPF applications

[![Github Actions](https://github.com/libbpf/libbpf-bootstrap/actions/workflows/build.yml/badge.svg)](https://github.com/libbpf/libbpf-bootstrap/actions/workflows/build.yml)
[![Github Actions](https://github.com/libbpf/libbpf-bootstrap/actions/workflows/build-android.yml/badge.svg)](https://github.com/libbpf/libbpf-bootstrap/actions/workflows/build-android.yml)

Make sure you have cloned this repo using `--recurse-submodules` and installed the dependencies 
before you try to build the examples.  See [Building](#Building) for details.

## minimal

`minimal` is just that – a minimal practical BPF application example. It
doesn't use or require BPF CO-RE, so should run on quite old kernels. It
installs a tracepoint handler which is triggered once every second. It uses
`bpf_printk()` BPF helper to communicate with the world. To see it's output,
read `/sys/kernel/debug/tracing/trace_pipe` file as a root:

```shell
$ cd examples/c
$ make minimal
$ sudo ./minimal
$ sudo cat /sys/kernel/debug/tracing/trace_pipe
           <...>-3840345 [010] d... 3220701.101143: bpf_trace_printk: BPF triggered from PID 3840345.
           <...>-3840345 [010] d... 3220702.101265: bpf_trace_printk: BPF triggered from PID 3840345.
```

`minimal` is great as a bare-bones experimental playground to quickly try out
new ideas or BPF features.

## minimal_ns

`minimal_ns` is as same as `minimal` but for namespaced environments.
`minimal` would not work in environments that have namespace, like containers,
or WSL2, because the perceived pid of the process in the namespace is not the
actual pid of the process. For executing `minimal` in namespaced environments
you need to use `minimal_ns` instead.

```shell
$ cd examples/c
$ make minimal_ns
$ sudo ./minimal_ns
$ sudo cat /sys/kernel/debug/tracing/trace_pipe
           <...>-3840345 [022] d...1  8804.331204: bpf_trace_printk: BPF triggered from PID 9087.
           <...>-3840345 [022] d...1  8804.331215: bpf_trace_printk: BPF triggered from PID 9087.
```

## minimal_Legacy

This version of `minimal` is modified to allow running on even older kernels
that do not allow global variables. bpf_printk uses global variables unless
BPF_NO_GLOBAL_DATA is defined before including bpf_helpers.h. Additionally,
the global variable my_pid has been replaced with an array of one element to
hold the process pid.

```
$ cd examples/c
$ make minimal_legacy
$ sudo ./minimal_legacy
$ sudo cat /sys/kernel/debug/tracing/trace_pipe
  minimal_legacy-52030 [001] .... 491227.784078: 0x00000001: BPF triggered from PID 52030.
  minimal_legacy-52030 [001] .... 491228.840571: 0x00000001: BPF triggered from PID 52030.
  minimal_legacy-52030 [001] .... 491229.841643: 0x00000001: BPF triggered from PID 52030.
  minimal_legacy-52030 [001] .... 491230.842432: 0x00000001: BPF triggered from PID 52030.
```

## bootstrap

`bootstrap` is an example of a simple (but realistic) BPF application. It
tracks process starts (`exec()` family of syscalls, to be precise) and exits
and emits data about filename, PID and parent PID, as well as exit status and
duration of the process life. With `-d <min-duration-ms>` you can specify
minimum duration of the process to log. In such mode process start
(technically, `exec()`) events are not output (see example output below).

`bootstrap` was created in the similar spirit as
[libbpf-tools](https://github.com/iovisor/bcc/tree/master/libbpf-tools) from
BCC package, but is designed to be more stand-alone and with simpler Makefile
to simplify adoption to user's particular needs. It demonstrates the use of
typical BPF features:
  - cooperating BPF programs (tracepoint handlers for process `exec` and `exit`
    events, in this particular case);
  - BPF map for maintaining the state;
  - BPF ring buffer for sending data to user-space;
  - global variables for application behavior parameterization.
  - it utilizes BPF CO-RE and vmlinux.h to read extra process information from
    kernel's `struct task_struct`.

`bootstrap` is intended to be the starting point for your own BPF application,
with things like BPF CO-RE and vmlinux.h, consuming BPF ring buffer data,
command line arguments parsing, graceful Ctrl-C handling, etc. all taken care
of for you, which are crucial but mundane tasks that are no fun, but necessary
to be able to do anything useful. Just copy/paste and do simple renaming to get
yourself started.

Here's an example output in minimum process duration mode:

```shell
$ sudo ./bootstrap -d 50
TIME     EVENT COMM             PID     PPID    FILENAME/EXIT CODE
19:18:32 EXIT  timeout          3817109 402466  [0] (126ms)
19:18:32 EXIT  sudo             3817117 3817111 [0] (259ms)
19:18:32 EXIT  timeout          3817110 402466  [0] (264ms)
19:18:33 EXIT  python3.7        3817083 1       [0] (1026ms)
19:18:38 EXIT  python3          3817429 3817424 [1] (60ms)
19:18:38 EXIT  sh               3817424 3817420 [0] (79ms)
19:18:38 EXIT  timeout          3817420 402466  [0] (80ms)
19:18:43 EXIT  timeout          3817610 402466  [0] (70ms)
19:18:43 EXIT  grep             3817619 3817617 [1] (271ms)
19:18:43 EXIT  timeout          3817609 402466  [0] (321ms)
19:18:44 EXIT  iostat           3817585 3817531 [0] (3006ms)
19:18:44 EXIT  tee              3817587 3817531 [0] (3005ms)
...
```
## tc

`tc` (short for Traffic Control) is an example of handling ingress network traffics.
It creates a qdisc on the `lo` interface and attaches the `tc_ingress` BPF program to it.
It reports the metadata of the IP packets that coming into the `lo` interface.

```shell
$ sudo ./tc
...
Successfully started! Please run `sudo cat /sys/kernel/debug/tracing/trace_pipe` to see output of the BPF program.
......
```

The `tc` output in `/sys/kernel/debug/tracing/trace_pipe` should look
something like this:

```
$ sudo cat /sys/kernel/debug/tracing/trace_pipe
            node-1254811 [007] ..s1 8737831.671074: 0: Got IP packet: tot_len: 79, ttl: 64
            sshd-1254728 [006] ..s1 8737831.674334: 0: Got IP packet: tot_len: 79, ttl: 64
            sshd-1254728 [006] ..s1 8737831.674349: 0: Got IP packet: tot_len: 72, ttl: 64
            node-1254811 [007] ..s1 8737831.674550: 0: Got IP packet: tot_len: 71, ttl: 64
```


# Building

libbpf-bootstrap supports multiple build systems that do the same thing.
This serves as a cross reference for folks coming from different backgrounds.

## Install Dependencies

You will need `clang` (at least v11 or later), `libelf` and `zlib` to build
the examples, package names may vary across distros.

On Ubuntu/Debian, you need:
```shell
$ apt install clang libelf1 libelf-dev zlib1g-dev
```

On CentOS/Fedora, you need:
```shell
$ dnf install clang elfutils-libelf elfutils-libelf-devel zlib-devel
```
## Getting the source code

Download the git repository and check out submodules:
```shell
$ git clone --recurse-submodules https://github.com/libbpf/libbpf-bootstrap
```

## C Examples

Makefile build:

```shell
$ git submodule update --init --recursive       # check out libbpf
$ cd examples/c
$ make
$ sudo ./bootstrap
TIME     EVENT COMM             PID     PPID    FILENAME/EXIT CODE
00:21:22 EXIT  python3.8        4032353 4032352 [0] (123ms)
00:21:22 EXEC  mkdir            4032379 4032337 /usr/bin/mkdir
00:21:22 EXIT  mkdir            4032379 4032337 [0] (1ms)
00:21:22 EXEC  basename         4032382 4032381 /usr/bin/basename
00:21:22 EXIT  basename         4032382 4032381 [0] (0ms)
00:21:22 EXEC  sh               4032381 4032380 /bin/sh
00:21:22 EXEC  dirname          4032384 4032381 /usr/bin/dirname
00:21:22 EXIT  dirname          4032384 4032381 [0] (1ms)
00:21:22 EXEC  readlink         4032387 4032386 /usr/bin/readlink
^C
```

CMake build:

```shell
$ git submodule update --init --recursive       # check out libbpf
$ mkdir build && cd build
$ cmake ../examples/c
$ make
$ sudo ./bootstrap
<...>
```

XMake build (Linux):

```shell
$ git submodule update --init --recursive       # check out libbpf
$ cd examples/c
$ xmake
$ xmake run bootstrap
```

XMake build (Android):

```shell
$ git submodule update --init --recursive       # check out libbpf
$ cd examples/c
$ xmake f -p android
$ xmake
```

Install [Xmake](https://github.com/xmake-io/xmake)

```shell
$ bash <(wget https://xmake.io/shget.text -O -)
$ source ~/.xmake/profile
```

# Troubleshooting

Libbpf debug logs are quire helpful to pinpoint the exact source of problems,
so it's usually a good idea to look at them before starting to debug or
posting question online.

`./minimal` is always running with libbpf debug logs turned on.

For `./bootstrap`, run it in verbose mode (`-v`) to see libbpf debug logs:

```shell
$ sudo ./bootstrap -v
libbpf: loading object 'bootstrap_bpf' from buffer
libbpf: elf: section(2) tp/sched/sched_process_exec, size 384, link 0, flags 6, type=1
libbpf: sec 'tp/sched/sched_process_exec': found program 'handle_exec' at insn offset 0 (0 bytes), code size 48 insns (384 bytes)
libbpf: elf: section(3) tp/sched/sched_process_exit, size 432, link 0, flags 6, type=1
libbpf: sec 'tp/sched/sched_process_exit': found program 'handle_exit' at insn offset 0 (0 bytes), code size 54 insns (432 bytes)
libbpf: elf: section(4) license, size 13, link 0, flags 3, type=1
libbpf: license of bootstrap_bpf is Dual BSD/GPL
...
```
