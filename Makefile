reg:
	cabal test
	cabal run -j4 hh200 -- --version

test:
	hh200 compile --io examples/hello.hhs [--policies specs.aimfor.toml]        # prints warnings to std err, or test on the outside world (dynamically-checked programming style) [override policies]
	hh200 compile examples/index.hhs [--policies specs.aimfor.toml] > out.hhsm  # prints warnings to std err [override policies], or hhsm source to std out (indirectable)
	hh200 interactive examples/hhsm/hello.hhsm                                  # asks confirmation before every side-effects

locust:
	# 901. ...905 (unacceptable perf) ...900 (acceptable) return 905 (less than aim)
	# the number at which system under test falls off acceptable performance
	aimfor [--config config.aimfor.toml] -n 1000 --specs specs.aimfor.toml smoke.hhs  # hh200 build examples/hello.hhs --policies specs.aimfor.toml  where build depends on parallelization strat

	# properties of interest: "System under test can handle N parallel users."
	# "Number of users is *the only factor* of any under-achievements." is a non-property as it's impossible to say about any unpure systems.
	#
	# To that goal, we increase a stride at a time our number of parallel users until we get unacceptable* perf.
	#
	# config.aimfor.toml: Mechanisms that shouldn't affect our property check.
	# stride = 0  # handled as {n div 10} by runtime
	# all_stats = 2  # "open|forget|quiet"
	#
	# *specs.aimfor.toml: worst acceptable execution stat of serving 1 user defined in numbers:
	# max_reruns = 2
	# max_retries_per_call = 2
	# time_millis_per_call = 60_000
	# # example: index.hhs contains C1 and C2 (where C2 is contrived to always fail) calls. Sequentially, C1 succeed and C2 failed the first time.
	# # Then, C2 is retried {max_retries_per_call} times.
	# # Then, the whole index.hhs is run {max_reruns} more times.

help:
	cabal build -j4
	dist-newstyle/build/x86_64-linux/ghc-9.8.2/hh200-0.1.0.0/x/hh200/build/hh200/hh200 --help

download:
	cabal run -j4 hh200 -- --hello examples/debug.hhs download https://httpbin.org/latest.apk

run:
	cabal run -j4 hh200 -- examples/debug.hhs

# # dist-newstyle/build/x86_64-linux/ghc-9.4.7/...
# dist-newstyle/build/x86_64-linux/ghc-9.8.2/hh200-0.1.0.0/x/hh200/build/hh200/hh200 --hello examples/debug.hhs
