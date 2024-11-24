interpret:
	hh200 run examples/ir/hello.hhir

test:
	# check prints warnings, or hhir source to std out (indirectable)
	hh200 check examples/hello.hhs > out.hhir
	hh200 run out.hhir

debug:
	cabal build -j4
	dist-newstyle/build/x86_64-linux/ghc-9.4.7/hh200-0.1.0.0/x/hh200/build/hh200/hh200 --hello examples/hhsm/hello.hhsm --enthusiasm 3
