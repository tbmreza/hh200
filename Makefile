interpret:
	hh200 run examples/ir/hello.hhir

test:
	hh200 check examples/hello.hhs --dump-hhir > out
	hh200 run out
