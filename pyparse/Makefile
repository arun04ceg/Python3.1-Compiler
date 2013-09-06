all: pyparse
	@echo "compiled"

lex:
	@pylex

pyparse: pyparse.rkt python-ast.grm.sx 
	raco exe pyparse.rkt

run: pyparse derivative-parsers.rkt
	@pylex | ./pyparse

parse: pyparse derivative-parsers.rkt
	@./pyparse

clean:
	rm -rf compiled pyparse sdiff

sdiff: sdiff.rkt
	raco exe sdiff.rkt

test: pyparse sdiff
	for i in tests/*.py; do make -s run < $$i > $$i.out; ./sdiff $$i.out $$i.expected; done
