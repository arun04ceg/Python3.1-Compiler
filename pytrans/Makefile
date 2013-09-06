all: pytrans
	@echo "compiled"

pytrans: pytrans.rkt hir-header.rkt
	raco exe pytrans.rkt

run: pytrans
	@pylex | pyparse | ./pytrans

standard: pytrans
	@pylex | pyparse | pytrans

trans: pytrans
	@./pytrans

clean:
	rm -rf pytrans

test_sdiff: pytrans
	for i in tests/*.py; do make -s run < $$i > $$i.out; make -s standard < $$i > $$i.expected; ./sdiff $$i.out $$i.expected;  done

test: pytrans
	for i in tests/*.py; do printf "%s\n" "Testing: $$i"; make -s run < $$i > $$i.out; make -s standard < $$i > $$i.expected; cat hir-header.rkt $$i.expected > $$i.expected.rkt | cat hir-header.rkt $$i.out > $$i.out.rkt; racket $$i.expected.rkt > $$i.expected.txt; racket $$i.out.rkt > $$i.out.txt; diff -q $$i.expected.txt $$i.out.txt; done
