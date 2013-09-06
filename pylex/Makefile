#
#Name: Arun Baskar
#UID : u0811338
#Project done with Arvind Haran
#

default: pylex

pylex.yy.c: pylex.l
	flex -o pylex.yy.c pylex.l

pylex: pylex.yy.c
	gcc -g -o pylex -ansi -W -w -Wall pylex.yy.c -lfl

run: pylex
	./pylex

clean:
	rm -v -f pylex pylex.yy.c tests/*.out

test: pylex
	for i in p1-new-tests/*.py; do ./pylex < $$i > $$i.out; pylex < $$i > $$i.expected; echo $$i; diff $$i.out $$i.expected; done

#
#Name: Arun Baskar
#UID : u0811338
#Project done with Arvind Haran
#

