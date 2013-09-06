
1) Name : Arun Baskar(u0811338). This project is done along with Arvind Haran(u0809850).

2) We rely on the pylex executable present in the server. We are NOT using the pylex we did in the project 1. Hence "make lex" and "make run" will make use of the pylex present in the caprica server.

3) We got all the features working based on the "simplified Python grammar" given by the professor.

4) The only file we changed in the stub code given by the professor is the "python-ast.grm.sx" file.

Files turned in:

1) pyparse.rkt  
2) python-ast.grm.sx 
3) derivative-parsers.rkt 
4) README.txt 
5) sdiff.rkt
6) Makefile

Make options:

1) make ->  compile the program.
2) make run ->  accept input on STDIN and send output to STDOUT.(Uses pylex available on caprica)
3) make lex -> run the lexer on STDIN.(Uses pylex available on caprica)
4) make parse -> run the parser on STDIN.
5) make test -> run the tests on python files available in "tests/" folder.

