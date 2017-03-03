compile:
	erlc *.erl
test: compile
	erl -pa . -noshell -boot start_clean -s suite run -s init stop
