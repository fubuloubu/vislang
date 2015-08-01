.PHONY : compiler tests
compiler:
	@cd src/ && make
	@cd ../

tests:
	@cd test/ && ./run_tests.sh
	@cd ../

clean_tests:
	@rm ./test/*.c*
	@rm ./test/*.o
	@rm ./test/*.so
	@rm ./test/*.py
