.PHONY : complier tests
compiler:
	@cd src/ && make
	@cd ../

tests:
	@cd test/ && ./run_tests.sh
	@cd ../

clean_tests:
	@rm ./test/*.c*
	@rm ./test/*.i.*
