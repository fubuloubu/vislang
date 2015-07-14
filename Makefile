.PHONY : complier tests
compiler:
	@cd src/ && make
	@cd ../

tests:
	@cd test/ && ./run_tests.sh
	@cd ../
