.PHONY : tests
compiler:
	cd src && $(MAKE)

tests: compiler
	cd test && ./run_tests.sh

clean:
	@rm -f ./testall.log
	@rm -f ./test/*.c*
	@rm -f ./test/*.o
	@rm -f ./test/*.so
	@rm -f ./test/*.py
	@cd src/ && $(MAKE) clean; cd ../
