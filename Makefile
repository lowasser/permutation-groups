.PHONY : test

test : Tests
	./Tests

Tests:
	ghc --make Tests -o Tests
