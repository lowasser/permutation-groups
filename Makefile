.PHONY : test Tests

test : Tests
	./Tests

Tests:
	ghc --make Tests -o Tests -fforce-recomp
