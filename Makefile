all: FORCE
	stack setup
	stack build
	stack test
	make conffmt
	make fromconf
	make toconf

build: FORCE
	stack build

test: FORCE
	stack test

conffmt: FORCE
	stack build --flag language-conf:conffmt

fromconf: FORCE
	stack build --flag language-conf:fromconf

toconf: FORCE
	stack build --flag language-conf:toconf

FORCE:
