all: FORCE
	stack setup
	stack build
	stack test
	make conffmt

build: FORCE
	stack build

test: FORCE
	stack test

conffmt:
	stack build --flag language-conf:conffmt

FORCE:
