all: FORCE
	stack setup
	stack build
	stack test

install: FORCE
	stack install --install-ghc --flag language-conf:conffmt --flag language-conf:fromconf --flag language-conf:toconf

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
