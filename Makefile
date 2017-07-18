include stack.mk

IVORY_REPO             ?= ../ivory
TOWER_REPO             ?= ../tower
IVORY_TOWER_STM32_REPO ?= ../ivory-tower-stm32

default:
	stack build .

test: haskell-backend-test
test: ivory-backend-test
test: tower-backend-test
test: rpc-backend-test
test: elm-backend-test

haskell-backend-test: default
	stack exec -- cidl -b haskell \
			   --debug \
			   -i tests/example.cidl \
			   -o tests/cidl-haskell-backend-test \
			   -p cidl-haskell-backend-test \
			   -n Cidl.Haskell.Test
	make -C tests/cidl-haskell-backend-test test

haskell-backend-test-clean:
	-rm -rf tests/cidl-haskell-backend-test

ivory-backend-test: default
	stack exec -- cidl -b ivory \
			   --debug \
			   -i tests/example.cidl \
			   -o tests/cidl-ivory-backend-test \
			   -p cidl-ivory-backend-test \
			   -n Cidl.Ivory.Test \
			   --ivory-repo=$(IVORY_REPO)
	make -C tests/cidl-ivory-backend-test test

ivory-backend-test-clean:
	-rm -rf tests/cidl-ivory-backend-test

tower-backend-test: default
	stack exec -- cidl -b tower \
			   --debug \
			   -i tests/example.cidl \
			   -o tests/cidl-tower-backend-test \
			   -p cidl-tower-backend-test \
			   -n Cidl.Test \
			   --ivory-repo=$(IVORY_REPO) \
			   --tower-repo=$(TOWER_REPO) \
			   --ivory-tower-stm32-repo=$(IVORY_TOWER_STM32_REPO)
	make -C tests/cidl-tower-backend-test test

tower-backend-test-clean:
	-rm -rf tests/cidl-tower-backend-test

rpc-backend-test: default
	stack exec -- cidl -b haskell-rpc \
			   --debug \
			   -i tests/example.cidl \
			   -o tests/cidl-rpc-backend-test \
			   -p cidl-rpc-backend-test \
			   -n Cidl.Test
	make -C tests/cidl-rpc-backend-test
	#make -C tests/cidl-rpc-backend-test test

rpc-backend-test-clean:
	-rm -rf tests/cidl-rpc-backend-test

elm-backend-test: default
	stack exec -- cidl -b elm \
			   --debug \
			   -i tests/example.cidl \
			   -o tests/cidl-elm-backend-test \
			   -p cidl-elm-backend-test \
			   -n Cidl.Test
	make -C tests/cidl-elm-backend-test

elm-backend-test-clean:
	-rm -rf tests/cidl-elm-backend-test

clean: ivory-backend-test-clean
clean: tower-backend-test-clean
clean: haskell-backend-test-clean
clean: rpc-backend-test-clean
clean: elm-backend-test-clean

TRAVIS_STACK ?= stack --no-terminal --system-ghc --skip-ghc-check

travis-test:
	$(TRAVIS_STACK) build --test --no-run-tests --haddock --no-haddock-deps --pedantic
	make test
