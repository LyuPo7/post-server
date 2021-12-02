
package = "post-server"
app = "app"

stack_yaml = STACK_YAML="stack.yaml"
stack = $(stack_yaml) stack

haskell_files = $(shell find . -name '*.hs')

build:
	$(stack) build $(package)

test:
	$(stack) test $(package)

run:
	$(stack) build --fast && $(stack) exec -- $(package)

ghci:
	$(stack) ghci $(package):lib --ghci-options='-j6 +RTS -A128m'

test-ghci:
	$(stack) ghci $(package):test:$(package)-test --ghci-options='-j6 +RTS -A128m'

ghcid:
	$(stack) exec -- ghcid -c "stack ghci $(package):lib --test --ghci-options='-fobject-code -fno-warn-unused-do-bind -j4 +RTS -A128m' --main-is $(app)"

hlint:
	echo "src:" && hlint src
	echo "app:" && hlint app
	echo "test:" && hlint test

format:
	fourmolu -i $(haskell_files)

.PHONY : build test hlint format ghci run