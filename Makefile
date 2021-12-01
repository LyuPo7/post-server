
package = "post-server"

stack_yaml = STACK_YAML="stack.yaml"
stack = $(stack_yaml) stack

haskell_files = $(shell find . -name '*.hs')

build:
	$(stack) build $(package)

test:
	$(stack) test $(package)

run:
	$(stack) build --fast && $(stack) exec -- $(package)

hlint:
	echo "src:" && hlint src
	echo "app:" && hlint app
	echo "test:" && hlint test

format:
	fourmolu -i $(haskell_files)

.PHONY : build test hlint format run