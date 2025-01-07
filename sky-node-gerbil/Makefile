ARCH := $(shell uname -m)
DOCKER_IMAGE := "mukn/all:$(ARCH)"

default: linux-static

all: build test

build:
	./build.ss

test:
	./unit-tests.ss

# Build using nix-build
nix:
	./build.ss nix

build-release:
	/opt/gerbil/bin/gxpkg deps -i
	/opt/gerbil/bin/gxpkg build --release

linux-static:
	docker run -it \
	-e USER=$(USER) \
	-e GERBIL_PATH=/src/.gerbil \
	-v $(PWD):/src:z \
	$(DOCKER_IMAGE) \
	make -C /src/ build-release

install:
	mv .gerbil/bin/pubsub /usr/local/bin/pubsub

clean:
	gerbil clean
	gerbil clean all

.PHONY: all build test nix default clean install build-release linux-static
