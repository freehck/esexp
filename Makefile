
.PHONY: build clean

build:
	corebuild -use-menhir test.native

clean:
	rm -rf _build test.native

