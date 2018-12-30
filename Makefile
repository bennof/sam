
all: build


build: sam.beam

src: sam.erl

%.beam: %.erl
	./sam.es compile $<

%.erl: %.xrl
	./sam.es leex $<

test: 
	./sam.es test sam.xrl README.md example/test.html

clean: 
	rm -f *.beam *.erl
