SHELL := /bin/bash
.PHONY: all clean run


all: clean simple

clean:
	@echo "clean start"
	rm -rf ebin/*.beam
	rm -rf *.dump
	@echo "clean stop"

simple:
	erlc -o ebin src/*.erl

run:
	erl -pa ebin
