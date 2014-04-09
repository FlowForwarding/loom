.PHONY: simple_ne stats_poller

all: simple_ne stats_poller

simple_ne:
	cd simple_ne; make

stats_poller:
	cd stats_poller; make
