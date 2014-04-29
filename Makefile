.PHONY: simple_ne stats_poller tapestry icontrol

all: simple_ne stats_poller tapestry icontrol

simple_ne:
	cd simple_ne; make

stats_poller:
	cd stats_poller; make

tapestry:
	cd tapestry; make

icontrol:
	cd icontrol; make
