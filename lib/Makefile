.PHONY: all

all:	libs
	@echo "*** library installation complete ***"

libs:   rebar rebarUpdate yaws yawsUpdate loom loomUpdate bifrost bifrostUpdate

rebar:
	git clone https://github.com/basho/rebar.git

rebarUpdate:
	cd rebar; git pull; ./bootstrap

yaws:
	git clone https://github.com/klacke/yaws.git

yawsUpdate:
	cd yaws; git pull; autoconf; ./configure; make


loom:
	git clone https://github.com/sigstop/loom.git

loomUpdate:
	cd loom; git pull; make

bifrost:
	git clone https://github.com/thorstadt/bifrost.git

bifrostUpdate:
	cd bifrost; git pull; make
