.PHONY: all

all:	libs
	@echo "*** library installation complete ***"

libs:   yaws yawsUpdate loom loomUpdate 

yaws:
	git clone https://github.com/klacke/yaws.git

yawsUpdate:
	cd yaws; git pull; autoconf; ./configure; make


loom:
	git clone https://github.com/sigstop/loom.git

loomUpdate:
	cd loom; git pull; make
