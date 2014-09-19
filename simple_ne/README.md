Simple Network Executive
========================
The simple network executive (simple_ne) is an example implementation using the LOOM framework.  It accepts connections from switches and allows for interacting with the switches via the Erlang shell.

## Configuration

Set the configuration rel/files/sys.config.  You may configure of_driver, ofs_handler, simple_ne, lager, and sasl.  of_driver listen_port is the TCP/IP port LOOM uses to listen for connections from switches.  listen_ip is the IP address of the listener port.  {0,0,0,0} means any address.  It should not be necessary to change any of the other of_driver settings or any ofs_handler settings.  simple_ne settings are described below.

## Building

After getting a copy of the LOOM repo:

1. cd simple_ne
2. edit the rel/files/sys.config as needed
3. make

## Starting

After running make offline to build the release use the utility program rel/loom/bin/loom start loom to start and stop the LOOM node.  For example: rel/loom/bin/loom console starts LOOM with an interactive Erlang shell.
