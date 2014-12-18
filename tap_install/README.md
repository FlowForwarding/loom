# Tapestry Flow Installer

## Overview

This is a simple OpenFlow controller that installs flows on switches that may be used by Tapestry.

## Requirements

* OpenFlow compatible switch.

## Installation

* Download an dcompile the LOOM examples

```bash
% git clone https://github.com/FlowForwarding/loom
% make
```

This builds the Tapestry collector (tapestry) and this controller.
To build only this controller, run make in the tap_installer directory.

## Deployment

Configure the OpenFlow switch providing the DNS tap for Tapestry to
connect to this controller.  Alternatively, this controller you can
configure this controller to connect to the OpenFlow switch
(see configuration options).

## Config

rel/files/sys.config has the listen port for the controller:

```erlang
 {of_driver,[ {listen_ip, {0,0,0,0}},
              {listen_port, 6653},
```

This is the default port, so this value probably doesn't need to change.

To set the ports for the flows, there is also a tapestry.config
file.  I've checked this file into the top directory for tap_install.
tap_install expects to find this file in rel/tap_install (after you
rebar generate).

The sys.config specifies the name and location of the file:

```erlang
 {tap_install, [{config_file, "tap_install.config"}
 ]},
```

When tap_install runs, the current working directory is rel/tap_install.
You can also specify an absolute path if you want to put the
tap_install.config file somewhere else (or change its name):

```erlang
 {tap_install, [{config_file, "/usr/local/etc/tap_install.config"}
 ]},
```

The tap_install.config has the following format:

```erlang
{"00:00:00:00:08:00:27:C5",[{port1, 1},
                            {port2, 2},
                            {dns_ips, [{10,0,1,2},{10,3,2,3}]}]}.
```

and

```erlang
{default,                  [{port1, 5},
                            {port2, 6},
                            {dns_ips, [{10,0,1,2},{10,3,2,3}]}]}.
```

Where the fields are:

```erlang
{DatapathId|default, [{port1, Port1}, {port2, Port2}, {dns_ips, [Ip1, Ip2, ...]}]}.
```

DatapathId is the datapath id of the switch using this config tuple.
tap_install will use the 'default' one if none of the config tuples
have the datapath id of the switch.  Port1 is the port connected
to the DNS server.  Port2 is the port connected to the client side
of the network.  Ip1, Ip2, ... are the IP addresses of the DNS
servers in the tuple format.  You may have any number of these tuple
config sets, but only one for default.  You must have at least one
that can be applied to the switch, otherwise tap_install will crash.
It might easiest to use a default config set and not specify any
datapath ids.

If tap_install needs to connect to the switch (rather than the other
way around), then add lines like:

```erlang
{connect_to, {{192,168,56,102}, 6653}}.
```

which specifies the IP address (in tuple form) of the switch and
the switch's listener port.

You can have any number of these.  If there are none, then tap_install
will only listen for switches to connect.

## Running

To run in interactive mode, run:

```bash
% rel/tap_install/bin/tap_install console
```

To run in a headless mode (no shell), run:

```bash
% rel/tap_install/bin/tap_install start
```

Logs are in rel/tap_install/logs.
