# icontrol: Interactive Open Flow Controller

## Overview

icontrol is a utility node that provides interactive commands
to send Open Flow requests to connected switches.  It is intended
to be run in the console mode.

## Running icontrol

Build icontrol:

```bash
cd icontrol
make
```

Run:

```bash
rel/icontrol/bin/icontrol console
```

## Using icontrol

icontrol is a open flow controller with an basic interactive interface.
The iof module has functions to send commands to the switch and
and interrogate the switch.

### Configure switch

To use icontrol, first configure your switch to connect to the icontrol
node.  Set the controller IP address to the IP address of the host
running icontrol.  By default, icontrol listens on port 6653 (see sys.config).

### List of connected switches

The iof:switches/0 function lists the switches connected to icontrol.

```erlang
> iof:switches().
Switch-Key DatapathId                       IpAddr            Version
---------- -------------------------------- ----------------- -------
*1         00:00:08:00:27:C5:95:48          {192,168,56,102}  4      
 2         00:01:08:00:27:C5:95:48          {192,168,56,102}  4      
 3         00:00:00:16:3E:49:9B:CE          {10,48,33,182}    4      
 4         00:00:00:16:3E:5F:4D:46          {10,32,1,39}      4      
ok
```

You use the Switch-Key to identify the switch in other commands.  The
switch's datapath id, ip address, and Open Flow protocol version
are also shown to help you recognize the switches.  The Switch-Key marked
with a * is the default switch.

### The default switch

The first switch to connect to icontrol is the default switch.  If you
do not specify
a Switch-Key in commands, the command is sent to the default switch.  You
can change the default switch with the iof:default/1 function.  iof:default/0
shows the current default.

```erlang
> iof:switches().
Switch-Key DatapathId                       IpAddr            Version
---------- -------------------------------- ----------------- -------
*7         00:00:00:16:3E:6A:9B:B9          {10,32,3,144}     4      
 8         00:00:08:00:27:C5:95:48          {192,168,56,102}  4      
 9         00:01:08:00:27:C5:95:48          {192,168,56,102}  4      
ok
> iof:default().
7
> iof:default(8).
ok
> iof:switches().
Switch-Key DatapathId                       IpAddr            Version
---------- -------------------------------- ----------------- -------
 7         00:00:00:16:3E:6A:9B:B9          {10,32,3,144}     4      
*8         00:00:08:00:27:C5:95:48          {192,168,56,102}  4      
 9         00:01:08:00:27:C5:95:48          {192,168,56,102}  4      
ok
> iof:default().
8
```

### Listing flows

The iof:flows function sends a flow stats request to the switch and
returns the flow stats reply.  Without any arguments, iof:flows sends
the request to the default switch.

The default switch has no flows.

```erlang
> iof:flows().
{ofp_flow_stats_reply,[],[]}
ok
```
Switch 7 has two flows.

```erlang
> iof:flows(7).
fp_flow_stats_reply,[],
    [{ofp_flow_stats,0,1141,389343000,100,0,0,[],
         <<0,0,0,0,0,0,0,10>>,
         0,0,
         {ofp_match,
             [{ofp_field,openflow_basic,in_port,false,<<0,0,0,2>>,undefined}]},
         [{ofp_instruction_apply_actions,2,
              [{ofp_action_output,16,1,no_buffer}]}]},
     {ofp_flow_stats,0,1141,560675000,100,0,0,[],
         <<0,0,0,0,0,0,0,10>>,
         0,0,
         {ofp_match,
             [{ofp_field,openflow_basic,in_port,false,<<0,0,0,1>>,undefined}]},
         [{ofp_instruction_apply_actions,2,
              [{ofp_action_output,16,2,no_buffer}]}]}]}
ok
```

### flows

The iof:forward_mod function adds a flow to the switch.  The flow
forwards all traffic from an input port to one or more output ports using
the given priority.  The flows are added to table 0.
Without the switch key parameter, iof:forward_mod
adds the flow to the default switch.

```erlang
> iof:forward_mod(1, 100, 3, 4).
{ok,{ofp_message,4,error,0,
                 {ofp_error_msg,bad_action,bad_out_port,<<>>}}}
```

Attempt to add a flow with priority 100 to switch 1 to forward traffic
from port 3 to port 4 is rejected by the switch because the output
port is not valid.

```erlang
> iof:forward_mod(100, 1, [2, controller]).
{ok,noreply}
```

Successfully adds a flow with priority 100 to the
 default switch to forward traffic
from port 1 to ports 2 and the controller.

Use the iof:bridge function to bridge two ports.  This is a convenience
function and is the equivalent of using iof:forward_mod twice, once to
forward traffic from port1 to port2, and again to forward traffic from
port2 to port1.  If you do not include the switch key parameter
the flow mods are added to the default switch.

```erlang
> iof:bridge(1, 200, 4, 5).
[{ok, noreply}, {ok, noreply}].
```

Bridges ports 4 and 5 on switch 1 using flows with the priority of 200.
This is the equivalent of:

```erlang
> iof:forward_mod(1, 200, 4, 5).
{ok,noreply}
> iof:forward_mod(1, 200, 5, 4).
{ok,noreply}
```

The iof:clear_flows functions removes flows from the switch.  Without the
switch key parameter, iof:clear_flows removes the flows from the
default switch.  The variant
ifo:clear_flows0 removes the flows on the default switch from table 0.

```erlang
> iof:clear_flows(3, 5)
{ok,noreply}
```

Deletes all the flows in table 5 of switch 3.

```erlang
> iof:clear_flows0()
{ok,noreply}
```

Deletes all the flows in table 0 on the default switch.

### Tapestry support

Tapestry users may use icontrol to configure the switch for tapestry.
The iof:tapestry_config function:

1. optionally clears all flows in table 0
2. bridges the DNS server and Client ports
3. forwards UDP traffic from the DNS server port to the controller

The steps may also be performed individually using
iof:clear_flows to remove the flows, iof:bridge to bridge the two ports,
and iof:dns_tap to forward the dns udp traffic.

If the switch key parameter is not given to iof:tapestry_config the function
configures the default switch.

```erlang
> iof:tapestry_config(3, 1, 2, [{10,4,2,2}, {10,4,2,3}]).
ok
```
Configures switch 3 for tapestry by bridging ports 1 and 2 and forwarding 
UDP packets from {10,4,2,2} and {10,4,2,3} to the controller (Tapestry node).

This is the same as:
```erlang
> iof:clear_flows(3, 0).
{ok,noreply}
> iof:bridge(3, 100, 1, 2).
[{ok, noreply}, {ok, noreply}].
> iof:dns_tap(3, 200, 1, 2, controller, [{10,4,2,2}, {10,4,2,3}]).
{ok,noreply}
```

If you do not want to remove all the flows from Table 0, you can use
iof:tapestry_config_add instead of iof:tapestry_config.  This is
useful if you are using more than one pair of ports on your
switch for tapestry.

```erlang
> iof:tapestry_config_add(3, 3, 4, [{10,4,2,2}, {10,4,2,3}]).
ok
```
This is the same as:
```erlang
> iof:bridge(3, 100, 4, 4).
[{ok, noreply}, {ok, noreply}].
> iof:dns_tap(3, 200, 4, 4, controller, [{10,4,2,2}, {10,4,2,3}]).
{ok,noreply}
```

For convenience icontrol can read tapestry configurations from config files.
iof:tapestry_config(Key, Filename) reads the configuration from Filename
and configures the switch associated with Key.  iof:tapetry_config
throws an error if the switch is not found in the configuration file.
If Key is not given, iof:tapestry_config attempts to configure the
default switch.  If the special Key "all" is used, iof:tapestry_config
attempts to configure all connected switches.  Switches that are not
in the configuration are not configured, but do not result in an error.
The format of the tapestry configuration file is described below.

### Sending open flow requests

iof:send sends \#ofp_message messages to the switch.  Use of_msg_lib
to create the \#ofp_message records.  iof:version returns the OF
protocol version which is needed to call of_msg_lib.

For example, to delete flows in table 3 on the default switch:

```
> V = iof:version().
4
> Request = of_msg_lib:flow_delete(V, [], [{table_id, 3}]).
{ofp_message,4,undefined,0,
             {ofp_flow_mod,<<0,0,0,0,0,0,0,0>>,
                           <<0,0,0,0,0,0,0,0>>,
                           3,delete,0,0,65535,no_buffer,any,any,[],
                           {ofp_match,[]},
                           []}}
{ok,noreply}
```

### Connecting to a switch

In OF Protocol, switches connect to the controllers.  For simpler setup
and debugging, some switches may be configured to accept connections
from controllers.  iof:connect lets you connect icongtrol to switches
accepting connections from controllers.

```erlang
> iof:connect({192,168,111,1}, 6634).
ok
```

Connects icontrol to the switch listening at 192.168.111.1.

## Command Reference

### iof:debug(on | off)

Turn on or off debugging output.  This can be helpful with icontrol
is receiving a lot of asynchronous messages from the switch.

### iof:send(Key, ofp_mesage{})

Send a ofp_message{} record to the switch associated with Key, or the
default switch if Key is not given.  Use ofp_msg_lib to construct
ofp_message{} records.

### iof:ping(Key)

Send an echo request to the switch associated with Key, or the default
switch if Key is not given.

### iof:forward_mod(Key, Priority, InPort, OutPort)

Create a flow in table 0 on the switch associated with Key, or the
default switch if Key is not given, forwarding traffic from the
InPort to the OutPort at the Priority.

### iof:forward_mod_with_push_vlan(Key, Priority, InPort, OutPort, VlanID)

Create the same flow as with `iof:forward_mod/4` but add a VLAN tag
with VlanID to an outgoing packet.

### iof:forward_mod_via_queue(Key, Priority, InPort, OutPort, QueueID) ###

Create the same flow as with iof:forward_mod/4 but place a packet in
a queue identified by a `QueueID` attached to the output port.

### iof:bridge(Key, Priority, Port1, Port2)

Create flows in table 0 on the switch associated with Key, or the
default switch if Key is not given, forwarding traffic from Port1 to
Port2 and Port2 to Port1 at the Priority.

### iof:clear_flows0()

Clear all of the flows in table 0 on the default switch.

### iof:clear_flows(Key, TableId)

Clear all the flows in the table with TableId on the switch
associated with Key, or the default switch if Key is not given.

### iof:flows(Key)

Show all the flows, via the get flow stats request, on the switch
associated with Key, or the default switch if Key is not given.

### iof:dns_tap(Key, Priority, Port1, Port2, Port3, DnsIps)

Create a flow in table 0 sending UDP responses from the DNS servers
attached to Port1 to ports Port2 and Port3 that at the list of IP
addresses given in DsnIps.  Install the flow on the switch associated
with Key, or the default switch if Key is not given.

### iof:tapestry_config(Key, Port1, Port2, DnsIps)

Configure the switch associated with Key, or the default switch if
Key is not given, for Tapestry.  Port1 is the port connected to
the DNS servers.  Port2 is the client port.  DnsIps is a list of the
DNS server IP addresses.

### iof:connect(IpAddr, Port)

Connect icontrol to a OpenFlow switch listening for connections from
controllers.  This is primarily used for debugging.

### iof:disconnect(Key)

Disconnect from the switch associated with Key, or the default switch
if Key is not given.  If the switch is working properly, it should
reconnect within a few seconds.  If the connection was created
using iof:connect, the controller-side connection handler will automatically
reestablish the connection with the switch.

### iof:default()

Return the Key of the default switch.

### iof:default(Key)

Set the default switch to the switch associated with Key.

### iof:switches()

Show a list of the connected switches.  The Key marked with "*" is the
default switch.  There may not be a default switch.

### iof:version(Key)

Return the OpenFlow protocol version for the switch associated with
Key, or the default switch if Key is not given.  The Open Flow protocol
version is negotiated when the controller and switch connect.

## sys.config

The icontrol node may be configured using the sys.config file.

|application|environment variable|example|description|
|-----------|--------------------|-------|-----------|
|of_driver|listen_ip|{0,0,0,0}|Listen for connections on this IP Address|
|of_driver|listen_port|6653|Listen for connections on this Port|
|of_driver|of_compatible_version|[4]|Supported OF Protocol Versions|

## tapestry.config

The Tapestry config file has one Erlang term per switch.

Example:

```erlang
{switch, [{ip_addr, {10,48,33,185}},
          {dns_port, 1},
          {client_port, 2},
          {dns_ips, [{10,102,3,50}, {10,48,2,5}]}
]}.

{switch, [{ip_addr, {10,32,1,39}},
          {dns_port, 1},
          {client_port, 2},
          {dns_ips, [{10,102,3,50}, {10,48,2,5}]}
]}.

{switch, [{dpid, "00:00:08:00:27:C5:95:48"},
          {dns_port, 1},
          {client_port, 2},
          {dns_ips, [{10,0,2,60}, {10,48,2,5}]},
          {dns_port, 3},
          {client_port, 4},
          {dns_ips, [{10,0,2,70}, {10,48,2,85}]}
]}.
```

The switch identified by datapath id 00:00:08:00:27:C5:95:48 has two
pairs of ports for tapestry.  Port 1 and Port 2 are tapping
DNS servers 10.0.2.60 and 10.48.2.5.  Port 3 and Port 4 are
tapping DNS servers 10.0.2.70 and 10.48.2.85.

Switches are identified by either IP Address or datapath id.  Each
switch entry should have one or the other, but not both.

|Key|Example|Description|
|---|-------|-----------|
|ip_addr|{10,48,33,185}|IP address of switch|
|dpid|00:00:08:00:27:C5:95:48|Datapath ID of switch|
|dns_port|1|Port connected to DNS servers|
|client_port|2|Port connected to clients|
|dns_ip|[{10,0,2,60}]|List of DNS Sever IP Addresses|

## Optical extension support ##

Optical extension is implemented in LINC-Switch by means of `linc_us4_oe`
backend.
It extends Open Flow 1.3.2 protocol by adding custom
enhancements to enable emulating optical networks. `iof` provides several
commands to configure emulated optical switches.

#### iof:oe_ports(Key) ####
Show optical ports' descriptions on switch associated with `Key`.
If `Key` is `default`, show the descriptions on the default switch.

#### iof:oe_ports() ####
The same as `iof:oe_ports(default)`

#### iof:oe_flow_tw(Key, Priority, InPort, OutPort, ChannelNumber) ####
Create a flow in table 0 on the switch associated with `Key`, or the
`default` switch if `Key` is not given, forwarding traffic from the
`InPort` (TPort) to the `OutPort` (WPort) at the Priority. The `OutPort`
is assumed to be an optical one and the `ChannelNumber` indicates
the frequency (lambda).

#### iof:oe_flow_ww(Key, Priority, InPort, InChannelNumber, OutPort, OutChannelNumber) ####
Create a flow in table 0 on the switch associated with `Key`, or the
`default` switch if `Key` is not given, forwarding traffic from the
`InPort` (WPort) to the `OutPort` (WPort) at the Priority. Both ports
are assumed to be optical ones and the `InChannelNumber` and `OutChannelNumber`
indicate respectively the frequency (lambda) to match on the input port and
to send with on the output port.

#### iof:oe_flow_wt(Key, Priority, InPort, InChannelNumber, OutPort) ####
Create a flow in table 0 on the switch associated with `Key`, or the
`default` switch if `Key` is not given, forwarding traffic from the
`InPort` (WPort) to the `OutPort` (TPort) at the Priority. The `InPort`
is assumed to be an optical one and the `InChannelNumber` indicates
the frequency (lambda) to match on.
