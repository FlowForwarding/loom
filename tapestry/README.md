# Tapestry: A Network Complexity Analyzer

## Overview

Tapestry is a system that measures network complexity by capturing Domain
Name System (DNS) endpoint interaction data via OpenFlow enabled network
taps in front of one or more of an organization's internal recursive DNS
servers.

Network interaction data is collected over time, distinct organizational
activities are detected, and a Network Complexity Index (NCI) is computed
on-line.  The NCI can be viewed and graphed in near real-time via a built
in Web based interface.

## Requirements

Tapestry requires:

* Erlang/OTP distributed computing platform, available from
    [Erlang.org](http://www.erlang.org/download.html).  R16B03 or newer.
* One or more servers running the Erlang to aggregate the data and 
    calculate the NCI.
* One or more internal recursive DNS servers to provide the raw data feeds
* One or more OpenFlow enabled switches or network white boxes with at
    least 3 ports to act as network taps. (or you can install LINC on
    the network white boxes)
* Network cabling and access to install the taps inline in front of
    the internal recursive DNS servers
* Network connectivity between the taps and machine or cluster running 
    the Erlang/OTP
* EXPERIMENTAL: graphviz is required if the config parameter use_graphviz is true: http://www.graphviz.org

## Installation

Download and compile the LOOM examples

```bash
% git clone https://github.com/FlowForwarding/loom
% make
```

This builds the Tapestry collector (tapestry) and configuration node
(icontrol).  To build only tapestry and icontrol, run make in the
tapestry and icontrol directories.

If required, download and compile LINC for the network switches

```bash
% git clone https://github.com/FlowForwarding/LINC-Switch.git
```

You will need to create ```$LINC_ROOT/rel/files/sys.config``` file by adding the correspond ports and adding the Tapestry
Controller.  There are 2 ports needed for Tapestry Controller and hence, we can generate a sys.config file by running the below command .
```bash
% cd $LINC_ROOT
% scripts/config_gen -s 0 eth1 eth2 eth3 eth4 -c tcp:10.10.10.75:6633 tcp:10.10.10.75:6634 -o rel/files/sys.config
% make compile
% make rel
```

## Deployment

An example deployment diagram: ![alt text][Illustration]

[Illustration]: https://raw.github.com/FlowForwarding/tapestry/master/docs/images/tapestry_deployment.jpg "Tapestry deployment"

## Running Tapestry

### First Install Flows on the Switch and make sure everything is working

Use icontrol to install the flows on the Switch

Start the icontrol node
```bash
# cd icontrol
# rel/icontrol/bin/icontrol console
```

See which switches are connected

```erlang
> iof:switches().
Switch-Key DatapathId                       IpAddr            Version
---------- -------------------------------- ----------------- -------
*1         00:01:08:00:27:C5:95:48          {192,168,56,102}  4      
 2         00:00:08:00:27:C5:95:48          {192,168,56,102}  4      
ok
```

Identify the switch you want to configure.  Use the Switch-Key for that
switch in the following commands.  The examples assume you want to configure
the switch with the Switch-Key of 1 (datapath id is {0,<<8,0,39,197,149,72>>}).

```erlang
> iof:tapestry_config(1, 2, 3, [{10,2,3,4},{10,2,3,44}]).
```

Removes all the flows from table 0, copies udp traffic entering port 2
to the controller from 10.2.3.4 and 10.2.3.44 (the DNS servers),
and bridges ports 2 and 3.

```erlang
> iof:flows(1).
```

Shows all the flows installed on the switch by sending the flow_stats_request
to the switch.

Now test connectivity.  If the DNS Server IP address connected to Port 1 of the LINC Switch is 10.10.10.10, then
```bash
# dig @10.10.10.10 flowforwarding.org
```

Now you can start Tapestry Collector
```erlang
# cd tapestry
# rel/tapestry/bin/tapestry start
```

Using a browser, go to URL - http://<tapestry_collector_hostname:28080/nci.html to see NCI graph and number.

### Tapestry configuration for icontrol

You can create config files for icontrol to capture the tapestry configuration.
The format of this file is:

```erlang
{switch, [{ip_addr, {192,168,56,102}},
          {dns_port, 1},
          {client_port, 2},
          {dns_ips, [{10,0,2,60}, {10,48,2,5}]}
]}.
{switch, [{dpid, "00:00:08:00:27:C5:95:48"},
          {dns_port, 1},
          {client_port, 2},
          {dns_ips, [{10,0,2,60}, {10,48,2,5}]}
]}.
```

Switches are identified by either ip address or datapath id (dpid).  The other
fields are the port connected to the DNS server (dns_port), the port connected
to the rest of the network (client_port) and the ip addresses of the
DNS servers.  There can be any number of switch tuples in the config file.

```erlang
> iof:tapestry_config(all, Filename).
```

Configures all of the attached switches using information in Filename.

```erlang
> iof:tapestry_config(Key, Filename).
```

Configures only the switch with the switch key Key using the information
in Filename.

### Interactive tapestry

You can change the limits on the community details at runtime.  Start
tapestry with an interactive shell:

```erlang
# cd tapestry
# rel/tapestry/bin/tapestry console
```

Then change the limits with:

```erlang
tap_ds:setlimit(max_vetrices, 1000)
tap_ds:setlimit(max_edges, 10000)
tap_ds:setlimit(max_communities, 100)
tap_ds:setlimit(comm_size_limit, 10)
```

A limit of infinity sets no limit.

## sys.config
The Tapestry sys.config (node configuration) file is in rel/files/sys.config.
The runtime copy is in rel/tapestry/releases/1/sys.config.  The files copy
overwrites the releases copy when you make Tapestry.  You may also override values in sys.config with values in tapestry.config.  The source copy of tapestry.config is in rel/files/tapestry.config.  The runtime copy is in rel/tapestry/tapestry.config.

Some configuration values (marked in **bold**) may be changed at runtime.  Update the value in tapestry.config and then use the tapestry utility to tell tapestry to update its runtime configuration values.

```erlang
# cd tapestry
# rel/tapestry/bin/tapestry config
```

Alternatively, from the tapestry shell you can run:

```erlang
> tap_config:refresh().
```

Section | Key | Example | Description
------- | --- | ------- | -----------
tapestry | config_file | "tapestry.config" | Name of the tapestry config file.
tapestry | web_address | {127,0,0,1} | webserver listener IP address
tapestry | web_port | 28080 | webserver listener port
tapestry | web_log | "./log" | log directory for webserver
tapestry | web_id | "tapestry" | webserver identifier
tapestry | ftpd_address | {0,0,0,0} | ftp server listener IP address
tapestry | ftpd_port | 7777 | ftp server listener port
tapestry | datasources | [packet_in] | identifies sources of DNS information
tapestry | connect_to | [{10.2.3.3},6653}] | switches to connect to (for packet_in)
tapestry | max_collector_idle_time | 600 | remove a collector from the report if no data received for this many seconds
tapestry | nci_min_interval | {seconds, 15} | shortest time between nci calculations
tapestry | max_vertices | 300 | maximum number of vertices before dropping a community's detail
tapestry | max_edges | 1000 | maximum number of edges before dropping a community's detail
tapestry | max_communities | 300 | maximum number communities in the communities graph
tapestry | comm_size_limit | 300 | when dropping community details, drop details of communities larger than this limit
tapestry | qps_max_interval | {seconds, 15} | longest time between Query/Sec ui updates
tapestry | clean_interval | [{days,0},{hms,{1,0,0}}] | interval between purging old data from nci calculation
tapestry | **data_max_age** | [{days,2},{hms,{0,0,0}}] | purge data older than data_max_age
tapestry | use_graphviz | false | EXPERIMENTAL: use graphviz to calculate the location of the community dots in community graphs
tapestry | neato_bin | "user/local/bin/neato" | path to neato from graphviz installation
tapestry | community_detector | part_louvain | module to use for community detector (only set in sys.config)
tapestry | **requester_whitelist** | [{"10.0.0.0",8}] | include these ip addresses as requesters
tapestry | **requester_blacklist** | [{"192.168.0.0",16}] | exclude these ip addresses as requesters
tapestry | **resolved_whitelist** | [{"::",0}] | include these ip addresses as resolved responses
tapestry | **resolved_blacklist** | [{"10.13.11.24",32}] | exclude these ip addresses as reolved responses
tapestry | **query_whitelist** | [".com$"] | list of regular expressions of dns queries to include
tapestry | **query_blacklist** | ["google.com$"] | list of regular expressions of dns queries to exclude
tapestry | **save_files** | false | save copies of the log files loaded via ftp
tapestry | **save_file_dir** | false | directory to store copies of log files loaded via ftp (only applicable when save_files is true)
tapestry | **reverselookup** | true | true - lookup hostname of dns requester IP addresses; false - do not lookup hostname of dns requesters
of_driver | listen_ip | {0,0,0,0} | open flow controller listener IP address
of_driver | listen_port | 6653 | open flow controller listener port

You may specify one or more datasources, however some combinations are
not allowed.  test_ui should not be used with any other datasource and
anonymized and logfile may not be used together.

neato_bin must be the path to the neato binary from the graphviz installation.  This is only used if use_graphviz is true.

Only requester, resolved address pairs that are included int he requester_whitelist and resolved_whitelist and not excluded by the requester_blacklist and resolved_blacklist are included in the data considered by tapestry's community detection and NCI calculation.  An address in these lists is specified as with a ipv4 or ipv6 address (as a string) and the number of bits that much match exactly.  For example, to match all 10.x.x.x networks, use {"10.0.0.0",8}.  To match 10.12.22.44 exactly, use {"10.12.22.44",32}.

datasource|Description
----------------|-----------
packet_in | Receive and process packet_in messages from open flow switch
test_ui | Generate random data (for UI testing)
anonymized | Process anonymized logs from IB Grid
logfile | Process logs from IB Grid

## Additional Documentation
>1. Technical White Paper: “[A Network Complexity Index for Networks of Networks] (http://www.flowforwarding.org/nci-article)” by Stuart Bailey and Robert L. Grossman
>2. NCI White Paper: [Tapestry for Tracking Network Complexity Index (NCI)] (http://www.infoblox.com/downloads/resources/network-complexity/download)
>3. Tapestry [Architecture Details] (https://github.com/FlowForwarding/tapestry/blob/master/docs/Tapestry.pdf?raw=true)
