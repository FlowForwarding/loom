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
    [Erlang.org](http://www.erlang.org/download.html)
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

## Installation

Download and compile the Tapestry collector

```bash
% git clone https://github.com/FlowForwarding/tapestry
% cd tapestry
% make
```

If required, download and compile LINC for the network switches

```bash
% git clone https://github.com/FlowForwarding/LINC-Switch.git
You will need to edit $LINC_ROOT/rel/files/sys.config and then
% make compile
% make rel
```

## Deployment

An example deployment diagram: ![alt text][Illustration]

[Illustration]: https://raw.github.com/FlowForwarding/tapestry/master/docs/images/tapestry_deployment.jpg "Tapestry deployment"

## Configure Tapestry Collector
Tapestry Collector Configuration file is located in
$TAPESTRY_ROOT/tapestry.config file

In the below file, LINC Switch IP Address is 10.10.10.149 and DNS Server
connected to Port 1 (as per LINC Switch's sys.config file) of the LINC
Switch is 10.10.10.10.  Port 2 (as per LINC Switch's sys.config file) of
the LINC Switch is connected to the Top of the Rack switch to act as a
receptor for DNS Queries and responses.

```erlang
{ofdps,[{ofdp,{ip_addr,{10,10,10,149}},
              {dns_port,1},
              {client_port,2},
              {dns_ips,[{10,10,10,10}]}
         }]}.
```


## Running Tapestry

### First Install Flows on the Switch and make sure everything is working
```erlang
# erl -pa ebin
> tap_loom:start().
> tap_loom:config().
> q().
```

Now test connectivity.  If the DNS Server IP address connected to Port 1 of the LINC Switch is 10.10.10.10, then
```bash
# dig @10.10.10.10 flowforwarding.org
```
Now you can start Tapestry Collector
```erlang
# erl -pa ebin
> tapestry:start().
```

Using a browser, go to URL - http://<tapestry_collector_hostname:28080/nci.html to see NCI graph and number.

## Additional Documentation
>1. Technical White Paper: “[A Network Complexity Index for Networks of Networks] (http://www.flowforwarding.org/nci-article)” by Stuart Bailey and Robert L. Grossman
>2. NCI White Paper: [Tapestry for Tracking Network Complexity Index (NCI)] (http://www.infoblox.com/downloads/resources/network-complexity/download)
