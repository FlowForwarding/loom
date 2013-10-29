# Tapestry: A Network Complexity Analyzer

## Overview

Tapestry is a system that measures network complexity by monitoring DNS
queries via a series of one or more Openflow enabled network taps in
front of a DNS server.

Network complexity information (NCI) is collected over time in an
internal database and reports can be viewed via a built in Web based
interface.

## Requirements

Tapestry requires:

* A server running erlang to collect and report the NCI data
* One or more DNS servers to collect data from
* One or more Open flow enabled Switches or network white boxes with at
    least 3 ports to act as network taps. (or you can install LINC on
    the network white boxes)
* Network cabling and access to install the taps inline in front of
    the DNS servers
* Network connectivity between the taps and the collector

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
% ./configure
% make
% make install
```

## Deployment

[Illustration] (docs/images/tapestry_deployment.jpg)


## Running Tapestry

```erlang
# erl -pa ebin
> tapestry:start().
```

## Additional Documentation
>1. Technical White Paper: “[A Network Complexity Index for Networks of Networks] (http://www.flowforwarding.org/nci-article)” by Stuart Bailey and Robert L. Grossman
>2. NCI White Paper: [Tapestry for Tracking Network Complexity Index (NCI)] (http://www.infoblox.com/downloads/resources/network-complexity/download)
