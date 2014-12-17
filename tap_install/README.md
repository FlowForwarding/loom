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
