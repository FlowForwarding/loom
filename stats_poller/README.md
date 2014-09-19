Stats Poller
============
Use the simple_ne application environment to control the behavior of the stats collector.

1. stats is the list of the stats to poll.  The valid values are: flow, table, aggregate, port, queue, group, and meter.  These correspond to the similarly named stats get requests in the open flow protocol.
2. stats_interval is the number of seconds between polls.  Set to 'disable' to disable stats polling.

The open flow stats are published as folsom gauge metrics.  The following naming scheme is used to encode the open flow stats into folsom metric names:

- *datapath_id*-flow-*table_id*-*priority*-*cookie* - flow stats
- *datapath_id*-table-*table_id* - table stats
- *datapath_id*-aggregate - aggregate stats
- *datapath_id*-port-*port_num* - port stats
- *datapath_id*-queue-*queue_id*-*port_num* - queue stats
- *datapath_id*-group-*group_id* - group stats
- *datapath_id*-group-*group_id*-bucket-*bucket_id* - group bucket stats
- *datapath_id*-meter-*meter_id* -meter stats
- *datapath_id*-meter-*meter_id*-band-*band_id* - meter band stats

For bucket and band stats, the statistics poller creates bucket and band ids to give each one a unique identifier within the group and meter.  The bucket and band ids are not in the open flow protocol.

## Configuration

Set the configuration rel/files/sys.config.  You may configure of_driver, ofs_handler, simple_ne, lager, and sasl.  of_driver listen_port is the TCP/IP port LOOM uses to listen for connections from switches.  listen_ip is the IP address of the listener port.  {0,0,0,0} means any address.  It should not be necessary to change any of the other of_driver settings or any ofs_handler settings.  simple_ne settings are described below.

## Building

After getting a copy of the LOOM repo:

1. edit the rel/files/sys.config as needed
2. make offline

## Starting

After running make offline to build the release use the utility program rel/loom/bin/loom start loom to start and stop the LOOM node.  For example: rel/loom/bin/loom console starts LOOM with an interactive Erlang shell.

