Loom
====

The purpose of the LOOM project is to design and prototype an experimental network switch controller that implements the OpenFlow 1.3.x and 1.4 protocols. The project explores the scalability and robustness of such controllers on a scale much larger than typically considered: 100,000s of end points, 10,000s of switches. 

As such LOOM employs a distributed architecture with a unique data management scheme to store switch configurations and the network topology to address the dual challenges of massive scale and enterprise-class robustness and reliability.

In addition, LOOM supports third party applications. The LOOM design explores tactics for integrating these applications into LOOM’s distributed architecture while protecting the network from rogue applications. 

# Simple Network Executive

The simple network executive (simple_ne) is an exmaple implementation using the LOOM framework.  It accepts connections from switches and allows for interacting with the switches via the Erlang shell.  The simple network exucutive also collects open flow statistics from the switches and publishes the stats using folsom.

## Statistics Poller

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
