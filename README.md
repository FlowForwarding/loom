Loom
====

The purpose of the LOOM project is to design and prototype an experimental network switch controller that implements the OpenFlow 1.3.x and 1.4 protocols. The project explores the scalability and robustness of such controllers on a scale much larger than typically considered: 100,000s of end points, 10,000s of switches. 

As such LOOM employs a distributed architecture with a unique data management scheme to store switch configurations and the network topology to address the dual challenges of massive scale and enterprise-class robustness and reliability.

LOOM is a platform that supports third party applications. The LOOM design explores tactics for integrating these applications into LOOM’s distributed architecture while protecting the network from rogue applications. 

# Current Status

LOOM development is in its early stages and at this point provides an integration test bed for of_msg_lib, of_driver, and ofs_handler.

# Sample applications

* simple_ne - simple interactive controller intended for testing
* stats_poller - collects OpenFlow stats and publishes the stats as folsom gagues.
* tapestry - taps dns traffic to compute a network complexity index.
* icontrol - simple interactive controller with utility functions to configure and manipulate connected switches.

# Building

Run _make_ at the top level to build all sample applications.

To build a paritcular sample applications, cd into the subdir and run _make_.
