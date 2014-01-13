Loom
====

The purpose of the LOOM project is to design and prototype an experimental network switch controller that implements the OpenFlow 1.3.x and 1.4 protocols. The project explores the scalability and robustness of such controllers on a scale much larger than typically considered: 100,000s of end points, 10,000s of switches. 

As such LOOM employs a distributed architecture with a unique data management scheme to store switch configurations and the network topology to address the dual challenges of massive scale and enterprise-class robustness and reliability.

In addition, LOOM supports third party applications. The LOOM design explores tactics for integrating these applications into LOOM’s distributed architecture while protecting the network from rogue applications. 
