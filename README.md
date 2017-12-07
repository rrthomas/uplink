<p align="center">
  <a href="http://www.adjoint.io"><img src="packages/uplink.png" width="250"/></a>
</p>
<h3 align="center">Community Edition</h3>

Uplink
======

**Uplink** is a distributed database built for orchestrating secure multiparty
workflows found in financial markets. 

Uplink operates as a federation of nodes which communicate over a private
network to provide an append-only database in which all transactions are
cryptographically signed and where all members participate in a consensus
protocol to maintain and verify the ledger state. 

Uplink also has the capacity to run so-called *smart contracts* which are units
of logic which run across the network and allow counterparties to interact
through arbitrarily sophisticated programs that can model common business
processes and financial instruments. Provided with the ledger is a new scripting
language [FCL](src/Script) (**Financial Core Language**) built on a verified
core and designed as a target for contract modeling tools.

The community version of [Adjoint's](http://www.adjoint.io) software is released
under an Apache License and is part of a larger suite of tools and languages
offered commercially.

Install
-------

**Video Tutorial**

1. [Installing Uplink](https://www.youtube.com/watch?v=N1I_jd89Lr4&list=PLssH0Xui89Ewqpo1GytkslfMhqFNN8jmt&index=1)
2. [Setting up a Network](https://www.youtube.com/watch?v=k6RA8sLPRW0&index=2&list=PLssH0Xui89Ewqpo1GytkslfMhqFNN8jmt)
3. [Using the Uplink Console](https://www.youtube.com/watch?v=-e-S_NnXkP4&list=PLssH0Xui89Ewqpo1GytkslfMhqFNN8jmt&index=3)
4. [Installing Uplink Explorer](https://www.youtube.com/watch?v=VeWkuNf83Kw&list=PLssH0Xui89Ewqpo1GytkslfMhqFNN8jmt&index=4)
5. [Importing Keys into Explorer](https://www.youtube.com/watch?v=5e9qpNu_ayU&index=5&list=PLssH0Xui89Ewqpo1GytkslfMhqFNN8jmt)

**Install Directions**

Install the ledger either through Docker or your system package manager.

* [Docker](https://www.adjoint.io/pages/downloads.html#docker) (recommended)
* [RedHat](https://www.adjoint.io/pages/downloads.html#redhat)
* [Debian](https://www.adjoint.io/pages/downloads.html#debian)
* [MacOS](https://www.adjoint.io/pages/downloads.html#mac)
* [Arch](https://www.adjoint.io/pages/downloads.html#arch)

To build using docker simply run:

```bash
$ docker run -it -p 8000:8000 --rm  uplinkdlt/uplink:latest
```

To build from source (not recommended) use:

```bash
$ git clone  git@github.com:adjoint-io/uplink.git
$ cd uplink
$ stack install --no-docker
```

Running a Node
--------------

To get started running a testnet download the testnet starter kit and unzip the
folder locally:

```bash
$ wget https://www.adjoint.io/release/config-1.0.zip
$ unzip config-1.0.zip -d config
```

This contains two files the node configuration and genesis block configuration
used to configure a private network:

* [node.config](config/node.config)
* [chain.config](config/chain.config)

To run an Uplink node that has the ability to construct and sign blocks, you
must supply  an existing private key located in the `config/validators`
directory. Accounts corresponding to these key pairs are created on boot and
added to the genesis world state, and the list of addresses defining the
validator nodes is defined in `config/chain.config.local`.

To boot a validator node (needed to create & sign blocks): 

```bash
$ uplink chain -p 8001 -d node1 -k config/validators/auth0/key -v
```

And run two non-validator nodes to start the consensus:

```bash
$ uplink chain -p 8002 -d node2 -v 
$ uplink chain -p 8003 -d node3 -v
```

Documentation
-------------

Extensive documentation can be found [here](https://www.adjoint.io/docs).

License
-------

Uplink is released under the Apache 2.0 License.
