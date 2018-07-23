Changelog
========

1.7
---

*June 20, 2018*

* Add support for TLS on top of TCP transport layer
* Make transport layer configurable: TCP, TCP/TLS, or in-memory 
* Add more robust supervision and recovery of Uplink Processes
* FCL: Added Helper functions 
* FCL: Disallow global variable name shadowing
* FCL: Added generic collection values and higher-order primops operating over
  collections
* Bug fixes

1.6
---

*April 20, 2018*

* Simplify internal Node-to-Node messaging protocol by deriving the serialzation
  using GHC.Generics.
* Fix hostname resolution on all supported operating systems.
* All superfluous ByteString usage is removed.
* Added the EncodedByteString typeclass and Base16, Base58, Base64, and Base64P
  ByteString types to allow for typesafe ByteString encoding representations 
  throughout the codebase.
* Added network-access-token such that all messages exchanged by nodes in the
  Uplink network must contain proof of possession of the network-access token;
  Uplink nodes that do not possess the preshared, private network-access-token 
  will not be able to interact with an Uplink network in which membership is 
  defined by the network-access-token.
* All messages sent by Uplink nodes are now signed by the network-access-token
  private key, and all message received by Uplink nodes are verified to be from
  a node that possess the shared, private network-access-token.

1.5
---

*March 26, 2018*

* FIX: Initializing a node using `uplink chain init` with a LevelDB backend now
  (correctly) fails when a LevelDB database already exists at the supplied
  database directory.
* FIX: Exporting Ledger and Blocks as XML using a LevelDB database path now
  (correctly) requires an existing database and does not instead create an empty
  database and export nothing.
* Users can now export Uplink blocks and/or ledger state as JSON. 
* Users can now import Uplink blocks and/or ledger state when initializing an
  Uplink node with `uplink chain init`.
* Added Simulation Msg to allow users to query contract simulation ledger state.
* Addresses for Assets and Contracts are now derived by base58 encoding the hash
  of the hash of their origin transaction.
* Timestamp fields are removed from transactions due to the fact that
  transaction ordering in a block and block timestamps carry more information
  about when the transacation modified the ledger.
* Transaction uniqueness is now enforced across the entire history of the ledger
  using the sha3_256 hash of transaction datastructure contents.
* Rewrote REPL to use Simulation Process. Added option to specify exported
  ledger state to simulate the contract in. Added tab completion on methods
  callable in current state.

1.4
---

*March 13, 2018*

* Top-level declarations may now have definitions that are arbitrary
  expressions (without side-effects).
* Effect checking: we can now check and show what side-effects
  expressions or methods have.
* Fix timeout issues in Federated Consensus Algorithm. Block period is 
  now adhered to properly within the block generation process.

1.3
---

*February 26, 2018*

* Added metadata field of assets.
* Add support for enumeration types in FCL.
* References to uninitialized global variables are now rejected at 
  compile-time
* Improved graph analysis on FCL contracts.
* Fix json serialization of datetime values in contract storage
* Change FCL asset type to explicitly reference 8 distinct asset types 
* Add polymorphic asset FCL prim ops that operate over all asset types
* Added FCL Smart Contract simulation feature
* Made logging more configurable: different services can log to
  different files.
* Removed superfluous "return" construct from FCL
	
1.2
---

*January 26, 2018*

* Fix database initialization bugs on corner cases.
* Removed necessity for a Postgres database to exist with the same name of the 
  Postgres user creating the uplink database.
* Consolidated Node data file reads & writes into a single module.
* Added data query language for querying ledger state.
* Added data query interface to RPC for doing cross contract/position
  aggregations for reporting.
* Bazel build scripts for continuous integration.

1.1
---

*December 5, 2017*

* Added PostgreSQL persistence backend.
* Added Transaction "graveyard'" for transactions for reporting on transactions
  that fail to commit.
* Added database reset mechanism in test mode for provisioning test
  environments.
* Added `Circulate` transaction for assets.
* Added `RevokeAsset` transaction for assets.
* Added protocol for swappable data persistence, supporting LevelDB and SQL
  backends.
* Improved P2P performance using new asynchronous messaging handling. This will
  yield better performance when the network scales by number of nodes and
  geographical sparsity.
* Fixed issues with database and log filepath locations relative to
  configuration.
* Remove ``nodekey`` from accounts.
* Fix Mac OSX filepath bugs.

1.0
---

*October 25, 2017*

Initial release.
