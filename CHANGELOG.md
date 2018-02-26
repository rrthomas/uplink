Changelog
========

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
