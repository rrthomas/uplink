Changelog
========

1.0
---

Initial release.

1.1
---

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
