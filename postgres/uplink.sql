CREATE TABLE accounts (
  publicKey      bytea   UNIQUE NOT NULL,
  address        varchar PRIMARY KEY,
  timezone       bytea   NOT NULL,
  metadata       bytea   NOT NULL -- XXX is this the best repr? serialize as list of pairs of bystrings
);

------------------------------------------------------

CREATE TABLE assets (
  name          varchar NOT NULL,
  issuer        varchar NOT NULL,
  issuedOn      int8    NOT NULL,
  supply        int8    NOT NULL,
  reference     varchar,
  assetType     varchar NOT NULL,
  address       varchar PRIMARY KEY,
  metadata      bytea   NOT NULL,

  CONSTRAINT assets_supply_check CHECK (supply >= 0)
);

CREATE TABLE holdings (
  asset   varchar NOT NULL REFERENCES assets(address) ON DELETE CASCADE,
  holder  varchar NOT NULL, -- XXX REFERENCES accounts(address) ON DELETE CASCADE,
  balance int8    NOT NULL,

  CONSTRAINT PK_holdings PRIMARY KEY (asset,holder),
  CONSTRAINT positive_balance CHECK (balance >= 0)
);

CREATE INDEX holdings_asset ON holdings (asset); -- (?) necessary (?)

------------------------------------------------------

CREATE TABLE contracts (
  timestamp        int8    NOT NULL,
  script           bytea   NOT NULL,
  localStorageVars bytea   NOT NULL,
  methods          bytea   NOT NULL, -- is this the best repr (list of method names)
  state            varchar NOT NULL,
  owner            varchar NOT NULL, -- XXX REFERENCES accounts(address),
  address          varchar PRIMARY KEY,

  CONSTRAINT contracts_timestamp_check CHECK (timestamp > 0)
);

CREATE TABLE global_storage (
  contract varchar NOT NULL REFERENCES contracts(address) ON DELETE CASCADE,
  key      varchar NOT NULL,
  value    bytea   NOT NULL,

  CONSTRAINT PK_global_storage PRIMARY KEY (contract,key)
);

CREATE INDEX global_storage_var ON global_storage (contract, key);

CREATE TABLE local_storage (
  contract varchar NOT NULL REFERENCES contracts(address) ON DELETE CASCADE,
  account  varchar NOT NULL, -- XXX REFERENCES accounts(address),
  key      varchar NOT NULL,
  value    bytea   NOT NULL,
  
  CONSTRAINT PK_local_storage PRIMARY KEY (contract,account,key)
);

CREATE INDEX local_storage_var ON local_storage (contract, account, key);

------------------------------------------------------

CREATE TABLE blocks (
  idx          int8    PRIMARY KEY,
  origin       varchar NOT NULL,    -- XXX REFERENCES accounts(address),
  prevHash     bytea   NOT NULL,
  merkleRoot   bytea   NOT NULL,
  timestamp    int8    NOT NULL,
  consensus    bytea   NOT NULL,    -- binary encoding of consensus params
  signatures   bytea   NOT NULL,    -- XXX (?) binary encoding of list of block signatures

  CONSTRAINT blocks_timestamp_check CHECK (timestamp > 0)
);

CREATE TABLE transactions (
  block_idx  int8    NOT NULL REFERENCES blocks(idx) ON DELETE CASCADE,
  hash       varchar NOT NULL UNIQUE,
  tx_type    varchar NOT NUll, -- type of transaction (header) 
  header     bytea   NOT NULL, -- binary encoding of header
  signature  bytea   NOT NULL,
  origin     varchar NOT NULL, -- REFERENCES accounts(address) ON DELETE RESTRICT,

  CONSTRAINT PK_transactions PRIMARY KEY (hash), -- may be very slow :(
  CONSTRAINT transactions_positive_block_idx CHECK (block_idx > 0)
);

CREATE TABLE invalidtxs (
  hash       bytea   NOT NULL UNIQUE,
  header     bytea   NOT NULL, -- binary encoding of header
  signature  bytea   NOT NULL,
  origin     varchar NOT NULL, -- REFERENCES accounts(address) ON DELETE RESTRICT,
  reason     bytea   NOT NULL,

  CONSTRAINT PK_invalidtxs PRIMARY KEY (hash) -- may be very slow :(
);
