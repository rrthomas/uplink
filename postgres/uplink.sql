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
  reference     int2,
  assetType     int2    NOT NULL,
  assetTypePrec int2,             -- If assetType is Fractiional
  address varchar PRIMARY KEY,

  CONSTRAINT assets_reference_check CHECK (reference >= 0),
  CONSTRAINT assets_supply_check CHECK (supply > 0),
  CONSTRAINT assets_assetType_check CHECK (assetType >= 0)
);

CREATE TABLE holdings (
  id      serial  PRIMARY KEY,
  asset   varchar NOT NULL REFERENCES assets(address) ON DELETE CASCADE,
  holder  varchar NOT NULL, -- XXX REFERENCES accounts(address) ON DELETE CASCADE,
  balance int8    NOT NULL,

  CONSTRAINT positive_balance CHECK (balance >= 0)
);

CREATE INDEX holdings_asset ON holdings (asset); -- (?) necessary (?)
CREATE INDEX holdings_asset_holder ON holdings (asset, holder);

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
  id           serial  PRIMARY KEY,
  contractAddr varchar NOT NULL REFERENCES contracts(address) ON DELETE CASCADE,
  key          bytea   NOT NULL,
  value        bytea   NOT NULL
);

CREATE INDEX global_storage_var ON global_storage (contractAddr, key);

CREATE TABLE local_storage (
  id           serial  PRIMARY KEY,
  contractAddr varchar NOT NULL REFERENCES contracts(address) ON DELETE CASCADE,
  accountAddr  varchar NOT NULL, -- XXX REFERENCES accounts(address),
  key          bytea   NOT NULL,
  value        bytea   NOT NULL
);

CREATE INDEX local_storage_var ON local_storage (contractAddr, accountAddr, key);

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
  idx        serial PRIMARY KEY,
  block_idx  int8    NOT NULL REFERENCES blocks(idx) ON DELETE CASCADE,
  hash       bytea   NOT NULL UNIQUE,
  header     bytea   NOT NULL, -- binary encoding of header
  signature  bytea   NOT NULL,
  origin     varchar NOT NULL, -- REFERENCES accounts(address) ON DELETE RESTRICT,
  timestamp  int8    NOT NULL,

  CONSTRAINT transactions_positive_timestamp CHECK (timestamp > 0),
  CONSTRAINT transactions_positive_block_idx CHECK (block_idx > 0)
);

CREATE TABLE invalidtxs (
  idx        serial PRIMARY KEY,
  hash       bytea   NOT NULL UNIQUE,
  header     bytea   NOT NULL, -- binary encoding of header
  signature  bytea   NOT NULL,
  origin     varchar NOT NULL REFERENCES accounts(address) ON DELETE RESTRICT,
  timestamp  int8    NOT NULL,
  reason     bytea   NOT NULL,

  CONSTRAINT invalidtxs_positive_timestamp CHECK (timestamp > 0)
);
