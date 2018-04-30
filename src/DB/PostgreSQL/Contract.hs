{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module DB.PostgreSQL.Contract (
  ContractRow(..),
  GlobalStorageRow(..),
  LocalStorageRow(..),
  MethodsCol,

  contractToRowTypes,
  rowTypesToContract,

  queryContract,
  queryContracts,
  queryContractsByAddrs,

  contractRowToContract,
  contractRowsToContracts,

  insertContract,
  insertContracts,

  deleteContract,
  deleteContracts,
) where

import Protolude

import Control.Arrow ((&&&))

import qualified Data.Map as Map
import qualified Data.Serialize as S

import Address
import Contract (Contract(..), LocalStorageVars(..))
import Storage (GlobalStorage(..), LocalStorage(..), Key(..))
import Script (Script, Name, Value)
import Script.Graph (GraphState(..), Label(..), terminalLabel, initialLabel)

import DB.PostgreSQL.Error

import Database.PostgreSQL.Simple

import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField

--------------------------------------------------------------------------------
-- Types and Conversions
--------------------------------------------------------------------------------

-- | Defined because Data.Serialize.encode-ing the list of methods and then
-- writing to columns in the DB does not yield a correctly byte encoding of the
-- list of methods when reading from the DB and calling Data.Serialize.decode.
newtype MethodsCol = MethodsCol
  { unMethodsCol :: [ Name ] }
  deriving (Eq, Ord, Generic)

instance ToField MethodsCol where
  toField = EscapeByteA . S.encode . unMethodsCol

instance FromField MethodsCol where
  fromField f mdata = do
    bs <- fromField f mdata
    case S.decode <$> bs of
      Nothing           -> returnError UnexpectedNull f ""
      Just (Left err)   -> returnError ConversionFailed f err
      Just (Right mcol) -> return $ MethodsCol mcol

data ContractRow = ContractRow
  { contractTs      :: Int64
  , contractScript  :: Script
  , contractLSVars  :: LocalStorageVars
  , contractMethods :: MethodsCol
  , contractState   :: Text
  , contractOwner   :: (Address AAccount)
  , contractAddress :: (Address AContract)
  } deriving (Generic)

instance ToRow ContractRow
instance FromRow ContractRow

data GlobalStorageRow = GlobalStorageRow
  { gsContract :: Address AContract
  , gsKey      :: Key
  , gsValue    :: Value
  } deriving (Generic)

instance ToRow GlobalStorageRow
instance FromRow GlobalStorageRow

data LocalStorageRow = LocalStorageRow
  { lsContract :: Address AContract
  , lsAccount  :: Address AAccount
  , lsKey      :: Key
  , lsValue    :: Value
  } deriving (Generic)

instance ToRow LocalStorageRow
instance FromRow LocalStorageRow

contractToRowTypes
  :: Contract
  -> (ContractRow, [GlobalStorageRow], [LocalStorageRow])
contractToRowTypes Contract{..} =
    (contractRow, gsRows, lsRows)
  where
    state' =
      case state of
        GraphInitial     -> initialLabel
        GraphTerminal    -> terminalLabel
        GraphLabel label -> label

    contractRow = ContractRow
      { contractTs      = timestamp
      , contractScript  = script
      , contractLSVars  = localStorageVars
      , contractMethods = MethodsCol methods
      , contractState   = unLabel state'
      , contractOwner   = owner
      , contractAddress = address
      }

    gsRows =
        map mkGSRow $ Map.toList $
          unGlobalStorage globalStorage
      where
        mkGSRow  = uncurry (GlobalStorageRow address)

    lsRows =
        concatMap (map mkLSRow . mkLSArgs) $
          Map.toList $ localStorage
      where
        mkLSArgs (accAddr, ls) =
          flip map (Map.toList $ unLocalStorage ls) $ \(key, val) ->
            (accAddr, key, val)

        mkLSRow (accAddr, key, val) =
          LocalStorageRow address accAddr key val

rowTypesToContract
  :: (ContractRow, [GlobalStorageRow], [LocalStorageRow])
  -> Contract
rowTypesToContract (contractRow, gsRows, lsRows) = do
    Contract
      { timestamp        = contractTs contractRow
      , script           = contractScript contractRow
      , globalStorage    = globalStorage
      , localStorage     = localStorages
      , localStorageVars = contractLSVars contractRow
      , methods          = unMethodsCol $ contractMethods contractRow
      , state            = state'
      , owner            = contractOwner contractRow
      , address          = contractAddress contractRow
      }
  where
    -- create global storage from global storage rows
    globalStorage :: GlobalStorage
    globalStorage =
      GlobalStorage . Map.fromList $
        map (gsKey &&& gsValue) gsRows

    -- create local storage from local storage rows
    localStorages = insertLSEntries mempty lsRows
      where
        -- Try to insert all entries, shortcircuit fail on failed decode
        insertLSEntries lsMap []         = lsMap
        insertLSEntries lsMap (lsr:lsrs) = do
          let (LocalStorageRow _ accAddr key val) = lsr
          let insertVar  = Map.alter (insertLSVar key val) accAddr
          insertLSEntries (insertVar $ lsMap) lsrs

        -- if local storage doesn't exist, create it and insert
        -- otherwise insert var/val into the existing localstorage
        insertLSVar k v Nothing =
          Just $ LocalStorage $ Map.singleton k v
        insertLSVar k v (Just ls) =
          Just $ LocalStorage $
            Map.insert k v $ unLocalStorage ls

    -- decode graph label
    contractState' = Label $ contractState contractRow
    state'
      | contractState' == initialLabel  = GraphInitial
      | contractState' == terminalLabel = GraphTerminal
      | otherwise = GraphLabel contractState'

--------------------------------------------------------------------------------
-- Queries (SELECTs)
--------------------------------------------------------------------------------

queryContract
  :: Connection
  -> Address AContract
  -> IO (Either PostgreSQLError Contract)
queryContract conn contractAddr = do
  eContractRow <- queryContractRow conn contractAddr
  case eContractRow of
    Left err                 -> pure $ Left err
    Right Nothing            -> pure $ Left $ ContractDoesNotExist contractAddr
    Right (Just contractRow) -> contractRowToContract conn contractRow

queryContracts
  :: Connection
  -> IO (Either PostgreSQLError [Contract])
queryContracts conn = do
  queryContractRows conn >>=
    either (pure . Left) (contractRowsToContracts conn)

queryContractsByAddrs
  :: Connection
  -> [Address AContract]
  -> IO (Either PostgreSQLError [Contract])
queryContractsByAddrs conn addrs = do
  queryContractRowsByAddrs conn addrs >>=
    either (pure . Left) (contractRowsToContracts conn)

contractRowToContract
  :: Connection
  -> ContractRow
  -> IO (Either PostgreSQLError Contract)
contractRowToContract conn contractRow = do
  let contractAddr = contractAddress contractRow
  eGsRows <- queryGlobalStorageRows conn contractAddr
  eLsRows <- queryLocalStorageRows conn contractAddr
  pure $ fmap rowTypesToContract $
    (contractRow,,) <$> eGsRows <*> eLsRows

contractRowsToContracts
  :: Connection
  -> [ContractRow]
  -> IO (Either PostgreSQLError [Contract])
contractRowsToContracts conn =
  fmap sequence . mapM (contractRowToContract conn)

--------------------------------------------------------------------------------

queryContractRows
  :: Connection
  -> IO (Either PostgreSQLError [ContractRow])
queryContractRows conn =
  querySafe_ conn "SELECT * FROM contracts"

queryContractRowsByAddrs
  :: Connection
  -> [Address AContract]
  -> IO (Either PostgreSQLError [ContractRow])
queryContractRowsByAddrs conn addrs = do
  querySafe conn "SELECT * FROM contracts WHERE address IN ?" $
    Only $ In addrs

queryContractRow
  :: Connection
  -> Address AContract
  -> IO (Either PostgreSQLError (Maybe ContractRow))
queryContractRow conn contractAddr = fmap headMay <$>
  querySafe conn "SELECT * FROM contracts where address=?" (Only contractAddr)

queryGlobalStorageRows
  :: Connection
  -> Address AContract
  -> IO (Either PostgreSQLError [GlobalStorageRow])
queryGlobalStorageRows conn contractAddr =
  querySafe conn
    "SELECT * FROM global_storage WHERE contract=?"
    (Only contractAddr)

queryLocalStorageRows
  :: Connection
  -> Address AContract
  -> IO (Either PostgreSQLError [LocalStorageRow])
queryLocalStorageRows conn contractAddr =
  querySafe conn
    "SELECT * FROM local_storage WHERE contract=?"
    (Only contractAddr)

--------------------------------------------------------------------------------
-- Inserts
--------------------------------------------------------------------------------

insertContracts :: Connection -> [Contract] -> IO (Either PostgreSQLError Int64)
insertContracts conn contracts =
  second sum . sequence <$> mapM (insertContract conn) contracts

-- | Insert a Contract into a PostgreSQL DB
-- Note: You may think it is best to use a Postgres Transaction here, but
-- contracts should only be written to the database along with the rest of the
-- world state (accounts & assets). Therefore, a transaction is created when
-- writing the entire ledger to the database, instead of here.
insertContract :: Connection -> Contract -> IO (Either PostgreSQLError Int64)
insertContract conn contract = do
    _ <- insertContractRow conn contractRow
    _ <- insertGlobalStorageRows conn gsRows
    insertLocalStorageRows conn lsRows
  where
    (contractRow, gsRows, lsRows) = contractToRowTypes contract

insertContractRow :: Connection -> ContractRow -> IO (Either PostgreSQLError Int64)
insertContractRow conn contractRow =
  executeSafe conn "INSERT INTO contracts VALUES (?,?,?,?,?,?,?)" contractRow

insertGlobalStorageRows :: Connection -> [GlobalStorageRow] -> IO (Either PostgreSQLError Int64)
insertGlobalStorageRows conn gsRows =
  executeManySafe conn "INSERT INTO global_storage VALUES (?,?,?)" gsRows

insertLocalStorageRows :: Connection -> [LocalStorageRow] -> IO (Either PostgreSQLError Int64)
insertLocalStorageRows conn lsRows =
  executeManySafe conn "INSERT INTO local_storage VALUES (?,?,?,?)" lsRows

--------------------------------------------------------------------------------
-- Deletes
--------------------------------------------------------------------------------

deleteContract :: Connection -> Address AContract -> IO (Either PostgreSQLError Int64)
deleteContract conn addr = do
  _ <- executeSafe conn "DELETE FROM contracts WHERE address = ?" (Only addr)
  _ <- executeSafe conn "DELETE FROM local_storage WHERE contract = ?" (Only addr)
  executeSafe conn "DELETE FROM global_storage WHERE contract = ?" (Only addr)

deleteContracts :: Connection -> IO (Either PostgreSQLError Int64)
deleteContracts conn = do
  _ <- executeSafe_ conn "DELETE FROM contracts"
  _ <- executeSafe_ conn "DELETE FROM local_storage"
  executeSafe_ conn "DELETE FROM global_storage"
