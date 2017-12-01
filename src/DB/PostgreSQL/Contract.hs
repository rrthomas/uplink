{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DB.PostgreSQL.Contract (
  ContractRow(..),

  contractToRowTypes,
  rowTypesToContract,

  queryContract,
  queryContracts,

  insertContract,
  insertContracts,

  deleteContract,
  deleteContracts,
) where

import Protolude

import Control.Arrow ((&&&))

import Data.Int as Int
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Serialize as S

import Address
import Contract (Contract(..), LocalStorageVars(..))
import Storage (GlobalStorage(..), LocalStorage(..))
import Script (Script, Name, Value)
import Script.Graph (GraphState(..), Label(..), terminalLabel, initialLabel)
import qualified Contract
import qualified Storage
import qualified Time

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow

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
  , contractOwner   :: Address
  , contractAddress :: Address
  } deriving (Generic)

instance ToRow ContractRow
instance FromRow ContractRow

data GlobalStorageRow = GlobalStorageRow
  { gsContractAddr :: Address
  , gsKey          :: ByteString
  , gsValue        :: Value
  } deriving (Generic)

instance ToRow GlobalStorageRow
instance FromRow GlobalStorageRow

data LocalStorageRow = LocalStorageRow
  { lsContractAddr :: Address
  , lsAccountAddr  :: Address
  , lsKey          :: ByteString
  , lsValue        :: Value
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
        mkGSArgs = first Storage.unKey
        mkGSRow  = uncurry (GlobalStorageRow address) . mkGSArgs

    lsRows =
        concatMap (map mkLSRow . mkLSArgs) $
          Map.toList $ localStorage
      where
        mkLSArgs (accAddr, ls) =
          flip map (Map.toList $ unLocalStorage ls) $ \(key, val) ->
            (accAddr, Storage.unKey key, val)

        mkLSRow (accAddr, key, val) =
          LocalStorageRow address accAddr key val

rowTypesToContract
  :: (ContractRow, [GlobalStorageRow], [LocalStorageRow])
  -> Either Text Contract
rowTypesToContract (contractRow, gsRows, lsRows) = do
    pure $ Contract
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
        map ((Storage.Key . gsKey) &&& gsValue) gsRows

    -- create local storage from local storage rows
    localStorages = insertLSEntries mempty lsRows
      where
        -- Try to insert all entries, shortcircuit fail on failed decode
        insertLSEntries lsMap []         = lsMap
        insertLSEntries lsMap (lsr:lsrs) = do
          let (LocalStorageRow _ accAddr key' val') = lsr
          let (key, val) = (Storage.Key key', val')
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
  -> Address
  -> IO (Either Text Contract)
queryContract conn contractAddr = do
  mContractRow <- queryContractRow conn contractAddr
  case mContractRow of
    Nothing -> pure $ Left $ Text.intercalate " "
      [ "PostgreSQL: Contract with addr"
      , toS (rawAddr contractAddr)
      , "does not exist."
      ]
    Just contractRow -> do
      gsRows <- queryGlobalStorageRows conn contractAddr
      lsRows <- queryLocalStorageRows conn contractAddr
      pure $ rowTypesToContract (contractRow, gsRows, lsRows)

queryContracts :: Connection -> IO (Either Text [Contract])
queryContracts conn = do
  contractRows <- queryContractRows conn
  fmap sequence $
    forM contractRows $ \contractRow -> do
      let contractAddr = contractAddress contractRow
      gsRows <- queryGlobalStorageRows conn contractAddr
      lsRows <- queryLocalStorageRows conn contractAddr
      pure $ rowTypesToContract (contractRow, gsRows, lsRows)

--------------------------------------------------------------------------------

queryContractRows
  :: Connection
  -> IO [ContractRow]
queryContractRows conn =
  query_ conn "SELECT * FROM contracts"

queryContractRow
  :: Connection
  -> Address
  -> IO (Maybe ContractRow)
queryContractRow conn contractAddr = headMay <$>
  query conn "SELECT * FROM contracts where address=?" (Only contractAddr)

queryGlobalStorageRows
  :: Connection
  -> Address
  -> IO [GlobalStorageRow]
queryGlobalStorageRows conn contractAddr =
  query conn
    "SELECT contractAddr,key,value FROM global_storage WHERE contractAddr=?"
    (Only contractAddr)

queryLocalStorageRows
  :: Connection
  -> Address
  -> IO [LocalStorageRow]
queryLocalStorageRows conn contractAddr =
  query conn
    "SELECT contractAddr,accountAddr,key,value FROM local_storage WHERE contractAddr=?"
    (Only contractAddr)

--------------------------------------------------------------------------------
-- Inserts
--------------------------------------------------------------------------------

insertContracts :: Connection -> [Contract] -> IO ()
insertContracts conn contracts =
  mapM_ (insertContract conn) contracts

-- | Insert a Contract into a PostgreSQL DB
-- Note: You may think it is best to use a Postgres Transaction here, but
-- contracts should only be written to the database along with the rest of the
-- world state (accounts & assets). Therefore, a transaction is created when
-- writing the entire ledger to the database, instead of here.
insertContract :: Connection -> Contract -> IO ()
insertContract conn contract = do
    insertContractRow conn contractRow
    insertGlobalStorageRows conn gsRows
    insertLocalStorageRows conn lsRows
  where
    (contractRow, gsRows, lsRows) = contractToRowTypes contract

insertContractRow :: Connection -> ContractRow -> IO ()
insertContractRow conn contractRow = void $
  execute conn "INSERT INTO contracts VALUES (?,?,?,?,?,?,?)" contractRow

insertGlobalStorageRows :: Connection -> [GlobalStorageRow] -> IO ()
insertGlobalStorageRows conn gsRows = void $
  executeMany conn "INSERT INTO global_storage (contractAddr,key,value) VALUES (?,?,?)" gsRows

insertLocalStorageRows :: Connection -> [LocalStorageRow] -> IO ()
insertLocalStorageRows conn lsRows = void $
  executeMany conn "INSERT INTO local_storage (contractAddr,accountAddr,key,value) VALUES (?,?,?,?)" lsRows

--------------------------------------------------------------------------------
-- Deletes
--------------------------------------------------------------------------------

deleteContract :: Connection -> Address -> IO ()
deleteContract conn addr = void $ do
  execute conn "DELETE FROM contracts WHERE address = ?" (Only addr)
  execute conn "DELETE FROM local_storage WHERE contractAddr = ?" (Only addr)
  execute conn "DELETE FROM global_storage WHERE contractAddr = ?" (Only addr)

deleteContracts :: Connection -> IO ()
deleteContracts conn = void $ do
  execute_ conn "DELETE FROM contracts"
  execute_ conn "DELETE FROM local_storage"
  execute_ conn "DELETE FROM global_storage"
