{-|

Builtin operations for FCL evaluation. Operations that interact with
cryptographic primitives, network status, and ledger state.

-}

{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Script.Prim (
  -- ** Data types
  PrimOp(..),
  AssetPrimOp(..),
  MapPrimOp(..),
  SetPrimOp(..),
  CollPrimOp(..),

  -- ** Mappings
  arity,
  primName,
  lookupPrim
) where

import Protolude hiding (Hashable)
import Control.Arrow ((&&&))

import qualified Data.List
import Data.Serialize (Serialize)

import Hash (Hashable)
import Script.Pretty (Pretty(..))

data PrimOp
  = Verify              -- ^ @verify(addr,sig,msg)@                           : Verify a signature
  | Sign                -- ^ @sign(msg)@                                      : Sign a message
  | Block               -- ^ @block()@                                        : Active block
  | Deployer            -- ^ @deployer()@                                     : Deployer of contract
  | Sender              -- ^ @sender()@                                       : Transaction caller
  | Created             -- ^ @created()@                                      : Time of contract creation
  | Address             -- ^ @address()@                                      : Address of contract
  | Validator           -- ^ @validator()@                                    : Account address of validator
  | Sha256              -- ^ @sha256(any)@                                    : SHA256 digest of any data type turned into a string
  | AccountExists       -- ^ @accountExists(addr)@                            : Check if account exists in world state
  | AssetExists         -- ^ @assetExists(addr)@                              : Check if asset exists in world state
  | ContractExists      -- ^ @contractExists(addr)@                           : Check if contract exists in world state
  | Terminate           -- ^ @terminate(msg)@                                 : Terminate contract execution
  | Now                 -- ^ @now()@                                          : Current date + time in UTC
  | Transition          -- ^ @transitionTo(msg)@                              : Transition to state msg
  | CurrentState        -- ^ @state()@                                        : Transition to state msg
  | TxHash              -- ^ @txHash()@                                       : Transaction hash
  | Bound               -- ^ @bound(addr,addr)@                               : Check binding status
  | ContractValueExists -- ^ @contractValueExists(addr,varName)@              : Query a value's existence in a contract's global storage
  | ContractState       -- ^ @contractState(addr)@                            : Query the state of a smart contract
  | NovationInit        -- ^ @novationInit(int)@                              : Start novation side logic
  | NovationStop        -- ^ @novationStop()@                                 : Start novation side logic
  | IsBusinessDayUK     -- ^ @isBusinessDayUK(datetime)@                      : Predicate checking if datetime is a business day or not
  | NextBusinessDayUK   -- ^ @nextBusinessDayUK(datetime)@                    : Returns the next business day after the supplied datetime
  | IsBusinessDayNYSE   -- ^ @isBusinessDayNYSE(datetime)@                    : Predicate checking if datetime is a business day or not
  | NextBusinessDayNYSE -- ^ @nextBusinessDayNYSE(datetime)@                  : Returns the next business day after the supplied datetime
  | Between             -- ^ @between(datetime,datetime,datetime)@            : Returns (True/False) if the first datetime is within the latter two
  | TimeDiff            -- ^ @timeDiff(datetime,datetime)@                    : Returns the differences in time between two datetimes
  | Fixed1ToFloat       -- ^ @fixed1ToFloat(fixed1)@                          : Coerce a fixed point number into a floating point number
  | Fixed2ToFloat       -- ^ @fixed2ToFloat(fixed2)@                          : Coerce a fixed point number into a floating point number
  | Fixed3ToFloat       -- ^ @fixed3ToFloat(fixed3)@                          : Coerce a fixed point number into a floating point number
  | Fixed4ToFloat       -- ^ @fixed4ToFloat(fixed4)@                          : Coerce a fixed point number into a floating point number
  | Fixed5ToFloat       -- ^ @fixed5ToFloat(fixed5)@                          : Coerce a fixed point number into a floating point number
  | Fixed6ToFloat       -- ^ @fixed6ToFloat(fixed6)@                          : Coerce a fixed point number into a floating point number
  | FloatToFixed1       -- ^ @floatToFixed1(float)@                           : Coerce a floating point number into a fixed point number
  | FloatToFixed2       -- ^ @floatToFixed2(float)@                           : Coerce a floating point number into a fixed point number
  | FloatToFixed3       -- ^ @floatToFixed3(float)@                           : Coerce a floating point number into a fixed point number
  | FloatToFixed4       -- ^ @floatToFixed4(float)@                           : Coerce a floating point number into a fixed point number
  | FloatToFixed5       -- ^ @floatToFixed5(float)@                           : Coerce a floating point number into a fixed point number
  | FloatToFixed6       -- ^ @floatToFixed6(float)@                           : Coerce a floating point number into a fixed point number
  | ContractValue       -- ^ @contractValue(addr,varName)@                    : Query a value in the contract's global storage
  | AssetPrimOp AssetPrimOp
  | MapPrimOp MapPrimOp
  | SetPrimOp SetPrimOp
  | CollPrimOp CollPrimOp
  deriving (Eq, Show, Generic, Ord, Hashable, Serialize, NFData)

-- | These prim ops are "polymorphic" in the sense that their argument or return
-- types vary based on the type of asset that is passed as an argument
data AssetPrimOp
  = HolderBalance       -- ^ @holderBalance(asset,account)@
  | TransferTo          -- ^ @transferTo(asset,amount)@                       : Transfer n asset holdings to contract
  | TransferFrom        -- ^ @transferFrom(asset,amount,acc)@                 : Transfer n asset holdings from contract to account
  | CirculateSupply     -- ^ @circulate(asset,amount)@                        : Circulate n asset supply to issuer's holdings
  | TransferHoldings    -- ^ @transferHoldings(from,asset,amount,to)@         : Transfer asset holdings from account to account
  deriving (Eq, Show, Generic, Ord, Hashable, Serialize, NFData)

-- | These primops are polymorphic over the key and value type parameters of the
-- map supplied as an argument.
data MapPrimOp
  = MapInsert  -- ^ @mapInsert(key, value, map)@     : Insert an element into a map
  | MapDelete  -- ^ @mapDelete(key, map)@            : Delete an element from map. If the element doesn't exist, an error occurs.
  | MapLookup  -- ^ @lookup(key, map)@            : Lookup an element from a map
  | MapModify  -- ^ @modify(key, f, map)@         : Modify an element in the map
  deriving (Eq, Show, Generic, Ord, Enum, Bounded, Hashable, Serialize,  NFData)

data SetPrimOp
  = SetInsert -- ^ @setInsert(value, set)@ : Insert an element into a set. If the element already exists, the original set is returned.
  | SetDelete -- ^ @setDelete(value, set)@ : Delete an element from a set. If the element does not exist, and error occurs.
  deriving (Eq, Show, Generic, Ord, Enum, Bounded, Hashable, Serialize,  NFData)

data CollPrimOp
  = Aggregate  -- ^ @aggregate(v, f, coll)@   : Fold over a collection, accumulating a resulting value with 'f' using 'v' as the initial value
  | Transform  -- ^ @transform(f, coll)@      : Map over a collection, producing a new collection by applying 'f' to all values in the original
  | Filter     -- ^ @filter(p, coll)@         : Produce a new collection by removing the values that do not satisfy the supplied predicate
  | Element    -- ^ @element(v, coll)@        : Checks for membership of a value to a collection
  | IsEmpty    -- ^ @isEmpty(coll)@           : Checks if the given collection is empty or not
  deriving (Eq, Show, Generic, Ord, Enum, Bounded, Hashable, Serialize,  NFData)

{-# INLINE primName #-}
primName :: IsString s => PrimOp -> s
primName = \case
  Verify              -> "verify"
  Sign                -> "sign"
  Block               -> "block"
  Deployer            -> "deployer"
  Sender              -> "sender"
  Created             -> "created"
  Address             -> "address"
  Validator           -> "validator"
  Sha256              -> "sha256"
  AccountExists       -> "accountExists"
  AssetExists         -> "assetExists"
  ContractExists      -> "contractExists"
  Terminate           -> "terminate"
  Now                 -> "now"
  Transition          -> "transitionTo"
  CurrentState        -> "state"
  TxHash              -> "txHash"
  Bound               -> "bound"
  ContractValue       -> "contractValue"
  ContractValueExists -> "contractValueExists"
  ContractState       -> "contractState"
  NovationInit        -> "novationInit"
  NovationStop        -> "novationStop"
  IsBusinessDayUK     -> "isBusinessDayUK"
  NextBusinessDayUK   -> "nextBusinessDayUK"
  IsBusinessDayNYSE   -> "isBusinessDayNYSE"
  NextBusinessDayNYSE -> "nextBusinessDayNYSE"
  Between             -> "isBetween"
  TimeDiff            -> "timeDiff"
  Fixed1ToFloat       -> "fixed1ToFloat"
  Fixed2ToFloat       -> "fixed2ToFloat"
  Fixed3ToFloat       -> "fixed3ToFloat"
  Fixed4ToFloat       -> "fixed4ToFloat"
  Fixed5ToFloat       -> "fixed5ToFloat"
  Fixed6ToFloat       -> "fixed6ToFloat"
  FloatToFixed1       -> "floatToFixed1"
  FloatToFixed2       -> "floatToFixed2"
  FloatToFixed3       -> "floatToFixed3"
  FloatToFixed4       -> "floatToFixed4"
  FloatToFixed5       -> "floatToFixed5"
  FloatToFixed6       -> "floatToFixed6"
  AssetPrimOp a       -> assetPrimName a
  MapPrimOp m         -> mapPrimName m
  SetPrimOp m         -> setPrimName m
  CollPrimOp c        -> collPrimName c

assetPrimName :: IsString s => AssetPrimOp -> s
assetPrimName = \case
  TransferTo          -> "transferTo"
  TransferFrom        -> "transferFrom"
  CirculateSupply     -> "circulate"
  TransferHoldings    -> "transferHoldings"
  HolderBalance       -> "holderBalance"

mapPrimName :: IsString s => MapPrimOp -> s
mapPrimName = \case
  MapInsert -> "mapInsert"
  MapDelete -> "mapDelete"
  MapLookup -> "lookup"
  MapModify -> "modify"

setPrimName :: IsString s => SetPrimOp -> s
setPrimName = \case
  SetInsert -> "setInsert"
  SetDelete -> "setDelete"

collPrimName :: IsString s => CollPrimOp -> s
collPrimName = \case
  Aggregate -> "aggregate"
  Transform -> "transform"
  Filter    -> "filter"
  Element   -> "element"
  IsEmpty   -> "isEmpty"

prims :: IsString s => [(s, PrimOp)]
prims =
  (map (primName &&& identity)
  [ Verify
  , Sign
  , Block
  , Deployer
  , Sender
  , Created
  , Address
  , Validator
  , Sha256
  , AccountExists
  , AssetExists
  , ContractExists
  , Terminate
  , Now
  , Transition
  , CurrentState
  , TxHash
  , Bound
  , ContractValue
  , ContractValueExists
  , ContractState
  , NovationInit
  , NovationStop
  , IsBusinessDayUK
  , NextBusinessDayUK
  , IsBusinessDayNYSE
  , NextBusinessDayNYSE
  , Between
  , TimeDiff
  , Fixed1ToFloat
  , Fixed2ToFloat
  , Fixed3ToFloat
  , Fixed4ToFloat
  , Fixed5ToFloat
  , Fixed6ToFloat
  , FloatToFixed1
  , FloatToFixed2
  , FloatToFixed3
  , FloatToFixed4
  , FloatToFixed5
  , FloatToFixed6
  ]) ++ map (second AssetPrimOp) assetPrims
     ++ map (second MapPrimOp) mapPrims
     ++ map (second SetPrimOp) setPrims
     ++ map (second CollPrimOp) collPrims

assetPrims :: IsString s => [(s, AssetPrimOp)]
assetPrims =
  map (assetPrimName &&& identity)
    [ TransferTo
    , TransferFrom
    , CirculateSupply
    , TransferHoldings
    , HolderBalance
    ]

mapPrims :: IsString s => [(s, MapPrimOp)]
mapPrims =
  map (mapPrimName &&& identity)
    [minBound.. maxBound]

setPrims :: IsString s => [(s, SetPrimOp)]
setPrims =
  map (setPrimName &&& identity)
    [minBound.. maxBound]

collPrims :: IsString s => [(s, CollPrimOp)]
collPrims =
  map (collPrimName &&& identity)
    [minBound.. maxBound]

lookupPrim :: (Eq s, IsString s) => s -> Maybe PrimOp
lookupPrim p = Data.List.lookup p prims

arity :: PrimOp -> Int
arity = \case
  Verify              -> 3
  Sign                -> 1
  Block               -> 0
  Deployer            -> 0
  Sender              -> 0
  Created             -> 0
  Address             -> 0
  Validator           -> 0
  Sha256              -> 1
  AccountExists       -> 1
  AssetExists         -> 1
  ContractExists      -> 1
  Terminate           -> 1
  Now                 -> 0
  Transition          -> 1
  CurrentState        -> 0
  TxHash              -> 0
  Bound               -> 2
  ContractValue       -> 2
  ContractValueExists -> 2
  ContractState       -> 1
  NovationInit        -> 1
  NovationStop        -> 0
  IsBusinessDayUK     -> 1
  NextBusinessDayUK   -> 1
  IsBusinessDayNYSE   -> 1
  NextBusinessDayNYSE -> 1
  Between             -> 3
  TimeDiff            -> 2
  Fixed1ToFloat       -> 1
  Fixed2ToFloat       -> 1
  Fixed3ToFloat       -> 1
  Fixed4ToFloat       -> 1
  Fixed5ToFloat       -> 1
  Fixed6ToFloat       -> 1
  FloatToFixed1       -> 1
  FloatToFixed2       -> 1
  FloatToFixed3       -> 1
  FloatToFixed4       -> 1
  FloatToFixed5       -> 1
  FloatToFixed6       -> 1
  AssetPrimOp a       -> assetPrimArity a
  MapPrimOp m         -> mapPrimArity m
  SetPrimOp m         -> setPrimArity m
  CollPrimOp c        -> collPrimArity c

assetPrimArity :: AssetPrimOp -> Int
assetPrimArity = \case
  TransferTo       -> 2
  TransferFrom     -> 3
  CirculateSupply  -> 2
  TransferHoldings -> 4
  HolderBalance    -> 2

mapPrimArity :: MapPrimOp -> Int
mapPrimArity = \case
  MapInsert -> 3
  MapDelete -> 2
  MapLookup -> 2
  MapModify -> 3

setPrimArity :: SetPrimOp -> Int
setPrimArity = \case
  SetInsert -> 2
  SetDelete -> 2

collPrimArity :: CollPrimOp -> Int
collPrimArity = \case
  Aggregate -> 3
  Transform -> 2
  Filter    -> 2
  Element   -> 2
  IsEmpty   -> 1

instance Pretty PrimOp where
  ppr = ppr . (primName :: PrimOp -> Text)
