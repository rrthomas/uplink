{-|

Global transaction log.

-}

module TxLog (
  -- ** Types
  TxLog,
  TxLogElem,

  -- ** Global transaction log
  txLogFile,

  -- ** Delta logging
  writeDeltas,
  writeDeltasJSON,
) where

import Protolude

import Delta
import Address
import Script.Pretty hiding ((<>))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BS

import System.FilePath

-------------------------------------------------------------------------------
-- Log Generation
-------------------------------------------------------------------------------

type TxLogElem = (Int64, Address AContract, Delta)
type TxLog = [TxLogElem]

txLogFile :: FilePath -> FilePath
txLogFile root = root </> "txlog"

-- | Write delta
writeDelta :: FilePath -> Int64 -> Address AContract -> Delta -> IO ()
writeDelta fp blkIx addr dt = appendFile fp (line <> "\n")
  where
    line :: Text
    line = prettyPrint ("Block " <+> ppr blkIx <+> ":" <+> ppr (Address.shortAddr addr) <+> ppr dt)

-- | Write delta list associated with a block
writeDeltas :: FilePath -> Int64 -> Address AContract -> [Delta] -> IO ()
writeDeltas fp blkIx addr = mapM_ (\dt -> writeDelta fp blkIx addr dt)

-- | Write delta list associated with a block in JSON
writeDeltasJSON :: Int64 -> FilePath -> Address AContract -> [Delta] -> IO ()
writeDeltasJSON blkIx file addr dts =
  BS.appendFile file $ A.encode [(blkIx, addr, Script.Pretty.print dt) | dt <- dts]
