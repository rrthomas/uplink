{-|

Compiler entry points and disk serialization for scripts.

-}

module Script.Compile (
  -- ** Compiler
  compile,
  compileFile,

  loadStorageFile,
  matchTypes,
  verifyStorage,

  emptyTarget,

  -- ** Editor integration
  lintFile,
  verifyScript,
  formatScript,

  -- ** Persistence
  magicNumber,
  writeScript,
  readScript,

  -- ** Binary format
  putScript,
  getScript,

  -- ** Testing
  scriptString,
  scriptBytes,
  scriptHex,
) where

import Protolude hiding (Type)

import qualified Utils
import qualified Storage
import qualified Address
import qualified Encoding

import Script (Script, Name(..), Type, Located)
import qualified Script
import Script.Typecheck (Sig)
import qualified Script.Pretty as Pretty
import qualified Script.Parser as Parser
import qualified Script.Analysis as Anal
import qualified Script.Duplicate as Dupl
import qualified Script.Typecheck as Typecheck
import qualified Script.Undefinedness as Undef
import Data.Bifunctor (first, second)
import Data.Serialize as S
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Hexdump
import qualified Data.Aeson as A
import qualified Data.Map as Map
import System.Directory
import Control.Monad (fail)

-------------------------------------------------------------------------------
-- FCL Compilation
-------------------------------------------------------------------------------

data Pass
  = Parse
  | Graph
  | Typecheck
  | DuplCheck
  | UndefCheck
  deriving (Eq, Show)

-- | A stage in the compiler.
stage :: Pretty.Pretty err => Pass -> Either err a -> Either Text a
stage pass = either (Left . Pretty.prettyPrint) Right

-- | Compile a given file into it's signatures and AST.
compileFile :: FilePath -> IO (Either Text ([(Name,Sig)], Script))
compileFile fpath = do
  res <- Utils.safeRead fpath
  case res of
    (Left err) -> return $ Left err
    (Right contents) -> return $ compile $ decodeUtf8 contents

-- | Compile a text stream into it's signatures and AST.
compile :: Text -> Either Text ([(Name,Sig)], Script)
compile body = do
  ast  <- stage Parse (Parser.parseScript body)
  _    <- stage DuplCheck (Dupl.duplicateCheck ast)
  sigs <- stage Typecheck (Typecheck.signatures ast)
  gr   <- stage Graph (Anal.checkGraph ast)
  _    <- stage UndefCheck (Undef.undefinednessAnalysis ast)
  pure (sigs, ast)

-- | Compile returning either the parser errors
lintFile :: FilePath -> IO [Parser.ParseErrInfo]
lintFile fpath = do
  fcontents <- readFile fpath
  let contents = Parser.parseScript fcontents
  case contents of
    Left err  -> pure [err]
    Right ast -> pure []

-- | Verify that a given script typechecks succesfully.
verifyScript :: Script -> Bool
verifyScript script =
  case Typecheck.signatures script of
    Left err -> False
    Right sigs ->
      case Anal.checkGraph script of
        Left err -> False
        Right gr -> True

-- | Compile a file pretty printing the resulting AST.
formatScript :: FilePath -> IO (Either Text LText)
formatScript fpath = do
  body <- readFile fpath
  let res = Parser.parseScript body
  case res of
    Left err  -> pure $ Left (show err)
    Right ast -> pure $ Right (Pretty.print ast)


loadStorageFile :: FilePath -> IO (Either Text Storage.Storage)
loadStorageFile fpath = do
  res <- Utils.safeRead fpath
  case res of
    Left err -> return $ Left err
    Right contents -> pure (loadStorage $ decodeUtf8 contents)


loadStorage :: Text -> (Either Text Storage.Storage)
loadStorage body = first toS $ A.eitherDecode (toS body)

verifyStorage :: Storage.Storage -> Script -> Either (Map.Map Name (Type, Type)) ()
verifyStorage store script = if null errors then Right () else Left errors
  where
    enumInfo = Script.createEnumInfo (Script.scriptEnums script)

    valueType v = case Script.mapType enumInfo v of
                    Nothing -> Script.TAny
                    Just ty -> ty

    storeLocals  = Map.mapKeys
        (\(Storage.Key v) -> Script.Name (toS v)) $ Map.map valueType store

    scriptLocals = Map.fromList
        [(name, ty) | Script.LocalDefNull ty (Script.Located _ name) <- Script.scriptDefs script ]

    errors = matchTypes storeLocals scriptLocals

matchTypes :: Map.Map Name Type -> Map.Map Name Type -> Map.Map Name (Type, Type)
matchTypes a b  = Map.mapMaybe identity $ Map.intersectionWith matchTypes' a b
  where
    matchTypes' Script.TInt Script.TFloat = Nothing
    matchTypes' a b = if a == b then Nothing else Just (a,b)

-- | Empty compiler artifact
emptyTarget :: IO (Either Text ([(Name,Sig)], Script))
emptyTarget = pure (Right ([], Script.emptyScript))

-------------------------------------------------------------------------------
-- Binary Serialization
-------------------------------------------------------------------------------

{-

+-------------------+
| Header            |
+-------------------+
|                   |
| Storage           |
|                   |
+-------------------+
|                   |
| Script            |
|                   |
+-------------------+

-}

magicNumber :: ByteString
magicNumber = BS.pack [46, 70, 85, 78, 67]

maxStorage :: Int16
maxStorage = maxBound

-- | Serialize a script to disk.
putScript :: Script -> Maybe Storage.Storage -> Address.Address -> PutM ()
putScript script store addr = do
  -- Header
  S.putByteString magicNumber

  -- Storage
  case store of
    (Just store) -> do
      let values = encode store
      let len = fromIntegral (BS.length values)
      putWord16be len
      S.putByteString values
    Nothing -> putWord16be 0

  -- Address
  Address.putAddress addr

   -- Script
  let scriptbs = Encoding.base64 (encode script)
  let len = (fromIntegral (BS.length scriptbs))
  putWord16be len
  S.putByteString scriptbs


getScript :: Get (Script, Maybe Storage.Storage, Address.Address)
getScript = do
  -- Storage
  storeLen <- fromIntegral <$> getWord16be
  storage <-
    if storeLen == 0
       then return Nothing
       else do
         sto <- decode <$> getByteString storeLen
         case sto of
           Left err -> fail "Could not decode storage."

           Right s -> return $ Just s

  -- Address
  addr <- Address.getAddress
  -- Script
  len <- fromIntegral <$> getWord16be
  scriptb64 <- getByteString len
  script <- return $ Encoding.unbase64 scriptb64 >>= decode
  case script of
    Left err -> fail "Could not decode script."

    Right script ->
      pure (script, storage, addr)

-------------------------------------------------------------------------------
-- IO Operations
-------------------------------------------------------------------------------

-- | Read a script from disk.
readScript :: ByteString -> Either [Char] (Script, Maybe Storage.Storage, Address.Address)
readScript s = case BS.splitAt (BS.length magicNumber) s of
  (header, contents) -> do
    if header == magicNumber
      then runGet getScript contents
      else Left "Header does not match"

-- | Write a script to disk.
writeScript :: Script -> Maybe Storage.Storage -> Address.Address -> ByteString
writeScript script store addr = snd (runPutM (putScript script store addr))

-------------------------------------------------------------------------------
-- testing
-------------------------------------------------------------------------------

scriptString :: Script -> ByteString
scriptString s = magicNumber <> (encode s)

scriptBytes :: Script -> [Word8]
scriptBytes s = Utils.toByteList (magicNumber <> encode s)

scriptHex :: Script -> [Char]
scriptHex s = Hexdump.prettyHex (magicNumber <> encode s)
