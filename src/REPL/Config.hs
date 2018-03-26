{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module REPL.Config where
import Protolude hiding (Type)
import Control.Distributed.Process (Process, ProcessId)
import Control.Distributed.Process.Node (LocalNode, runProcess)


import System.Console.Repline
import System.Console.Haskeline.MonadException
import Script (Name, Type)
import Network.P2P.Simulate
import Address

data REPLContext = REPLContext {
    simKey  :: SimKey
  , sender  :: Address
  , verbose :: Bool
  , callableMethods :: [(Name,[(Name,Type)])]
  }

data REPLConfig = REPLConfig {
    simProcId :: ProcessId,
    localNode :: LocalNode
  }

-- | General REPLT Monad Transformer (to get instances for freee!)
newtype REPLT m a = REPLT
  { unREPLT :: StateT REPLContext m a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadState REPLContext)

-- | Necessary for using REPLT as a base monad for HaskelinT transformer
-- Note: This sucks, but I got it working
instance MonadException m => MonadException (REPLT m) where
  controlIO f =
    REPLT $ StateT $ \s ->
      controlIO $ \(RunIO run) ->
        let run' = RunIO (fmap (REPLT . StateT . const) . run . flip runStateT s . unREPLT)
        in fmap (flip runStateT s . unREPLT) $ f run'

runREPLT :: Monad m => REPLContext -> REPLT m a -> m a
runREPLT replContext = flip evalStateT replContext . unREPLT

-- | Monad for use in REPL Program
newtype REPLM a = REPLM
  { unREPLM :: HaskelineT (REPLT IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadState REPLContext)
