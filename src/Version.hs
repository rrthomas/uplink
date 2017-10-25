{-|

Version information.

-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Version (
  version,
  branch,
  commit,
  dirty,
) where

import Protolude

# ifndef OS_Mac
import Development.GitRev
# endif

import qualified Data.Version as V
import qualified Paths_uplink as Paths

-- Template Haskell is not our friend
# ifdef __APPLE__
version :: [Char]
version = V.showVersion Paths.version

branch :: [Char]
branch = "unknown"

commit :: [Char]
commit = "unknown"

dirty :: [Char]
dirty = "unknown"

# else

version :: [Char]
version = V.showVersion Paths.version

branch :: [Char]
branch = $(gitBranch)

commit :: [Char]
commit = (take 8 $(gitHash))

dirty :: [Char]
dirty = show $(gitDirty)
# endif
