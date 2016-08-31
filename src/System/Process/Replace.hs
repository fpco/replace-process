{-# LANGUAGE CPP #-}
module System.Process.Replace
    ( replaceProcess
    ) where

import Data.ByteString (ByteString)
import Data.Foldable as F
import qualified Data.Map as Map
import Data.Map (Map)

#ifdef WINDOWS
#else
import qualified System.Posix.Process.ByteString as P
#endif

-- | Replace the currently executing process with a different process.
--
-- On POSIX systems, this is well known as the @exec@ system call.
--
-- @since 0.1.0.0
replaceProcess
    :: F.Foldable f
    => ByteString -- ^ command
    -> Bool -- ^ search PATH?
    -> f ByteString -- ^ Arguments
    -> Maybe (Map ByteString ByteString) -- ^ Environment
    -> IO a

#ifdef WINDOWS
#else
replaceProcess cmd path args menv = P.executeFile
    cmd
    path
    (F.toList args)
    (fmap Map.toList menv)
#endif
