{-# LANGUAGE CPP #-}
module System.Process.Replace
    ( replaceProcess
    ) where

import Data.ByteString (ByteString)
import Data.Foldable as F
import qualified Data.Map as Map
import Data.Map (Map)

#ifdef WINDOWS
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Marshal.Utils (withMany)
import Foreign.Marshal.Array (withArray0)
import Foreign.C (CString, CInt (CInt))
import Foreign.C (getErrno, errnoToIOError)
import Data.ByteString (useAsCString)
import System.Process.Internals (pPrPr_disableITimers)
import qualified Data.ByteString.Char8 as BC
import Control.Monad (void)
#else
import System.Posix.Process.ByteString (executeFile)
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

replaceProcess cmd path args menv = executeFile
    cmd
    path
    (F.toList args)
    (fmap Map.toList menv)

#ifdef WINDOWS
--------------------------------------
-- COPIED FROM unix PACKAGE
--------------------------------------

type RawFilePath = ByteString

withFilePath :: RawFilePath -> (CString -> IO a) -> IO a
withFilePath = useAsCString

-- | as 'throwErrno', but exceptions include the given path when appropriate.
--
throwErrnoPath :: String -> RawFilePath -> IO a
throwErrnoPath loc path =
  do
    errno <- getErrno
    ioError (errnoToIOError loc errno Nothing (Just (BC.unpack path)))

-- | as 'throwErrnoIf', but exceptions include the given path when
--   appropriate.
--
throwErrnoPathIf :: (a -> Bool) -> String -> RawFilePath -> IO a -> IO a
throwErrnoPathIf cond loc path f =
  do
    res <- f
    if cond res then throwErrnoPath loc path else return res

-- | as 'throwErrnoIf_', but exceptions include the given path when
--   appropriate.
--
throwErrnoPathIf_ :: (a -> Bool) -> String -> RawFilePath -> IO a -> IO ()
throwErrnoPathIf_ cond loc path f  = void $ throwErrnoPathIf cond loc path f

-- | as 'throwErrnoIfMinus1_', but exceptions include the given path when
--   appropriate.
--
throwErrnoPathIfMinus1_ :: (Eq a, Num a) => String -> RawFilePath -> IO a -> IO ()
throwErrnoPathIfMinus1_  = throwErrnoPathIf_ (== -1)

-- | @'executeFile' cmd args env@ calls one of the
--   @execv*@ family, depending on whether or not the current
--   PATH is to be searched for the command, and whether or not an
--   environment is provided to supersede the process's current
--   environment.  The basename (leading directory names suppressed) of
--   the command is passed to @execv*@ as @arg[0]@;
--   the argument list passed to 'executeFile' therefore
--   begins with @arg[1]@.
executeFile :: RawFilePath                          -- ^ Command
            -> Bool                         -- ^ Search PATH?
            -> [ByteString]                 -- ^ Arguments
            -> Maybe [(ByteString, ByteString)]     -- ^ Environment
            -> IO a
executeFile path search args Nothing = do
  withFilePath path $ \s ->
    withMany withFilePath (path:args) $ \cstrs ->
      withArray0 nullPtr cstrs $ \arr -> do
        pPrPr_disableITimers
        if search
           then throwErrnoPathIfMinus1_ "executeFile" path (c_execvp s arr)
           else throwErrnoPathIfMinus1_ "executeFile" path (c_execv s arr)
        return undefined -- never reached

executeFile path search args (Just env) = do
  withFilePath path $ \s ->
    withMany withFilePath (path:args) $ \cstrs ->
      withArray0 nullPtr cstrs $ \arg_arr ->
    let env' = map (\ (name, val) -> name `BC.append` ('=' `BC.cons` val)) env in
    withMany withFilePath env' $ \cenv ->
      withArray0 nullPtr cenv $ \env_arr -> do
        pPrPr_disableITimers
        if search
           then throwErrnoPathIfMinus1_ "executeFile" path
                   (c_execvpe s arg_arr env_arr)
           else throwErrnoPathIfMinus1_ "executeFile" path
                   (c_execve s arg_arr env_arr)
        return undefined -- never reached

foreign import ccall unsafe "_execvp"
  c_execvp :: CString -> Ptr CString -> IO CInt

foreign import ccall unsafe "_execv"
  c_execv :: CString -> Ptr CString -> IO CInt

foreign import ccall unsafe "_execve"
  c_execve :: CString -> Ptr CString -> Ptr CString -> IO CInt

foreign import ccall unsafe "_execvpe"
  c_execvpe :: CString -> Ptr CString -> Ptr CString -> IO CInt
#endif
