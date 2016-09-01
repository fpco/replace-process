{-# LANGUAGE OverloadedStrings #-}
import System.Process.Replace
import System.Environment.Executable (getExecutablePath)
import System.Environment (getArgs, getEnvironment)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode (ExitFailure), exitWith)
import Test.Hspec (shouldBe, it, hspec)
import qualified Data.ByteString.Char8 as S8
import qualified System.IO as IO
import Control.Monad (forM_)
import qualified Data.Map as Map

say :: String -> IO ()
say s = do
    putStrLn s
    IO.hFlush IO.stdout

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> master
        ["child", arg1, arg2] -> child arg1 arg2
        ["grandchild"] -> grandchild
        _ -> error $ "Invalid args: " ++ show args

argPairs :: [(String, String)]
argPairs = do
    arg1 <- ["env", "noenv"]
    arg2 <- ["path", "nopath"]
    return (arg1, arg2)

master :: IO ()
master = hspec $ do
    it "exit code sanity" $ do
        cmd <- getExecutablePath
        (ec, _out, _err) <- readProcessWithExitCode cmd ["grandchild"] ""
        ec `shouldBe` ExitFailure 2

    forM_ argPairs $ \(arg1, arg2) -> it (unwords [arg1, arg2]) $ do
        cmd <- getExecutablePath
        (ec, out, err) <- readProcessWithExitCode cmd ["child", arg1, arg2] ""

        out `shouldBe` unlines
            [ "start child"
            , "start grandchild"
            , show $ case arg1 of
                "env" -> Just ("BAR" :: String)
                "noenv" -> Nothing
                _ -> error $ "arg1 is invalid: " ++ arg1
            , "end grandchild"
            ]
        err `shouldBe` ""
        ec `shouldBe` ExitFailure 2

child :: String -> String -> IO a
child arg1 arg2 = do
    menv <-
        case arg1 of
            "env" -> return $ Just $ Map.fromList [("FOO", "BAR")]
            "noenv" -> return $ Nothing
            _ -> error $ "Invalid arg1: " ++ arg1
    path <-
        case arg2 of
            "path" -> return True
            "nopath" -> return False
            _ -> error $ "Invalid arg2: " ++ arg2

    say "start child"
    cmd <- getExecutablePath
    () <- replaceProcess (S8.pack cmd) path ["grandchild"] menv
    say "end child" -- should never be called
    exitWith (ExitFailure 1) -- also not called

grandchild :: IO a
grandchild = do
    say "start grandchild"

    env <- getEnvironment
    print $ lookup "FOO" env

    say "end grandchild"
    exitWith (ExitFailure 2)
