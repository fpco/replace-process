## replace-process

Replace the currently running process with a different process. Unlike
the functions from the `System.Process` module, we do not spawn a new
process. Once the functions in this repo are used, the currently
running process ceases to exist.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import System.Process.Replace (replaceProcess)
import Data.Void (absurd)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
    putStrLn "I'm about to disappear"
    hFlush stdout
    res <- replaceProcess "date" True [] Nothing
    putStrLn "This will never be called"
    absurd res -- arbitrary result type
```
