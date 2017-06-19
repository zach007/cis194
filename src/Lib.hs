module Lib
    ( someFunc,
      someString
    ) where

import Week1
someFunc :: IO ()
someFunc = putStrLn someString

someString :: String
someString = "someString"
