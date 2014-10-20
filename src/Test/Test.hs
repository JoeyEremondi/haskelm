{-# LANGUAGE TemplateHaskell, QuasiQuotes, MultiWayIf #-}
module Main where

{-|
Haskelm test suite
For the moment, just contains some basic tests
This file also serves as an example of how to 
translate Elm from different sources
-}



import Language.Elm.TH
import Data.List (intercalate)
import Control.Monad


-- | Similarly, we can load a module from a file
elmString = $(translateToElm
    (defaultOptions {moduleName="Foo"})
    ("src/Test/module1.hs" ) )

-- | We can now access the elm strings we declared
main = do
  putStrLn "Generated elm strings: "
  mapM_ putStrLn [elmString]
  writeFile "src/TestOutput.elm" elmString
  --putStrLn elmString
  return ()
