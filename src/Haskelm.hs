module Main( main ) where

import System.Environment( getArgs )

import Language.Elm.TH
import Language.Haskell.TH




main = do
  (infile:_) <- getArgs
  result <- runQ $ do
    let options = Options True [] [] "Main"
    LitE (StringL str) <- translateToElm options infile
    return str
  putStrLn result