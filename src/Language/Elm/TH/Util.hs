-----------------------------------------------------------------------------
--
-- Module      :  Language.Elm.TH.Util
-- Copyright   :  Copyright: (c) 2011-2013 Joey Eremondi
-- License     :  BSD3
--
-- Maintainer  :  joey.eremondi@usask.ca
-- Stability   :  experimental
-- Portability :  portable
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell, QuasiQuotes, MultiWayIf #-}

module Language.Elm.TH.Util where


import Language.Haskell.TH.Syntax

import Data.Aeson.TH


import qualified AST.Module as M
import qualified AST.Declaration as D
import qualified AST.Expression.General as E
import qualified AST.Expression.Source as S
import qualified AST.Literal as L
import qualified AST.Annotation as Lo
import qualified AST.Pattern as P
import qualified AST.Type as T
import qualified AST.Variable as V

import Data.List (isPrefixOf)

import Language.Haskell.TH.Desugar.Sweeten
import Language.Haskell.TH.Desugar


--import Parse.Expression (makeFunction)

import Control.Applicative

import Control.Monad.State (StateT)
import qualified Control.Monad.State as S

import qualified Data.Map as Map

--translate newName into our new monad
liftNewName :: String -> SQ Name
liftNewName s = do
  oldState <- S.get
  let num = currentNum oldState
  name <- S.lift $ newName $ s ++ "__xxfreshxx__" ++ show num
  S.put $ oldState {currentNum = num + 1}
  return name

  
doEmitWarning :: String -> SQ [a]
doEmitWarning s = S.lift $ emitWarning s


--State information

type SQ a = StateT TranslationState Q a

--Enum for the different state vars we can access
data TranslationState = TranslationState {
    records :: Map.Map String [String],
    currentNum :: Int
  }
  
defaultState = TranslationState (Map.fromList []) 1 




-- | General error function for unimplemented features
unImplemented s = error $ "Translation of the The following haskell feature is not yet implemented: " ++ s

emitWarning :: String -> Q [a]
emitWarning s = do
  runIO $ putStrLn $ "Warning! Ignoring feature in Haskell source: " ++ s
  return []


-- |Stolen from Parse.Expression so we don't have to change any internal Elm code
makeFunction :: [P.RawPattern] -> S.Expr -> S.Expr
makeFunction args body@(Lo.A s _) =
  foldr (\arg body' -> Lo.A s $ E.Lambda arg body') body args

-- |Translate a type variable to a name, ignoring its kind
tyVarToName :: TyVarBndr -> Name
tyVarToName (PlainTV n) = n
tyVarToName (KindedTV n _ ) = n

--Abstract out the translation of names to strings
--So that we can modify if need be
--Right now is just a synonym
nameToString :: Name -> String
nameToString name = 
  case nameModule name of
    Nothing -> nameBase name--TODO fancier?
    Just base -> if  "GHC." `isPrefixOf` base
                      then nameBase name
                      else showName name

--Split a list into two alternating lists
--from http://www.haskell.org/haskellwiki/Blow_your_mind
splitList :: [a] -> ([a], [a])
splitList = foldr (\a ~(x,y) -> (a:y,x)) ([],[])

splitListN :: Int -> [a] -> [[a]]
splitListN 0 l = []
splitListN 1 l = [l]
splitListN 2 l = let (l1, l2) = splitList l
                 in [l1, l2]
splitListN n l
  | even n = let (l1, l2) = splitList l
             in (splitListN (quot n 2) l1) ++ (splitListN (quot n 2) l2)
  | otherwise = let (l1, l2) = splitList l
             in [l1] ++ (splitListN (n-1) l2)
                      
--------------------------------------------------------------------------
-- |Type helper functions
--  We use these, since String comparison is insufficients:
-- Template Haskell returns GHC.Types.Int instead of Inst

int = [t| Int |]
string = [t| String |]
float = [t| Float |]
bool = [t| Bool |]

isIntType t = do
  tint <- int
  --runIO $ putStrLn $ "Checking if int " ++ (show (t == tint))
  return (t == tint)

isStringType t = do
  tstr <- string
  return (t == tstr)

isFloatType t = do
  tfloat <- float
  return (t == tfloat)

isBoolType t = do
  tbool <- bool
  return (t == tbool)

-- | Helper function to traverse a tree of AppTs and check if a type is a tuple all the way down  
isTupleType (AppT (TupleT _arity) _) = True
isTupleType (AppT t1 t2) = isTupleType t1
isTupleType _ = False

isMaybeType (AppT (ConT name) _) = (nameToString name) == "Maybe"
isMaybeType _ = False

isMapType (AppT (AppT (ConT name) _) _) = (nameToString name) `elem` ["Map", "Data.Map.Map", "Map.Map"] --TODO deeper comparison
isMapType _ = False

-- | Helper function to linearize the AppT of tuple types
tupleTypeToList (AppT (TupleT _arity) t) = [t]
tupleTypeToList (AppT t1 t2) = tupleTypeToList t1 ++ [t2]

-- | Given a record dictionary, find constructor for one containing all the given fields
recordWithFields :: Map.Map String [String] -> [String] -> String
recordWithFields recMap fields = 
  case ctors of
       [] -> unImplemented $ "Records from other modules\n" ++ (show recMap) ++ "\n" ++ (show fields)
       [(ctor, _)] -> ctor
       _ -> unImplemented "Records sharing field names"
  where
    recList = Map.toList recMap
    hasFields (_, fieldsInRecord) = not $ null (filter (`elem` fields) fieldsInRecord)
    ctors = filter hasFields recList
    
-- | Helper to get all subtypes of a type
subTypes :: Type -> [Type]
subTypes (ForallT _ _ t) = [t]    
subTypes (VarT _) = []
subTypes (ConT _) = []
subTypes (TupleT _) = []
subTypes ArrowT = []   
subTypes ListT = []       
subTypes (AppT t1 t2) = [t1, t2]
subTypes (SigT t _) = [t]
subTypes _ = [] --TODO better catch-all?
                                      