-----------------------------------------------------------------------------
--
-- Module      :  Language.Elm.TH.Json
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
module Language.Elm.TH.Json where

{-# LANGUAGE TemplateHaskell, QuasiQuotes, MultiWayIf #-}

import Language.Haskell.TH.Syntax

import Data.Aeson.TH


import qualified AST.Module as M
import qualified AST.Declaration as D
import qualified AST.Expression.General as E
import qualified AST.Literal as L
--import qualified AST.Location as Lo
import qualified AST.Pattern as P
import qualified AST.Type as T

import Data.List (isPrefixOf)

import Language.Haskell.TH.Desugar.Sweeten
import Language.Haskell.TH.Desugar

import Language.Elm.TH.Util


--import Parse.Expression (makeFunction)

import Control.Applicative

import Control.Monad

import Control.Monad.State (StateT)
import qualified Control.Monad.State as S

-- |Helper function to apply arguments to a function
applyArgs :: Exp -> [Exp] -> Exp
applyArgs fun args = foldl (\ accumFun nextArg -> AppE accumFun nextArg) fun args

fnComp = VarE $ mkName "."

-- | Helper function to generate a the names X1 .. Xn with some prefix X  
nNames :: Int -> String -> SQ [Name]
nNames n base = do
  let varStrings = map (\n -> base ++ show n) [1..n]
  mapM liftNewName varStrings

-- | Variable for the getter function getting the nth variable from a Json
varNamed :: Exp
varNamed = VarE (mkName "JsonUtils.varNamed")
  
-- | Expression getting a named subvariable from a JSON object
getVarNamed :: String -> Exp
getVarNamed nstr = AppE (AppE varNamed jsonArgExp ) (LitE $ StringL nstr)

-- | Filter function to test if a dec is a data
-- Also filters out decs which types that can't be serialized, such as functions
isData :: Dec -> Bool
isData dec = (isData' dec) && (canSerial dec) 
  where
    isData' DataD{} = True
    isData' NewtypeD{} = True
    isData' TySynD{} = True
    isData' _ = False
    
    canSerial (DataD _ _ _ ctors _) = all canSerialCtor ctors
    canSerial (NewtypeD _ _ _ ctor _) = canSerialCtor ctor
    canSerial (TySynD _ _ ty) = canSerialType ty
    --can't serialize if type variables --TODO is this true?
    canSerial _ = False
    
    canSerialCtor (NormalC _ types) = all (canSerialType) (map snd types)
    canSerialCtor (RecC _ types) = all (canSerialType) (map (\(_,_,c)->c) types)
    
    canSerialType (ArrowT) = False
    canSerialType t = all canSerialType (subTypes t)

--General helper functions
jsonArgName :: Name
jsonArgName = mkName "jsonArg"

jsonArgPat :: Pat
jsonArgPat = VarP jsonArgName

jsonArgExp :: Exp
jsonArgExp = VarE jsonArgName

fromJsonName :: Name -> Name
fromJsonName name = mkName $ "fromJson_" ++ nameToString name

toJsonName :: Name -> Name
toJsonName name = mkName $ "toJson_" ++ nameToString name


makeFromJson :: [Dec] -> SQ [Dec]
makeFromJson allDecs = do
  let decs = filter isData allDecs
  mapM fromJsonForDec decs

-- | Given a type, and an expression for an argument of type Json
-- return the expression which applies the proper fromJson function to that expression
fromJsonForType :: Type -> SQ Exp

--Type name not covered by Prelude
fromJsonForType (ConT name) = case (nameToString name) of
  "Int" -> return $ VarE $ mkName "JsonUtils.intFromJson"
  "Bool" -> return $ VarE $ mkName "JsonUtils.boolFromJson"
  "Float" -> return $ VarE $ mkName "JsonUtils.floatFromJson"
  "Double" -> return $ VarE $ mkName "JsonUtils.floatFromJson"
  "String" -> return $ VarE $ mkName "JsonUtils.stringFromJson"
  _ -> return $ VarE $ fromJsonName name

fromJsonForType (AppT ListT t) = do
  subExp <- fromJsonForType t
  return $ AppE (VarE $ mkName "JsonUtils.listFromJson") subExp  
  
fromJsonForType (AppT (ConT name) t) = do
  subExp <- fromJsonForType t
  case (nameToString name) of
    "Maybe" -> return $ AppE (VarE $ mkName "JsonUtils.maybeFromJson") subExp
    
fromJsonForType (AppT (AppT (ConT name) t1) t2) = do
  sub1 <- fromJsonForType t1
  sub2 <- fromJsonForType t2
  case (nameToString name) of
    "Data.Map.Map" -> return $ applyArgs (VarE $ mkName "JsonUtils.dictFromJson") [sub1, sub2]
    s -> error  $ "Unsupported json type " ++ s
    
fromJsonForType t
  | isTupleType t = do
      let tList = tupleTypeToList t
      let n = length tList
      --Generate the lambda to convert the list into a tuple
      subFunList <- mapM fromJsonForType tList
      argNames <- mapM (liftNewName . ("x" ++) . show) [1 .. n]
      let argValues = map VarE argNames
      let argPat = ListP $ map VarP argNames
      let lambdaBody = TupE $ zipWith AppE subFunList argValues
      let lambda = LamE [argPat] lambdaBody
      let makeList = VarE $ mkName "makeList"
      return $ InfixE (Just lambda) fnComp (Just makeList)
   | otherwise = error $ "Can't make Json for type " ++ (show t) 

-- |Given a type declaration, generate the function declaration
-- Which takes a Json object to a value of that type
fromJsonForDec :: Dec -> SQ Dec

--Special case: we only have one ctor, so we don't use a tag
fromJsonForDec dec@(DataD _ name _ [ctor] _deriving) = do
  Match _pat fnBody _decs <- fromMatchForCtor 1 ctor
  let argPat = jsonArgPat
  let fnName = fromJsonName name
  let fnClause = Clause [argPat] fnBody []
  return $ FunD fnName [fnClause]
  
  

fromJsonForDec dec@(DataD _ name _ ctors _deriving) = do
  let argTagExpression = AppE (VarE $ mkName "JsonUtils.getTag") jsonArgExp
  let numCtors = length ctors
  ctorMatches <- mapM (fromMatchForCtor numCtors) ctors
  let fnExp = CaseE argTagExpression ctorMatches
  let argPat = jsonArgPat
  let fnName = fromJsonName name
  let fnBody = NormalB fnExp
  let fnClause = Clause [argPat] fnBody []
  return $ FunD fnName [fnClause]

fromJsonForDec (NewtypeD cxt name tyBindings  ctor nameList) = 
  fromJsonForDec $ DataD cxt name tyBindings [ctor] nameList
  
fromJsonForDec dec@(TySynD name _tyvars ty) = do
  let fnName = fromJsonName name
  fnBody <- NormalB <$> fromJsonForType ty
  let fnClause = Clause [] fnBody []
  return $ FunD fnName [fnClause]
  
  
fromMatchForCtor :: Int -> Con -> SQ Match

fromMatchForCtor numCtors (NormalC name strictTypes) = do
  let types = map snd strictTypes
  let leftHandSide = LitP $ StringL $ nameToString name
  
  let ctorExp = VarE name
  
  --Exp in TH, list in Haskell
  contentListExpr <- NormalB <$> unpackContents numCtors jsonArgExp
  
  fromJsonFunctions <- mapM fromJsonForType types
  let intNames = map (("subVar" ++) . show) [1 .. length types]
  subDataNames <- mapM liftNewName intNames
  --We unpack each json var into its own named variable, so we can unpack them into different types
  let subDataListPattern = ListP $ map VarP subDataNames
  
  --let subDataExprs = map VarE subDataNames
  
  let unJsonedExprList =   zipWith AppE fromJsonFunctions (map VarE subDataNames)
  
  let letExp = LetE [ValD subDataListPattern contentListExpr []] (applyArgs ctorExp unJsonedExprList)
  
  let rightHandSide = NormalB $ letExp
  return $ Match leftHandSide rightHandSide []
  

fromMatchForCtor _numCtors (RecC name vstList) = do
  let nameTypes = map (\(a,_,b)->(nameToString a,b)) vstList
  let matchPat = LitP $ StringL $ nameToString name
  (subNames, subDecs) <- unzip <$> mapM getSubJsonRecord nameTypes
  let body = NormalB $ if null subNames
              then applyArgs subNames ctorExp
              else LetE subDecs (applyArgs subNames ctorExp)
  return $ Match matchPat body []
  where
    ctorExp = ConE name
    applyArgs t accum = foldl (\ accum h -> AppE accum (VarE h)) accum t
    

-- | Generate a declaration, and a name bound in that declaration,
-- Which unpacks a value of the given type from the nth field of a JSON object
getSubJsonRecord :: (String, Type) -> SQ (Name, Dec)
-- We need special cases for lists and tuples, to unpack them
--TODO recursive case
getSubJsonRecord (field, t) = do
  funToApply <- fromJsonForType t
  subName <- liftNewName "subVar"
  let subLeftHand = VarP subName
  let subRightHand = NormalB $ AppE funToApply (getVarNamed field)
  return (subName, ValD subLeftHand subRightHand [])
    
unpackContents :: Int -> Exp -> SQ Exp
unpackContents numCtors jsonValue = return $ applyArgs (VarE $ mkName "JsonUtils.unpackContents") [LitE $ IntegerL $ toInteger numCtors, jsonValue]


  
  
  
  
  

makeToJson allDecs = do
  let decs = filter isData allDecs
  mapM toJsonForDec decs

toJsonForType :: Type -> SQ Exp
toJsonForType (ConT name) = case (nameToString name) of
  "Int" -> return $ VarE $ mkName "JsonUtils.intToJson"
  "Bool" -> return $ VarE $ mkName "JsonUtils.boolToJson"
  "Float" -> return $ VarE $ mkName "JsonUtils.floatToJson"
  "Double" -> return $ VarE $ mkName "JsonUtils.floatToJson"
  "String" -> return $ VarE $ mkName "JsonUtils.stringToJson"
  _ -> return $ VarE $ toJsonName name
  
toJsonForType (AppT (AppT (ConT name) t1) t2) = do
  sub1 <- toJsonForType t1
  sub2 <- toJsonForType t2
  case (nameToString name) of
    "Data.Map.Map" -> return $ applyArgs (VarE $ mkName "JsonUtils.dictToJson") [sub1, sub2]
    s -> error  $ "Unsupported json type " ++ s
    
  
toJsonForType (AppT ListT t) = do
  subExp <- toJsonForType t
  return $ AppE (VarE $ mkName "JsonUtils.listToJson") subExp  
  
toJsonForType (AppT (ConT name) t) = do
  subExp <- toJsonForType t
  case (nameToString name) of
    "Maybe" -> return $ AppE (VarE $ mkName "JsonUtils.maybeToJson") subExp

toJsonForType t 
  | isTupleType t = do
      let tList = tupleTypeToList t
      let n = length tList
      --Generate the lambda to convert the list into a tuple
      subFunList <- mapM toJsonForType tList
      argNames <- mapM (liftNewName . ("x" ++) . show) [1 .. n]
      let argValues = map VarE argNames
      let argPat = TupP $ map VarP argNames
      --Get each tuple element as Json, then wrap them in a Json Array
      let listExp = AppE (VarE $ mkName "Json.Array") (ListE $ zipWith AppE subFunList argValues)
      return $ LamE [argPat] listExp  

toJsonForDec :: Dec -> SQ Dec
toJsonForDec dec@(DataD _ name _ ctors _deriving) = do
  let argPat = jsonArgPat
  let argExp = jsonArgExp
  let numCtors = length ctors 
  ctorMatches <- mapM (toMatchForCtor numCtors) ctors
  
  let fnExp = CaseE jsonArgExp ctorMatches
  
  let fnName = toJsonName name
  let fnBody = NormalB fnExp
  let fnClause = Clause [argPat] fnBody []
  return $ FunD fnName [fnClause]
  
toJsonForDec (NewtypeD cxt name tyBindings  ctor nameList) = 
  toJsonForDec $ DataD cxt name tyBindings [ctor] nameList
  
toJsonForDec dec@(TySynD name _tyvars ty) = do
  let fnName = toJsonName name
  fnBody <- NormalB <$> toJsonForType ty
  let fnClause = Clause [] fnBody []
  return $ FunD fnName [fnClause]
 
toJsonForDec dec = error $ "Unknown dec type" ++ (show dec)

  
toMatchForCtor :: Int -> Con -> SQ Match
toMatchForCtor numCtors (NormalC name strictTypes) = do
  let types = map snd strictTypes
  let numStrings = map (("subVar_" ++) . show) [1 .. length types]
  subDataNames <- mapM liftNewName numStrings
  let subDataPats = map VarP subDataNames
  
  let leftHandSide = ConP name subDataPats
  
  let subDataExprs = map VarE subDataNames
  
  toJsonFunctions <- mapM toJsonForType types
  
  let contentsList = ListE $ zipWith AppE toJsonFunctions subDataExprs
  
  jsonValueExp <- packContents numCtors name contentsList
  let rightHandSide = NormalB  jsonValueExp
  
  return $ Match  leftHandSide rightHandSide []

--TODO is there ever a record with 0 args?
toMatchForCtor _numCtors (RecC name vstList) = do
  let (adtNames, _, types) = unzip3 vstList
  let n = length types
  jsonNames <- nNames n "jsonVar"
  let adtPats = map VarP adtNames
  let matchPat = ConP name adtPats
  jsonDecs <- mapM makeSubJsonRecord (zip3 types adtNames jsonNames)
  dictName <- liftNewName "objectDict"
  dictDec <-  makeRecordDict name dictName jsonNames
  let ret = AppE (VarE $ mkName "Json.Object") (VarE dictName)
  let body = NormalB $ LetE (jsonDecs ++ [dictDec]) ret
  return $ Match matchPat body []
 
-- | Generate the declaration of a dictionary mapping field names to values
-- to be used with the JSON Object constructor
makeRecordDict :: Name -> Name -> [Name] -> SQ Dec

makeRecordDict ctorName dictName jsonNames = do
  let leftSide = VarP dictName
  let jsonExps = map VarE jsonNames
  let fieldNames = map (LitE . StringL . show) [1 .. (length jsonNames)]
  let tuples = map (\(field, json) -> TupE [field, json]) (zip fieldNames jsonExps)

  let ctorExp = LitE $ StringL $ nameToString ctorName

  let ctorTuple = TupE [LitE $ StringL "tag", AppE (VarE (mkName "Json.String")) ctorExp ]
  let tupleList = ListE $ [ctorTuple] ++ tuples
  let rightSide = NormalB $ AppE (VarE $ mkName "Data.Map.fromList") tupleList
  return $ ValD leftSide rightSide []
  

-- | Generate the declaration of a value converted to Json
-- given the name of an ADT value to convert
makeSubJsonRecord :: (Type, Name, Name) -> SQ Dec
-- We need special cases for lists and tuples, to unpack them
--TODO recursive case
makeSubJsonRecord (t, adtName, jsonName) = do
  funToApply <- toJsonForType t
  let subLeftHand = VarP jsonName
  let subRightHand = NormalB $ AppE funToApply (VarE adtName)
  return $ ValD subLeftHand subRightHand []
  
packContents :: Int -> Name -> Exp -> SQ Exp
packContents numCtors name contentList = do
  return $ applyArgs (VarE $ mkName "JsonUtils.packContents") [LitE $ IntegerL $ toInteger numCtors, LitE $ StringL $ nameToString name, contentList]
  
  
