-----------------------------------------------------------------------------
--
-- Module      :  Language.Elm.TH.HToE
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
module Language.Elm.TH.HToE where

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

import Data.List.Split (splitOn)

import Control.Monad.State (StateT)
import qualified Control.Monad.State as S

import qualified Data.Map as Map
import Data.List (intercalate)

import Debug.Trace (trace)

{-|
Haskell to Elm Translations
Most of these functions operate in the SQ monad, so that we can
compare against Haskell expressions or types in quotes (see isIntType etc.)

The return value is a list of Elm declarations
-}



findRecords :: [Dec] -> SQ ()
findRecords decs = 
  do
    mapM processDec decs
    return ()
  where
    processDec :: Dec -> SQ ()
    processDec (DataD _ _ _ ctors _) = do
      mapM_ processCtor ctors
      return ()
    processDec (NewtypeD _ _ _ ctor _) = processCtor ctor
    processDec _ = return ()
    processCtor :: Con -> SQ ()
    processCtor (RecC name vstList) = do
      let str = (nameToString name) :: String
      let (nameList, _, _) = unzip3 vstList
      let names = map nameToString nameList
      oldState <- S.get
      let newState = oldState {records = Map.insert str names (records oldState)   }
      S.put newState
      return ()
    processCtor _ = return ()



-- |Translate a constructor into a list of Strings and type-lists,
-- Which Elm uses for its internal representation of constructors
--Also returns declarations associated with records
translateCtor :: Con -> SQ ( (String,[T.Type]), [D.Declaration])
translateCtor (NormalC name strictTyList) =  do
  let sndList = map snd strictTyList
  tyList <- mapM translateType sndList
  return ( (nameToElmString name, tyList), [])

translateCtor (RecC name vstList) =  do
  --ignore strictness
  let nameTypes = map (\(a,_,b)->(a,b)) vstList
  recordTy <- translateRecord nameTypes
  let recordDecs = map ((accessorDec name). fst) nameTypes
  let makerDec = recordMakerDec (nameToElmString name) (map (nameToElmString . fst) nameTypes)
  let unboxDec = recordUnboxDec (nameToElmString name)
  return ( (nameToElmString name, [recordTy]), (makerDec:unboxDec:recordDecs)) --TODO add decs 

--Elm has no concept of infix constructor
translateCtor (InfixC t1 name t2) = translateCtor $ NormalC name [t1, t2]

translateCtor (ForallC _ _ _) = unImplemented "forall constructors"

-- | Take a list of declarations and a body
-- and put it in a let only if the declarations list is non-empty
maybeLet :: [E.Def] -> E.Expr -> E.Expr 
maybeLet eWhere eBody = 
        if null eWhere
          then  eBody
          else E.Let eWhere (Lo.none eBody)

--------------------------------------------------------------------------
-- | Helper to get the fields of the Clause type
unClause :: Clause -> ([Pat], Body, [Dec])
unClause (Clause p b d) = (p, b, d)

-- |Helper for putting elements in a list
single :: a -> [a]
single a = [a]

{-|Translate a Haskell declaration into an Elm Declaration
  Currently implemented:
    ADTs
    Functions
    Value declarations
    Type synonyms

-}

translateDec:: Dec -> SQ [D.Declaration]

--TODO translate where decs into elm let-decs
--TODO what about when more than one clause?
translateDec (FunD name [Clause patList body whereDecs])  = do
    let eName = nameToElmString name
    (ePats, asDecList) <- unzip <$> mapM translatePattern patList
    let asDecs = concat asDecList
    eWhere <- mapM translateDef whereDecs
    let eDecs = asDecs ++ eWhere
    fnBody <- translateBody body
    let eBody = maybeLet eDecs fnBody
    return $ single $ D.Definition $ E.Definition (P.PVar eName) (makeFunction ePats (Lo.none eBody)) Nothing --TODO what is maybe arg?
    
--multi-clause case i.e. pattern matching
--Convert to a single-clause function with a case statement
translateDec (FunD name clauseList) = do
  let ((Clause patList _ _):_) = clauseList
  let numArgs = length patList
  let argStrings = map (("arg" ++) . show) [1..numArgs]
  argNames <- mapM liftNewName argStrings
  let argPatList = map VarP argNames
  
  let argTuple = TupE $ map VarE argNames
  cases <- mapM clauseToCase clauseList
  let newBody = NormalB $ CaseE argTuple cases
  let singleClause = Clause argPatList newBody []
  translateDec $ FunD name [singleClause]
  where
    clauseToCase (Clause patList body whereDecs) = do
      let leftSide = TupP patList
      return $ Match leftSide body whereDecs
  

translateDec (ValD pat body whereDecs)  = do
    (ePat, asDecs) <- translatePattern pat
    valBody <- translateBody body
    eWhere <- (asDecs ++) <$> mapM translateDef whereDecs
    let eBody = maybeLet eWhere valBody
    
    return $ single $ D.Definition $ E.Definition ePat (Lo.none eBody) Nothing --TODO what is maybe arg?


translateDec dec@(DataD [] name tyBindings ctors names) = do
    --jsonDecs <- deriveFromJSON defaultOptions name
    (eCtors, extraDecLists) <- unzip <$> mapM translateCtor ctors
    return $ [ D.Datatype eName eTyVars eCtors []] ++ (concat extraDecLists) --TODO derivations?
    where
        eName = nameToElmString name
        eTyVars = map (nameToElmString . tyVarToName) tyBindings


--TODO data case for non-empty context?
translateDec (DataD cxt name tyBindings ctors names) = 
  doEmitWarning "Data declarations with TypeClass context"

--We just translate newTypes as Data definitions
--TODO: what about when record notation is used?
translateDec (NewtypeD cxt name tyBindings  ctor nameList) = 
  translateDec $ DataD cxt name tyBindings [ctor] nameList

translateDec (TySynD name tyBindings ty) = do
    let eName = nameToElmString name
    let eTyVars = map (nameToElmString . tyVarToName) tyBindings
    eTy <- translateType ty
    return $ single $ D.TypeAlias eName eTyVars eTy []

translateDec (ClassD cxt name tyBindings funDeps decs ) = doEmitWarning "Class definitions"
translateDec (InstanceD cxt ty decs) = doEmitWarning "Instance declarations"

--TODO fix signatures
translateDec (SigD name ty) = return []--(single . D.Definition . (E.TypeAnnotation (nameToString name)) ) <$> translateType ty
translateDec (ForeignD frn) = doEmitWarning "FFI declarations"


translateDec (PragmaD pragma)  = doEmitWarning "Haskell Pragmas"


translateDec (FamilyD famFlavour name [tyVarBndr] mKind) = doEmitWarning "Type families"

translateDec (DataInstD cxt name types ctors names) = doEmitWarning "Data instances"


translateDec (NewtypeInstD cxt name types ctor names) = doEmitWarning "Newtypes instances"

translateDec (TySynInstD name types theTy) = doEmitWarning "Type synonym instances"

--------------------------------------------------------------------------
-- | Convert a declaration to an elm Definition
-- Only works on certain types of declarations TODO document which

translateDef :: Dec -> SQ E.Def

--TODO functions
translateDef (ValD pat body whereDecs) = do
    (ePat, asDecs) <- translatePattern pat
    eWhere <- (asDecs ++ ) <$> mapM translateDef whereDecs
    decBody <- translateBody body
    let eBody = maybeLet eWhere decBody
    return $ E.Definition ePat (Lo.none eBody) Nothing

--To do functions, we translate them into an Elm declaration
--Then we convert
translateDef (funD@(FunD _ _)) = do
  elmDec <- translateDec funD
  case elmDec of
       [D.Definition elmDef] -> return elmDef
       _ -> unImplemented "Function can't be converted to a declaration"
  

translateDef d = unImplemented "Non-simple function/value definitions"

-- | Helper to put an object in a tuple with an empty list as snd
unFst x = (x, [])

--------------------------------------------------------------------------
-- |Translate a pattern match from Haskell to Elm

translatePattern :: Pat -> SQ (P.Pattern, [E.Def])
--Special case for As, to carry over the name
translatePattern (AsP name initPat) = do
  (pat, patExp) <- patToExp initPat
  (retPat, subDecs) <- translatePattern $ pat
  
  dec <- translateDef $ ValD (VarP name) (NormalB patExp) []
  return (retPat, [dec] ++ subDecs)
{-
translatePattern p = do
  runIO $ putStrLn $ show p
  ret <-translatePattern' p
  return (ret, [])
  -}

translatePattern (LitP lit) = (unFst . P.PLiteral) <$> translateLiteral lit

translatePattern (VarP name) = return $ unFst $ P.PVar $ nameToElmString name

--Special case: if only one pattern in tuple, don't treat as tuple
--TODO why do we need this?
translatePattern (TupP [pat]) = translatePattern pat

translatePattern (TupP patList) = do
  (patList, allAsDecs) <- unzip <$> mapM translatePattern patList
  return (P.tuple patList, concat allAsDecs)

--Treat unboxed tuples like tuples
translatePattern (UnboxedTupP patList) = translatePattern $ TupP patList  

translatePattern (ConP name patList) = do
  let str = nameToString name
  (patList, allAsDecs) <- unzip <$> mapM translatePattern patList
  recMap <- records <$> S.get
  if (Map.member str recMap  )
     then do
       let varNames = recMap Map.! str
       let decs = map makeDef $ zip patList varNames
       return (P.PData str $ [P.PRecord varNames], decs ++ (concat allAsDecs))
       
     else do
      return (P.PData (nameToElmString name) patList, concat allAsDecs) 
  where 
    makeDef :: (P.Pattern, String) -> E.Def
    makeDef (pat, varString) = E.Definition pat (Lo.none $ E.Var varString) Nothing
    

--Just pass through parentheses
translatePattern (ParensP p) = translatePattern p
 
--TODO Infix, tilde, bang, as, record,  view


translatePattern WildP = return $ unFst P.PAnything

--Ignore the type signature if theres one in the pattern
translatePattern (SigP pat _) = translatePattern pat

translatePattern (ListP patList) = do
  (patList, allAsDecs) <- unzip <$> mapM translatePattern patList
  return (P.list patList, concat allAsDecs)

--Convert infix patterns to Data patterns, then let Elm decide
-- how it translates them (i.e. cons is a special case)                                                     
translatePattern (InfixP p1 name p2) = translatePattern $ ConP name [p1,p2]
--treat unboxed infix like infix
translatePattern (UInfixP p1 name p2) = translatePattern $ InfixP p1 name p2

--TODO implement records
translatePattern (RecP _ _) = unImplemented "Record patterns"



translatePattern (TildeP _) = unImplemented "Tilde patterns/laziness notation"
translatePattern (BangP _) = unImplemented "Baing patterns/strictness notation"

translatePattern (ViewP _ _) = unImplemented "View patterns"

--translatePattern p = unImplemented $ "Misc patterns " ++ show p

-------------------------------------------------------------------------
-- | Convert a pattern into an expression
-- Useful for as patterns, so we can do pattern checking as well as multiple naming
patToExp :: Pat -> SQ (Pat, Exp)
patToExp p = do
  noWild <- removeWildcards [1..]  p
  return (noWild, patToExp' noWild)
  
  where
    patToExp' (LitP l) = LitE l
    patToExp' (VarP n) = VarE n
    patToExp' (TupP l) = TupE $ map patToExp' l
    patToExp' (UnboxedTupP l) = UnboxedTupE $ map patToExp' l
    patToExp' (ConP n pl) = foldl  AppE (VarE n) (map patToExp' pl) --Apply constructor to each subexp
    patToExp' (InfixP p1 n p2) = InfixE (Just $ patToExp' p1) (VarE n) (Just $ patToExp' p2)
    patToExp' (UInfixP p1 n p2) = UInfixE (patToExp' p1) (VarE n) (patToExp' p2)
    patToExp' (ParensP p) = ParensE $ patToExp' p
    patToExp' (AsP n p) = patToExp' p --TODO ignore name? Should get covered by other translation
    patToExp' WildP = error "Can't use wildcard in expression"
    patToExp' (ListP pList) = ListE $ map patToExp' pList
    patToExp' _ = unImplemented "Complex as-patterns"

doWithNames nameList patList = do
  let nameLists = splitListN (length patList) nameList
  let fnsToApply = [removeWildcards nl | nl <- nameLists]
  let tuples = zip fnsToApply patList
  mapM (\(f,x)-> f $ x) tuples
    
-- | Recursively replace wildcards in an exp with new names
-- Useful for as patterns, so we can unbox patterns and re-pack them with a new name
--Assumes we have an infinite list of names to take
removeWildcards :: [Int] -> Pat -> SQ Pat
removeWildcards (i:_) WildP = do
  name <- liftNewName $ ("arg_" ++ ( show i))
  return $ VarP name
removeWildcards nameList (TupP l) = do
  TupP <$> doWithNames nameList l
removeWildcards nameList (UnboxedTupP l) = UnboxedTupP <$> doWithNames nameList l
removeWildcards nameList (ConP n pl) = (ConP n) <$> doWithNames nameList pl
removeWildcards nameList (InfixP p1 n p2) = do
  let (l1, l2) = splitList nameList
  ret1 <- removeWildcards l1 p1
  ret2 <- removeWildcards l2 p2
  return $ InfixP ret1 n ret2
removeWildcards nameList (UInfixP p1 n p2) = do
  let (l1, l2) = splitList nameList
  ret1 <- removeWildcards l1 p1
  ret2 <- removeWildcards l2 p2
  return $ UInfixP ret1 n ret2
removeWildcards nameList (ParensP p) = ParensP <$> removeWildcards nameList p
removeWildcards nameList (AsP n p) = AsP n <$> removeWildcards nameList p --TODO ignore name? Should get covered by other translation
removeWildcards nameList (ListP pList) = ListP <$> doWithNames nameList pList
removeWildcards nameList p = return p --All other cases, nothing to remove, either simple or unsupported

--------------------------------------------------------------------------
-- |Translate a function body into Elm
translateBody  :: Body -> SQ E.Expr
translateBody (NormalB e) = translateExpression e
--Just convert to a multi-way If statement
translateBody (GuardedB guardExpList) = translateExpression $ MultiIfE guardExpList
  


-- | Expression helper function to convert a Var to a String
expressionToString (VarE name) = nameToElmString name

-- | Generic elm expression for "otherwise"
elmOtherwise = E.Var "otherwise"

-- | Translate a guard into an Elm expression
translateGuard (NormalG exp) = translateExpression exp
translateGuard _ = unImplemented "Pattern-match guards"
--------------------------------------------------------------------------
{-|Translate a haskell Expression into Elm
Currently supported:
  Variables
  Literals
  Lambdas
  Constructors
  Function Application
  Parenthises
  tuples
  Conditionals
  Multi-way If statements
  Let-expressions
  Case expressions
  List literals
  Infix operations
  
Supported but not translated:
  Type signatures
-}
translateExpression :: Exp -> SQ E.Expr

--TODO multi pattern exp?
translateExpression (LamE [pat] expBody) = do
  (ePat, asDecs) <- translatePattern pat
  lambdaBody <- translateExpression expBody
  let eBody = maybeLet asDecs lambdaBody
  return $ E.Lambda ePat (Lo.none eBody)

translateExpression (VarE name) =  return $ E.Var $ nameToElmString name

--Just treat constructor as variable --TODO is this okay?
translateExpression (ConE name) = return $ E.Var $ nameToElmString name

translateExpression (LitE lit) = E.Literal <$> translateLiteral lit

--Lo.none converts expressions to located expressions with no location

--Special case for records, we need a curry-able function to construct records in Elm
translateExpression (AppE fun@(ConE ctor) arg) = do
  recMap <- records <$> S.get
  let str = nameToString ctor
  if Map.member str recMap
     then do
       let recordFunc = mkName $ recordMakerName (nameToString ctor)
       translateExpression (AppE (VarE recordFunc) arg)
     else do
        eFun <- translateExpression fun
        eArg <- translateExpression arg
        return $ E.App (Lo.none eFun) (Lo.none eArg)

translateExpression (AppE fun arg) = do
    eFun <- translateExpression fun
    eArg <- translateExpression arg
    return $ E.App (Lo.none eFun) (Lo.none eArg)

--TODO infix stuff, ranges, record con, record update

translateExpression (ParensE e) = translateExpression e

translateExpression (TupE es) = (E.tuple . map Lo.none) <$> mapM translateExpression es

translateExpression (CondE cond th el) = do
    eCond <- Lo.none <$> translateExpression cond
    eTh <- Lo.none <$> translateExpression th
    eEl <- Lo.none <$> translateExpression el
    let loOtherwise = Lo.none elmOtherwise
    return $ E.MultiIf [(eCond, eTh), (loOtherwise, eEl)]

translateExpression (MultiIfE guardExpList) = do
    expPairs <- mapM transPair guardExpList 
    return $ E.MultiIf expPairs
    where
        transPair (guard, exp) = do
            eGuard <- translateGuard guard
            eExp <- translateExpression exp
            return (Lo.none eGuard, Lo.none eExp)

translateExpression (LetE decList exp) = do
    eDecs <- mapM translateDef decList
    eExp <- translateExpression exp
    return $ E.Let eDecs (Lo.none eExp)

--TODO deal with Where
translateExpression (CaseE exp matchList) = do
    eExp <- translateExpression exp
    eMatch <- mapM getMatch matchList
    return $ E.Case (Lo.none eExp) eMatch
    where
      getMatch (Match pat body whereDecs) = do
        (ePat, asDecs) <- translatePattern pat
        eWhere <- (asDecs ++ ) <$> mapM translateDef whereDecs
        matchBody <- translateBody body 
        let eBody = maybeLet eWhere matchBody
        return (ePat, Lo.none eBody)

translateExpression (ListE exps) = (E.ExplicitList . map Lo.none) <$> mapM translateExpression exps

--Unboxed infix expression
translateExpression (UInfixE e1 op e2) = do
    eE1 <- translateExpression e1
    eE2 <- translateExpression e2
    let eOp =  expressionToString op
    return $ E.Binop eOp (Lo.none eE1) (Lo.none eE2)

--Infix where we have all the parts, i.e. not a section
--Just translate as unboxed
translateExpression (InfixE (Just e1) op (Just e2)) = 
  translateExpression $ UInfixE e1 op e2

translateExpression e@(RecConE name nameExpList ) = do
  let (names, expList) = unzip nameExpList
  eExps <- mapM translateExpression expList
  let stringList = map nameToString names
  let lexps = map Lo.none eExps
  return $ E.App (Lo.none $ E.Var $ nameToString name) (Lo.none $ E.Record $ zip stringList lexps)

translateExpression e@(RecUpdE recExp nameExpList ) = do
  let (names, expList) = unzip nameExpList
  eExps <- mapM translateExpression expList
  let lexps = map Lo.none eExps
  let varStrings = map nameToString names
  eRec <- translateExpression recExp
  recMap <- records <$> S.get
  recName <- nameToString <$> liftNewName "rec"
  let ctor = recordWithFields recMap (map nameToString names)
  let internalRecDef = E.Definition (P.PVar recName) (Lo.none $ E.App (Lo.none $ E.Var $ unboxRecordName ctor) (Lo.none eRec)) Nothing
  return $ E.Let [internalRecDef] $ Lo.none $ E.App (Lo.none $ E.Var ctor) (Lo.none $ E.Modify (Lo.none $ E.Var recName) ( zip varStrings lexps) )
  
translateExpression (InfixE _ _ _) = unImplemented "Operator sections i.e. (+3)"    
    
--Just ignore signature
translateExpression (SigE exp _) = translateExpression exp

translateExpression e@(ArithSeqE r) = translateRange r

translateExpression e@(LamCaseE _) = unImplemented $ "Lambda case expressions: " ++ show e


translateExpression e@(DoE _) = unImplemented $ "Sugared do notation: " ++ show e

translateExpression e@(CompE _) = unImplemented $ "List comprehensions: " ++ show e





translateExpression e = unImplemented $ "Misc expression " ++ show e

--------------------------------------------------------------------------
-- |Translate a literal value from Haskell to Elm
-- Strings are translated into strings, not char lists

translateLiteral :: Lit-> SQ  L.Literal
translateLiteral = return . noQTrans where
    noQTrans (CharL c) = L.Chr c

    noQTrans (StringL s) = L.Str s

    noQTrans (IntegerL i) = L.IntNum $ fromInteger i

    noQTrans (IntPrimL i) =  L.IntNum $ fromInteger i
    
    noQTrans (WordPrimL i) =  L.IntNum $ fromInteger i

    noQTrans (FloatPrimL f) = L.FloatNum $ fromRational f

    noQTrans (DoublePrimL f) = L.FloatNum $ fromRational f

    noQTrans (RationalL f) = L.FloatNum $ fromRational f

    noQTrans (StringPrimL _) = unImplemented "C-string literals"


-- | Translate a Haskell range. Infinite lists not supported, since Elm is strict
translateRange :: Range -> SQ E.Expr
translateRange (FromToR start end) = do
  e1 <- Lo.none <$> translateExpression start
  e2 <- Lo.none <$> translateExpression end
  return $ E.Range e1 e2
  
translateRange _ = unImplemented "Infinite ranges, or ranges with steps not equal to 1"



--------------------------------------------------------------------------
{-|
Translate a Haskell type into an Elm type
Currently translates primitive types, lists, tuples and constructors (ADTs)
Doesn't support type classes or fancier types
-}
translateType :: Type -> SQ T.Type

translateType (ForallT _ _ _ ) = unImplemented "forall types"
translateType (PromotedT _ ) = unImplemented "promoted types"
translateType (PromotedTupleT _ ) = unImplemented "promoted tuple types"
translateType (PromotedNilT ) = unImplemented "promoted nil types"
translateType (PromotedConsT ) = unImplemented "promoted cons types"
translateType (StarT) = unImplemented "star types"
translateType (UnboxedTupleT i ) = translateType $ TupleT i
translateType ArrowT = error "Arrow type: Should never recurse this far down"
translateType ListT = error "List type: Should never recurse this far down"
translateType ConstraintT = unImplemented "Type constraints"
translateType (LitT _) = error "Type literals"


--TODO fill in other cases, esp records
--Cases which aren't captured by basic pattern matching
translateType t = do
  --Unbox some Monad information that we need
  isInt <- S.lift $ isIntType t
  isString <- S.lift $ isStringType t
  isFloat <- S.lift $ isFloatType t
  isBool <- S.lift $ isBoolType t
  generalTranslate isInt isString isFloat isBool --TODO get these in scope
  where
    generalTranslate :: Bool -> Bool -> Bool -> Bool -> SQ T.Type
    generalTranslate isInt isString isFloat isBool
      | isInt = return $ T.Data "Int" []
      | isString = return $ T.Data "String" []
      | isFloat = return $ T.Data "Float" []
      | isBool = return $ T.Data "Bool" []
      | isTupleType t = do
          tyList <- mapM translateType (tupleTypeToList t)
          return $ T.tupleOf tyList
      | otherwise = case t of
          --type variables
          (VarT name) -> return $ T.Var (nameToElmString name)
          --sum types/ADTs
          (ConT name) -> return $ T.Data (nameToElmString name) [] --TODO what is this list param?
          --functions
          (AppT (AppT ArrowT a) b) -> do
            ea <- translateType a
            eb <- translateType b
            return $ T.Lambda ea eb

          --empty tuple/record
          (TupleT 0) -> return $ T.recordOf []
          --Lists and tuples, just Data in Elm
          (AppT ListT t) -> do
            et <- translateType t
            return $ T.listOf et
          --Type variable application: get type to apply to as Data
          --then add this type to the list of applied types
          (AppT subt tvar) -> do
            etvar <- translateType tvar
            T.Data ctor varList <- translateType subt
            return $ T.Data ctor (varList ++ [etvar])                                           


            
-- | Special record type translation
translateRecord :: [(Name, Type)] -> SQ T.Type
translateRecord nameTyList = do
  let (nameList, tyList) = unzip nameTyList
  let eNames = map nameToElmString nameList
  eTypes <- mapM translateType tyList
  return $ T.recordOf $ zip eNames eTypes
  
--Generate the function declarations associated with a record type
accessorDec :: Name -> Name -> D.Declaration
--Names are always local
accessorDec ctor name = 
  let
    nameString = nameToString name
    var = "rec"
    varExp = E.Var var
    varPat = P.PData (nameToString ctor) [P.PVar var]
    funBody = E.Access (Lo.none $ varExp) nameString
    fun = E.Lambda varPat (Lo.none funBody)
  in D.Definition $ E.Definition (P.PVar nameString) (Lo.none fun) Nothing

recordMakerDec :: String -> [String] -> D.Declaration
recordMakerDec ctor vars =
  let
      argNames = map (("arg" ++) . show) [1 .. (length vars)]
      patList = map P.PVar argNames
      expList = map (Lo.none . E.Var) argNames
      recordCons = Lo.none $ E.Record $ zip vars expList
      funBody = E.App (Lo.none $ E.Var ctor) recordCons
      fun = makeCurry patList funBody 
  in D.Definition $ E.Definition (P.PVar $ recordMakerName ctor) (Lo.none fun) Nothing
  where makeCurry argPats body = foldr (\pat body-> E.Lambda pat (Lo.none body) ) body argPats

recordUnboxDec :: String ->  D.Declaration
recordUnboxDec ctor  =
  let
      pat = P.PData ctor [P.PVar "x"]
      body = E.Var "x"
      fun = E.Lambda pat (Lo.none body)
  in D.Definition $ E.Definition (P.PVar $ unboxRecordName ctor) (Lo.none fun) Nothing
  
recordMakerName name =  "makeRecord__" ++ name
unboxRecordName name =  "unboxRecord__" ++ name 
--------------------------------------------------------------------------
{-|
Conversion from Haskell namespaces and prelude names
to Elm base names
-}

nameToElmString :: Name -> String
nameToElmString = getElmName . nameToString

getElmName :: String -> String

getElmName "$"  = "<|"

--Cons should get translated automatically, but just in case
getElmName ":"  = "::"

--Not a change, but lets us search for . in module names
getElmName "." = "."

getElmName "error" = "Error.raise"

--Specific cases
getElmName s
    | length partList > 1 = getElmModuleName modul name
    --Default: don't change the string
    | otherwise = s
    where 
          name = last partList
          modul = init partList
          partList = (splitOn "." s)

--modules that are supported by Elm
elmHasFunction :: String -> String -> Bool
elmHasFunction "Dict" s = s `elem` ["empty", "singleton", "insert", "update", "remove", "member", "lookup", "findWithDefault",
                            "union", "intersect", "diff", "keys", "values", "toList", "fromList", "map", "foldl", "foldr"]

elmHasFunction "Json" s = s `elem` ["String", "Number", "Boolean", "Null", "Array", "Object"]  

elmHasFunction "Error" s = s `elem` ["raise"]                                                      

elmHasFunction _ _ = False   

directTranslate "Dict" s = 
  case Map.lookup s m of
       Just ret -> ret
       Nothing -> error $ "Elm Dictionary operation not supported: " ++ s
  where m = Map.fromList [("Map", "Dict")] --TODO more
    
getElmModuleName :: [String] -> String -> String    
--TODO fix infix?    
getElmModuleName ["Data", "Map"] name = "Dict." ++ case name of --TODO are there any special cases?
    _ -> if (elmHasFunction "Dict" name)
        then name
        else directTranslate "Dict" name

getElmModuleName ["Data", "Aeson"] s = "Json." ++ case s of
    _ -> if (elmHasFunction "Json" s) 
            then s
            else error "Elm Dictionary doesn't support operation " ++ s
getElmModuleName m s = (intercalate "." m) ++ "." ++ s
