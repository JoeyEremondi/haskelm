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
import qualified AST.Expression.Source as S
import qualified AST.Literal as L
import qualified AST.Annotation as Lo
import qualified AST.Pattern as P
import qualified AST.Type as T
import qualified AST.Variable as V

import Data.List (isPrefixOf)

import Language.Haskell.TH.Desugar.Sweeten
import Language.Haskell.TH.Desugar

import Language.Elm.TH.Util
--import ParsS.Expression (makeFunction)

import Control.Applicative

import Data.List.Split (splitOn)

import Control.Monad.State (StateT)
import qualified Control.Monad.State as S

import qualified Data.Map as Map
import Data.List (intercalate)


{-|
Haskell to Elm Translations
Most of these functions operate in the SQ monad, so that we can
compare against Haskell expressions or types in quotes (see isIntType etS.)

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
translateCtor :: Con -> SQ ( (String,[T.RawType]), [D.SourceDecl])
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
maybeLet :: [S.Def] -> S.Expr -> S.Expr 
maybeLet eWhere eBody = 
        if null eWhere
          then  eBody
          else Lo.none (E.Let eWhere eBody)

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

translateDec:: Dec -> SQ [D.SourceDecl]

--TODO translate where decs into elm let-decs
--TODO what about when more than one clause?
translateDec (FunD name [Clause patList body whereDecs])  =  do
    let eName = nameToElmString name
    (ePats, asDecList) <- unzip <$> mapM translatePattern patList
    let asDecs = concat asDecList
    eWhere <- mapM translateDef whereDecs
    let eDecs = asDecs ++ eWhere
    fnBody <- translateBody body
    let eBody = maybeLet eDecs fnBody
    return $ single $ D.Definition $ S.Definition (P.Var eName) (makeFunction ePats  eBody) 
    
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
    
    return $ single $ D.Definition $ S.Definition ePat eBody


translateDec dec@(DataD [] name tyBindings ctors names) = do
    --jsonDecs <- deriveFromJSON defaultOptions name
    (eCtors, extraDecLists) <- unzip <$> mapM translateCtor ctors
    return $ [ D.Datatype eName eTyVars eCtors ] ++ (concat extraDecLists) --TODO derivations?
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
    return $ single $ D.TypeAlias eName eTyVars eTy 

translateDec (ClassD cxt name tyBindings funDeps decs ) = doEmitWarning "Class definitions"
translateDec (InstanceD cxt ty decs) = doEmitWarning "Instance declarations"

--TODO fix signatures
translateDec (SigD name ty) = return []--(single . D.Definition . (E.TypeAnnotation (nameToString name)) ) <$> translateType ty
translateDec (ForeignD frn) = doEmitWarning "FFI declarations"


translateDec (PragmaD pragma)  = doEmitWarning "Haskell Pragmas"


translateDec (FamilyD famFlavour name [tyVarBndr] mKind) = doEmitWarning "Type families"

translateDec (DataInstD cxt name types ctors names) = doEmitWarning "Data instances"


translateDec (NewtypeInstD cxt name types ctor names) = doEmitWarning "Newtypes instances"

--TODO check
--translateDec (TySynInstD name types theTy) = doEmitWarning "Type synonym instances"

--------------------------------------------------------------------------
-- | Convert a declaration to an elm Definition
-- Only works on certain types of declarations TODO document which

translateDef :: Dec -> SQ S.Def

--TODO functions
translateDef (ValD pat body whereDecs) = do
    (ePat, asDecs) <- translatePattern pat
    eWhere <- (asDecs ++ ) <$> mapM translateDef whereDecs
    decBody <- translateBody body
    let eBody = maybeLet eWhere decBody
    return $ S.Definition ePat eBody

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
--Helper for dealing with infix patterns

--TODO better solution for list of names and ops?
data LinearInfix = UIPat Pat
                  | UIExp Exp
                  | UIOp Name
  deriving (Eq, Show)
                  
data Associativity = LeftAssoc | RightAssoc | NonAssoc
  deriving (Eq, Show)

linearizeInfixPat :: Pat -> [LinearInfix]
linearizeInfixPat p@(UInfixP p1 name p2) = 
    (linearizeInfixPat p1) ++ [UIOp name] ++ (linearizeInfixPat p2)
linearizeInfixPat p = [UIPat p]

linearizeInfixExp :: Exp -> [LinearInfix]
linearizeInfixExp e@(UInfixE e1 (VarE name) e2) = 
  (linearizeInfixExp e1) ++ [UIOp name] ++ (linearizeInfixExp e2)
linearizeInfixExp e@(UInfixE e1 (ConE name) e2) = 
  (linearizeInfixExp e1) ++ [UIOp name] ++ (linearizeInfixExp e2)
linearizeInfixExp e = [UIExp e]

getPrecedence :: LinearInfix -> (Int, Associativity)
getPrecedence (UIOp op) = case (nameToString op) of
  "!!" -> (9, LeftAssoc)
  "." -> (9, RightAssoc)
  "^" -> (8, RightAssoc)
  "^^" -> (8, RightAssoc)
  "**" -> (8, RightAssoc)
  "*" -> (7, LeftAssoc)
  "/" -> (7, LeftAssoc) 
  "`div`" -> (7, LeftAssoc)
  "`mod`" -> (7, LeftAssoc)
  "`rem`" -> (7, LeftAssoc)
  "`quot`" -> (7, LeftAssoc)
  "+" -> (6, LeftAssoc)
  "-" -> (6, LeftAssoc)
  ":" -> (5, RightAssoc)
  "++" -> (5, RightAssoc)
  "==" -> (4, NonAssoc)
  "/=" -> (4, NonAssoc)
  "<" -> (4, NonAssoc)
  "<=" -> (4, NonAssoc)
  ">" -> (4, NonAssoc)
  ">=" -> (4, NonAssoc)
  "`elem`" -> (4, NonAssoc)
  "`notElem`" -> (4, NonAssoc)
  "&&" -> (3, RightAssoc)
  "||" -> (2, RightAssoc)
  ">>" -> (1, LeftAssoc)
  ">>=" -> (1, LeftAssoc)
  "$" -> (0, RightAssoc)
  "$!" -> (0, RightAssoc)
  "`seq`" -> (0, RightAssoc)
  
 
--If not an infix operator, has precedence -1, so we never split our list on a non-operator
getPrecedence _ = (-1, NonAssoc)

reassocPat :: [LinearInfix] -> Pat

--Base case: only one op
reassocPat [UIPat p] = p

reassocPat lin = InfixP leftPat op rightPat
  where
    precs = map getPrecedence lin
    maxPrec = maximum (fst $ unzip precs)
    --Left most op with highest precedence
    leftMostOp = head $ filter ((== maxPrec) . fst . getPrecedence) lin
    leftPartition [p1, UIOp name, p2] =  ([p1], name, [p2])
    leftPartition (p1: (op@(UIOp name) ) :t) = 
        if (leftMostOp == UIOp name)
        then ([p1], name, t)
        else let 
            (subLeft, subOp, subRight) = leftPartition t
          in (p1:op:subLeft, subOp, subRight)
    --Find the op we split on, and look at its associativity
    --Which decides if we split on left or rightmost occurrence of that op
    rightPartition l = let
        (revR, op, revL) = leftPartition $ reverse l
      in (reverse revL, op, reverse revR)
    (leftList, op, rightList) = case (snd $ getPrecedence leftMostOp) of
      LeftAssoc -> rightPartition lin
      _ -> leftPartition lin
    leftPat = reassocPat leftList
    rightPat = reassocPat rightList
    
      
reassocExp :: [LinearInfix] -> Exp
reassocExp [UIExp e] = e
reassocExp lin = InfixE (Just leftExp) (VarE op) (Just rightExp)
  where
    precs = map getPrecedence lin
    maxPrec = maximum (fst $ unzip precs)
    --Left most op with highest precedence
    leftMostOp = head $ filter ((== maxPrec) . fst . getPrecedence) lin
    leftPartition [p1, UIOp name, p2] = ([p1], name, [p2])
    leftPartition (e1: (op@(UIOp name) ) :t) = 
         if (leftMostOp == UIOp name)
        then ([e1], name, t)
        else let 
            (subLeft, subOp, subRight) = leftPartition t
          in (e1:op:subLeft, subOp, subRight)
    --leftPartition x = error $ show x
    --Find the op we split on, and look at its associativity
    --Which decides if we split on left or rightmost occurrence of that op
    rightPartition l = let
        (revR, op, revL) = leftPartition $ reverse l
      in (reverse revL, op, reverse revR)
    (leftList, op, rightList) = case (snd $ getPrecedence leftMostOp) of
      LeftAssoc -> rightPartition lin --Left associative means start with right-most operator
      _ -> leftPartition lin
    leftExp = reassocExp leftList
    rightExp = reassocExp rightList    
    

-- |Translate a pattern match from Haskell to Elm

translatePattern :: Pat -> SQ (P.RawPattern, [S.Def])
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

translatePattern (LitP lit) = (unFst . P.Literal) <$> translateLiteral lit

translatePattern (VarP name) = return $ unFst $ P.Var $ nameToElmString name

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
       return (P.Data (V.Raw str) $ [P.Record varNames], decs ++ (concat allAsDecs))
       
       
     else do
      return (P.Data (V.Raw $ nameToElmString name) patList, concat allAsDecs) 
  where 
    makeDef :: (P.RawPattern, String) -> S.Def
    makeDef (pat, varString) = S.Definition pat (Lo.none $ E.Var $ V.Raw varString)
    

--Just pass through parentheses
translatePattern (ParensP p) = translatePattern p
 
--TODO Infix, tilde, bang, as, record,  view


translatePattern WildP = return $ unFst P.Anything

--Ignore the type signature if theres one in the pattern
translatePattern (SigP pat _) = translatePattern pat

translatePattern (ListP patList) = do
  (patList, allAsDecs) <- unzip <$> mapM translatePattern patList
  return (P.list patList, concat allAsDecs)

--Convert infix patterns to Data patterns, then let Elm decide
-- how it translates them (i.e. cons is a special case)                                                     
translatePattern p@(InfixP p1 name p2) = case (nameToString name) of
  ":" -> do
        (ep1, sub1) <- translatePattern p1
        (ep2, sub2) <- translatePattern p2
        return $ (P.Data (V.Raw "::") [ep1, ep2], sub1 ++ sub2) --TODO make cons constant somewhere
  _ -> translatePattern $ ConP name [p1,p2]
--treat unboxed infix like infix
translatePattern p@(UInfixP p1 name p2) = 
  translatePattern $ reassocPat $ linearizeInfixPat p

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
  noWild <-  removeWildcards [1..]  p
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
translateBody  :: Body -> SQ S.Expr
translateBody (NormalB e) = translateExpression e
--Just convert to a multi-way If statement
translateBody (GuardedB guardExpList) = translateExpression $ MultiIfE guardExpList
  


-- | Expression helper function to convert a Var to a String
expressionToString (VarE name) = nameToElmString name
expressionToString (ConE name) = nameToElmString name

expressionToString x = error $ "Can't convert " ++ (show x) ++ " to string"

-- | Generic elm expression for "otherwise"
elmOtherwise :: S.Expr
elmOtherwise = Lo.none $ E.Var $ V.Raw "otherwise"

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
translateExpression :: Exp -> SQ S.Expr

--TODO multi pattern exp?
translateExpression (LamE [pat] expBody) = do
  (ePat, asDecs) <- translatePattern pat
  lambdaBody <- translateExpression expBody
  let eBody = maybeLet asDecs lambdaBody
  return $ Lo.none $ E.Lambda ePat ( eBody)

translateExpression (VarE name) =  return $ Lo.none $ E.Var $ V.Raw $ nameToElmString name

--Just treat constructor as variable --TODO is this okay?
translateExpression (ConE name) = return $ Lo.none $ E.Var $ V.Raw $ nameToElmString name

translateExpression (LitE lit) = (Lo.none . E.Literal) <$> translateLiteral lit

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
        return $ Lo.none $ E.App (eFun) ( eArg)

translateExpression (AppE fun arg) = do
    eFun <- translateExpression fun
    eArg <- translateExpression arg
    return $ Lo.none $ E.App ( eFun) (eArg)

--TODO infix stuff, ranges, record con, record update

translateExpression (ParensE e) = translateExpression e

translateExpression (TupE es) = (Lo.none . E.tuple ) <$> mapM translateExpression es

translateExpression (CondE cond th el) = do
    eCond <-  translateExpression cond
    eTh <-  translateExpression th
    eEl <-  translateExpression el
    let loOtherwise =  elmOtherwise
    return $ Lo.none $ E.MultiIf [(eCond, eTh), (elmOtherwise, eEl)]

translateExpression (MultiIfE guardExpList) = do
    expPairs <- mapM transPair guardExpList 
    return $ Lo.none $ E.MultiIf expPairs
    where
        transPair (guard, exp) = do
            eGuard <- translateGuard guard
            eExp <- translateExpression exp
            return ( eGuard,  eExp)

translateExpression (LetE decList exp) = do
    eDecs <- mapM translateDef decList
    eExp <- translateExpression exp
    return $ Lo.none $ E.Let eDecs ( eExp)

--TODO deal with Where
translateExpression (CaseE exp matchList) = do
    eExp <- translateExpression exp
    eMatch <- mapM getMatch matchList
    return $ Lo.none $ E.Case ( eExp) eMatch
    where
      getMatch (Match pat body whereDecs) = do
        (ePat, asDecs) <- translatePattern pat
        eWhere <- (asDecs ++ ) <$> mapM translateDef whereDecs
        matchBody <- translateBody body 
        let eBody = maybeLet eWhere matchBody
        return (ePat,  eBody)

translateExpression (ListE exps) = ( Lo.none . E.ExplicitList ) <$> mapM translateExpression exps

--Unboxed infix expression
--Haskell treats list  cons as binop, but Elm treats it as Constructor
translateExpression (InfixE (Just e1) op (Just e2)) =  do
    eE1 <- translateExpression e1
    eE2 <- translateExpression e2
    let opString =  expressionToString op
    case opString of
      "::" -> return $ Lo.none $ E.Data ( opString) [eE1, eE2]
      _ -> return $ Lo.none $ E.Binop (V.Raw opString) eE1 eE2

--Infix where we have all the parts, i.e. not a section
--Just translate as unboxed
translateExpression e@(UInfixE e1 op e2) = let
    lexp = linearizeInfixExp e
    rexp = reassocExp lexp
    texp = translateExpression  rexp
  in  texp

translateExpression e@(RecConE name nameExpList ) = do
  let (names, expList) = unzip nameExpList
  eExps <- mapM translateExpression expList
  let stringList = map ( nameToString ) names
  let lexps =  eExps
  return $ Lo.none $ E.App ( Lo.none $ E.Var $ V.Raw $ nameToString name) ( Lo.none $ E.Record $ zip stringList lexps)

translateExpression e@(RecUpdE recExp nameExpList ) = do
  let (names, expList) = unzip nameExpList
  eExps <- mapM translateExpression expList
  let lexps =  eExps
  let varStrings = map ( nameToString) names
  eRec <- translateExpression recExp
  recMap <- records <$> S.get
  recName <- nameToString <$> liftNewName "rec"
  let ctor = recordWithFields recMap (map nameToString names)
  let internalRecDef = S.Definition (P.Var recName) (Lo.none $ E.App (Lo.none $ E.Var $ V.Raw $ unboxRecordName ctor) (eRec))
  return $ Lo.none $  E.Let [internalRecDef] $ Lo.none $ E.App (Lo.none $ E.Var $ V.Raw ctor) (Lo.none $ E.Modify (Lo.none $ E.Var $ V.Raw recName) ( zip varStrings lexps) )
  
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
translateRange :: Range -> SQ S.Expr
translateRange (FromToR start end) = do
  e1 <-  translateExpression start
  e2 <-  translateExpression end
  return $ Lo.none $ E.Range e1 e2
  
translateRange _ = unImplemented "Infinite ranges, or ranges with steps not equal to 1"



--------------------------------------------------------------------------
{-|
Translate a Haskell type into an Elm type
Currently translates primitive types, lists, tuples and constructors (ADTs)
Doesn't support type classes or fancier types
-}
translateType :: Type -> SQ T.RawType

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
    generalTranslate :: Bool -> Bool -> Bool -> Bool -> SQ T.RawType
    generalTranslate isInt isString isFloat isBool
      | isInt = return $ T.Type (V.Raw "Int")
      | isString = return $ T.Type (V.Raw "String")
      | isFloat = return $ T.Type (V.Raw "Float")
      | isBool = return $ T.Type (V.Raw "Bool")
      | isTupleType t = do
          tyList <- mapM translateType (tupleTypeToList t)
          return $ T.tupleOf tyList
      | otherwise = case t of
          --type variables
          (VarT name) -> return $ T.Var (nameToElmString name)
          --sum types/ADTs
          (ConT name) -> return $ T.Type (V.Raw $ nameToElmString name) --TODO what is this list param?
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
            esubt <- translateType subt
            return $ T.App esubt [etvar]                                            


            
-- | Special record type translation
translateRecord :: [(Name, Type)] -> SQ T.RawType
translateRecord nameTyList = do
  let (nameList, tyList) = unzip nameTyList
  let eNames = map nameToElmString nameList
  eTypes <- mapM translateType tyList
  return $ T.recordOf $ zip eNames eTypes
  
--Generate the function declarations associated with a record type
accessorDec :: Name -> Name -> D.SourceDecl
--Names are always local
accessorDec ctor name = 
  let
    nameString = nameToString name
    var = "rec"
    rawVar = V.Raw var
    varExp =  (E.Var rawVar)  
    varPat = (P.Data (V.Raw $  nameToString ctor) [P.Var var])::  P.RawPattern
    funBody =  Lo.none (E.Access ( Lo.none varExp) nameString ) 
    fun = Lo.none (E.Lambda varPat funBody) 
  in D.Definition $ S.Definition (P.Var  nameString) ( fun) 

recordMakerDec :: String -> [String] -> D.SourceDecl
recordMakerDec ctor vars =
  let
      argNames = map (("arg" ++) . show) [1 .. (length vars)]
      patList = map P.Var argNames
      expList = (map ( Lo.none . E.Var . V.Raw ) argNames) :: [S.Expr]
      recordCons = (Lo.none $ E.Record $ zip vars expList) :: S.Expr
      funBody = ( E.App (Lo.none $ E.Var $ V.Raw ctor) recordCons) 
      fun = Lo.none $ makeCurry patList funBody 
  in  D.Definition $ S.Definition (P.Var $ recordMakerName ctor) ( fun)
  where makeCurry argPats body = foldr (\pat body-> E.Lambda pat (Lo.none body) ) body argPats

recordUnboxDec :: String ->  D.SourceDecl
recordUnboxDec ctor  =
  let
      pat = P.Data (V.Raw ctor) [P.Var "x"]
      body = E.Var $ V.Raw "x"
      fun = E.Lambda pat (Lo.none body)
  in D.Definition $ S.Definition (P.Var $ unboxRecordName ctor) (Lo.none fun) 
  
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
getElmName "." = "<<"

getElmName "^^" = "^"
getElmName "**" = "^"

getElmName "error" = "Error.raise"

getElmName "id" = "identity"

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
