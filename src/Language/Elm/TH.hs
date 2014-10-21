-- |
-- Module: Language.Elm.TH
-- Copyright: (c) 2014 Joey Eremondi
-- License: BSD3
-- Maintainer: Joey Eremondi <jmitdase@gmail.com>
-- Stability: experimental
-- Portability: portable
-- 
-- The given functions can be used to convert Haskell source code
-- into Elm source code.
-- 
-- Example usage:
-- 
-- >  elmSource = $(translateToElm defaultOptions "path/to/myFile.hs")
-- 
-- Here, `elmString1` will be a String variable which you can use in your Haskell code.
-- Note that the Haskell functions in the file you give are not imported.
-- If you would like to use them, you must import them the normal way.
-- 
-- Haskelm can currently translate most basic Haskell, including functions, algebraic data types, newtypes, and type synonyms.
-- Support is now in place for records, guarded-function-bodies, list-ranges, where-declarations, as-patterns, 
-- and multi-clause function definitions (pattern matching).
-- 
-- Translation of class or instance declarations is not supported, and will not likely be supported in the near future,
-- as Elm does not support Type classes.
-- However, if your Haskell code contains Class or Instance declarations,
-- they will simply be ignored by Haskelm.
-- 
-- Most GHC extensions are unsupported, with the exception of Multi-Way-If statements,
-- since they have a direct translation into Elm.
-- 
-- 
-- If JSON deriving is enabled, in addition to translating Haskell functions and types,
-- Elm functions will be generated to transform data to and from the JSON format.
-- This follows the format used by Data.Aeson.TH, so you can automatically derive your Haskell JSON definitions.
-- For a type FOO, the functions `toJson_FOO` and `fromJson_FOO` will be added to the Elm code
-- Returning and taking values of type Json.JsonValue respectively.
--
-- The module contains instances of ToJSON and FromJSON for `Data.Map.Map`
-- which match the format used by Elm's JsonUtils
-- 
-- If you use the JSON functionality, the generated Elm code will depend on the `JsonUtils` library,
-- which can be obtained from <http://library.elm-lang.org>. 


module Language.Elm.TH
    ( 
    translateToElm,
    TranslateOptions (..),
    ToJSON (..),
    FromJSON (..),
    defaultOptions,

    ) where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH
import qualified Data.Text as TS
import AST.Declaration as D
import AST.Module as M
import AST.Variable as V

import qualified Language.Elm.TH.BaseDecs as BaseDecs
import Language.Haskell.TH.Lib
import qualified Language.Elm.TH.HToE as HToE
import qualified Language.Elm.TH.Json as Json
import qualified Language.Elm.TH.Util as Util
import Data.List (intercalate)
import AST.PrettyPrint as Pretty
import Control.Monad.State (evalStateT)
import Control.Applicative ((<$>))
--source parser
import Language.Haskell.Meta.Parse
import Language.Haskell.Exts.Pretty (prettyPrint)
import qualified Language.Haskell.Exts.Syntax as Exts
import Data.Aeson (ToJSON, FromJSON, parseJSON, toJSON, fromJSON)
import qualified Data.Map

-- | Options for how to generate Elm source code
data TranslateOptions = Options {
 --
 makeJson :: Bool,
 -- ^ When true, generates `toJson` and `fromJson` for translated type declarations.
 -- The format used by the Json is the same as the one used by Data.Aeson.TH.
 -- This is handy for passing data between a Haskell server and an Elm client.
 qualifiedImports :: [String],
 -- ^ Each module name given will be imported in Elm by `import Module`
 openImports :: [String],
 -- ^ Each module name given will be imported in Elm by `import Module (..)`
 moduleName :: String
 -- ^ The name of the elm module generated. i.e. prepends `module ModuleName` to the generated Elm source.
}

{- |
Default options for translation:
Generates `toJson` and `fromJson` functions,
has no open or qualified imports, and has
module name `Main`.

-}
defaultOptions = Options True [] [] "Main"


-- | 'toElm' takes a 'String' module name and a list of Template Haskell declarations
-- and generates a translated Elm AST module
toElm :: TranslateOptions -> [Dec] -> Q (M.SourceModule)
toElm options decs = do
  let doJson = makeJson options
  fromJsonDecs <- if doJson then evalStateT  (Json.makeFromJson decs) Util.defaultState else return []
  toJsonDecs <- if doJson then evalStateT  (Json.makeToJson decs) Util.defaultState else return []
  let jsonDecs = fromJsonDecs ++ toJsonDecs
  --sumDecs <- evalStateT  (Json.giantSumType decs) Util.defaultState
  elmDecs <- evalStateT  (concat <$> translateDecs (decs ++ jsonDecs)  ) Util.defaultState
  let importList = map (\im->(im, M.importing [])) $ qualifiedImports options
  let openImportList = map (\im->(im, M.open )) $ openImports options
  return $ M.Module [moduleName options] "" (V.openListing)  (importList ++ openImportList) elmDecs 

--Single stateful computation to store record state information  
translateDecs decs = do
  HToE.findRecords decs
  mapM HToE.translateDec decs
  
-- | Given a module name and a list of template-haskell declarations
-- | translate the declarations into Elm and return the string of the translated module
toElmString :: TranslateOptions -> [Dec] -> Q String
toElmString options decs = elmModuleToString <$> toElm options decs
  

-- | Translate a Haskell string into a list of Template-Haskell declarations
decsFromString :: String -> Q [Dec]
decsFromString s = case parseDecs s of
    Left e -> error $ "Failed to parse module\n" ++ e
    Right decs -> return decs

  
--TODO also generate options?
decsFromModuleString :: String -> DecsQ
decsFromModuleString source = case parseHsModule source of
    Left e -> error $ "Failed to parse module\n" ++ e
    Right (Exts.Module _ _ _ _ _ _ decs) -> do
      let decString = intercalate "\n" $ map prettyPrint decs
      decsFromString decString

decsFromModuleFile :: String -> DecsQ
decsFromModuleFile filePath = do
  decString <- runIO $ readFile filePath
  decsFromModuleString decString


elmModuleToString :: M.SourceModule -> String
elmModuleToString (M.Module [name] path exports imports elmDecs ) =
  let allDecs = BaseDecs.baseDecs ++ elmDecs 
      allImports = BaseDecs.baseImports --TODO --imports ++ [("Json", M.As "Json"), ("Dict", M.As "Dict"), ("JsonUtil", M.As "JsonUtil"), ("Error", M.As "Error")]
      newModule = M.Module [name] path exports allImports allDecs
      modString =  Pretty.renderPretty newModule
  in modString              
               

-- | Given options for translation, and the file path of a Haskell module,
-- generate the String literal which is the corresponding Elm source code.
-- This must be invoked using Template Haskell
-- For example: 
--
-- >  elmSource = $(translateToElm defaultOptions "path/to/myFile.hs")
translateToElm :: TranslateOptions -> FilePath -> ExpQ
translateToElm options filePath = do
  decs <- decsFromModuleFile filePath
  elmString <- toElmString options decs
  liftString elmString

-- | A string containing the JsonUtil module. This can be installed via elm-get, or you can use this string
jsonUtilModule = BaseDecs.jsonUtilModule


-- | ToJSON instance for Data.Map which matches the format used by Elm's JsonUtils    
instance (ToJSON a, ToJSON b, Ord a) => ToJSON (Data.Map.Map a b) where
  toJSON m = toJSON $ Data.Map.toList m
  
-- | FromJSON instance for Data.Map which matches the format used by Elm's JsonUtils    
instance (FromJSON a, FromJSON b, Ord a) => FromJSON (Data.Map.Map a b) where
  parseJSON json = Data.Map.fromList <$> parseJSON json
