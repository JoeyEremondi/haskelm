{-# OPTIONS_GHC -W #-}
module AST.Module where

import Data.Binary
import qualified Data.List as List
import qualified Data.Map as Map
import Control.Applicative ((<$>),(<*>))

import qualified AST.Expression.Canonical as Canonical
import qualified AST.Declaration as Decl
import qualified AST.Type as Type
import qualified AST.Variable as Var

import AST.PrettyPrint
import Text.PrettyPrint as P

--import qualified Elm.Internal.Version as Version

data Module exs body = Module
    { names   :: [String]
    , path    :: FilePath
    , exports :: exs
    , imports :: [(String, ImportMethod)]
    , body    :: body
    }

getName :: Module exs body -> String
getName modul =
    List.intercalate "." (names modul)

data CanonicalBody = CanonicalBody
    { program   :: Canonical.Expr
    , types     :: Types
    , fixities  :: [(Decl.Assoc, Int, String)]
    , aliases   :: Aliases
    , datatypes :: ADTs
    , ports     :: [String]
    }

type SourceModule    = Module (Var.Listing Var.Value) [Decl.SourceDecl]
type ValidModule     = Module (Var.Listing Var.Value) [Decl.ValidDecl]
type CanonicalModule = Module [Var.Value] CanonicalBody

type Interfaces = Map.Map String Interface

type Types   = Map.Map String Type.CanonicalType
type Aliases = Map.Map String ( [String], Type.CanonicalType )
type ADTs    = Map.Map String (AdtInfo String)

type AdtInfo v = ( [String], [(v, [Type.CanonicalType])] )
type CanonicalAdt = (Var.Canonical, AdtInfo Var.Canonical)

data Interface = Interface
    { iVersion  :: Int
    , iExports  :: [Var.Value]
    , iTypes    :: Types
    , iImports  :: [(String, ImportMethod)]
    , iAdts     :: ADTs
    , iAliases  :: Aliases
    , iFixities :: [(Decl.Assoc, Int, String)]
    , iPorts    :: [String]
    }

toInterface :: CanonicalModule -> Interface
toInterface modul =
    let body' = body modul in
    Interface
    { iVersion  = 0 --Version.elmVersion
    , iExports  = exports modul
    , iTypes    = types body'
    , iImports  = imports modul
    , iAdts     = datatypes body'
    , iAliases  = aliases body'
    , iFixities = fixities body'
    , iPorts    = ports body'
    }

instance Binary Interface where
  get = Interface <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get
  put modul = do
      put (iVersion modul)
      put (iExports modul)
      put (iTypes modul)
      put (iImports modul)
      put (iAdts modul)
      put (iAliases modul)
      put (iFixities modul)
      put (iPorts modul)

data ImportMethod
    = As !String
    | Open !(Var.Listing Var.Value)

open :: ImportMethod
open = Open (Var.openListing)

importing :: [Var.Value] -> ImportMethod
importing xs = Open (Var.Listing xs False)

instance Binary ImportMethod where
    put method =
        case method of
          As alias     -> putWord8 0 >> put alias
          Open listing -> putWord8 1 >> put listing

    get = do tag <- getWord8
             case tag of
               0 -> As   <$> get
               1 -> Open <$> get
               _ -> error "Error reading valid ImportMethod type from serialized string"

instance (Pretty exs, Pretty body) => Pretty (Module exs body) where
  pretty (Module names _ exs ims body) =
      P.vcat [modul, P.text "", prettyImports, P.text "", pretty body]
    where 
      modul = P.text "module" <+> name <+> pretty exs <+> P.text "where"
      name = P.text (List.intercalate "." names)

      prettyImports = P.vcat $ map prettyMethod ims

prettyMethod :: (String, ImportMethod) -> Doc
prettyMethod (name, method) =
    case method of
      As alias
          | name == alias -> P.empty
          | otherwise     -> P.text "as" <+> P.text alias

      Open listing -> pretty listing
