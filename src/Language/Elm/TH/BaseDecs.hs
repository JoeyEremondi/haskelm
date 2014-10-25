 {-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Language.Elm.TH.BaseDecs where

--import AST.Annotation
import qualified AST.Module as Module
import qualified Data.Map as Map
import qualified Parse.Parse as Parse
import qualified Data.Text as Text
import qualified Text.PrettyPrint as Pretty

import Text.QuasiText

--import Data.FileEmbed

baseDecsElm :: Text.Text
baseDecsElm = [embed|
import Json 
import Dict
import JsonUtils


getCtor = \(Json.Object d) -> case Dict.get "tag" d of
                           Just (Json.String c) -> c
varNamed = \(Json.Object d) n -> case Dict.get (show n) d of
                              Just val -> val
mapJson = \f (Json.Array l) -> map f l
makeList = \(Json.Array l) -> l
error = \s -> case True of False -> s
    |]

infixes = Map.fromList . map (\(assoc,lvl,op) -> (op,(lvl,assoc))) . concatMap Module.iFixities $ []
--TODO add NthVar to unpack ADTs
baseDecs = case (Parse.program infixes $ Text.unpack baseDecsElm) of
    Left doc -> error $ concatMap Pretty.render doc
    Right modul -> Module.body modul

baseImports = case (Parse.program infixes $ Text.unpack baseDecsElm) of
    Left doc -> error $ concatMap Pretty.render doc
    Right modul -> Module.imports modul

jsonUtilModule = [embed|
module JsonUtils where

{-| General utility functions for working with JSON values.
Primarily intended working with values made or to be read by
Haskell's Aeson library.
Also contains some type definitions which act like ToJson
and FromJson typeclasses.
Note that there is an unenforced dependency on maxsnew/Error.

# Dict classes
@docs ToJson, FromJson

# ToJson and FromJson instances
@docs listToJson, listFromJson, maybeToJson, maybeFromJson,
intToJson, intFromJson, stringToJson, stringFromJson,
boolToJson, boolFromJson, floatToJson, floatFromJson, dictToJson, dictFromJson

# Helper functions
@docs packContents, unpackContents, getTag, varNamed
-}

import Json
import Dict


{-| Given the number of constructors a type has, a constructor name string,
and a list of JSON values to pack,
pack the values into a JSON object representing an ADT,
using the same format as Haskell's Aeson.
-}
packContents : Int -> String -> [Json.Value] -> Json.Value
packContents numCtors name contentList =
  case contentList of
    -- [] -> Json.Null TODO special case for only string
    [item] -> let
          dictList = [("tag", Json.String name), ("contents", item)]
        in Json.Object <| Dict.fromList dictList
    _ ->
      if (numCtors == 0) 
        then Json.Array contentList  
      else
        let
          dictList = [("tag", Json.String name), ("contents", Json.Array contentList)]
        in Json.Object <| Dict.fromList dictList
    
{-| Given the number of constructors a type has, and a JSON value,
get the sub-values wrapped up in that constructor,
assuming the values are packed using the same format as Haskell's Aeson.
-}
unpackContents : Int -> Json.Value -> [Json.Value]
unpackContents numCtors json = case (json, numCtors) of
   (Json.Array contents, 0) -> contents
   --Case when there are no values, just constructor
   (Json.String s, _) -> []
   (Json.Object valDict, _) -> case (Dict.get "contents" valDict) of
      Just (Json.Array contents) -> contents
      --any other case, means we had a single element for contents
      Just json -> [json]
      --_ -> Error.raise <| "No contents field of JSON " ++ (show json)
   --_ -> Error.raise <| "No contents field of JSON. num: " ++ (show numCtors) ++ " json " ++ (show json)

{-| A value of type `ToJson a` is a function converting a value of type `a`` to a `Json.Value`. 
This can be used similarly to typeclasses in Haskell, where a value of this type corresponds to an
instance of a typeclass.
-}  
type ToJson a = (a -> Json.Value)

{-| A function converting from `a` to `Json.Value`. Similar to `ToJson`.
-}
type FromJson a = (Json.Value -> a)
  
{-| Given a ToJson instance for a type, generate a ToJson instance
for a list of that type.
-}
listToJson : ToJson a -> ToJson [a]
listToJson toJson = \values -> Json.Array (map toJson values)

{-| Given a ToJson instance for a type, generate a ToJson instance
for that type wrapped in `Maybe`.
-}
maybeToJson : ToJson a -> ToJson (Maybe a)
maybeToJson toJson = \mval -> case mval of
  Nothing -> Json.Null
  Just a -> toJson a
  
{-| Given a FromJson instance for a type, generate a FromJson instance
for a list of that type.
-}
listFromJson : FromJson a -> FromJson [a]
listFromJson fromJson = \(Json.Array elems) -> map fromJson elems

{-| Given a FromJson instance for a type, generate a FromJson instance
for that type wrapped in `Maybe`.
-}
maybeFromJson : FromJson a -> FromJson (Maybe a)
maybeFromJson fromJson = \json -> case json of
  Json.Null -> Nothing
  _ -> Just <| fromJson json
  
{-| Simple Int from JSON conversion, using `round` -}
intFromJson : FromJson Int
intFromJson (Json.Number f) = round f

{-| Simple Int to JSON conversion -}
intToJson : ToJson Int
intToJson i = Json.Number <| toFloat i

{-| Simple Float from JSON conversion -}
floatFromJson : FromJson Float
floatFromJson (Json.Number f) = f

{-| Simple Float to JSON conversion -}
floatToJson : ToJson Float
floatToJson = Json.Number

{-| Simple String from JSON conversion -}
stringFromJson : FromJson String
stringFromJson (Json.String s) = s

{-| Simple String to JSON conversion -}
stringToJson : ToJson String
stringToJson s = Json.String s

{-| Simple Bool from JSON conversion -}
boolFromJson : FromJson Bool
boolFromJson (Json.Boolean b) = b 

{-| Simple Bool to JSON conversion -}
boolToJson : ToJson Bool
boolToJson b = Json.Boolean b

{-| Given FromJson instances for a comparable key type and some value type,
generate the conversion from a JSON object do a Dict mapping keys to values.
Assumes the JSON values represents a list of pairs.
-}
dictFromJson : FromJson comparable -> FromJson b -> FromJson (Dict.Dict comparable b)
dictFromJson keyFrom valueFrom = \(Json.Array tuples) ->
  let unJsonTuples = map (\ (Json.Array [kj,vj]) -> (keyFrom kj, valueFrom vj)) tuples 
  in Dict.fromList unJsonTuples

{-| Given ToJson instances for a comparable key type and some value type,
generate the conversion from a Dict mapping keys to values to a JSON object.
Represents the Dict as a list of pairs.
-}  
dictToJson : ToJson comparable -> ToJson b -> ToJson (Dict.Dict comparable b)
dictToJson keyTo valueTo = \dict ->
  let 
    dictList = Dict.toList dict
    tupleJson = map (\(k,v) -> Json.Array [keyTo k, valueTo v]) dictList 
  in Json.Array tupleJson

{-| From a Json Object, get a string from a field named "tag".
Fails using `Error.raise` if no such field exists. 
Useful for extracting values from Haskell's Aeson instances.
-}
getTag : Json.Value -> String
getTag json = case json of
  (Json.Object dict) -> case (Dict.get "tag" dict) of
    Just (Json.String s) -> s
    -- _ -> Error.raise <| "Couldn't get tag from JSON" ++ (show dict)
  (Json.String s) -> s --Ctors with no contents get stored as strings
  
{-| From a Json Object, get a value from a field with the given name.
Fails using `Error.raise` if no such field exists. 
Useful for extracting values from Haskell's Aeson instances.
-}
varNamed : Json.Value -> String -> Json.Value
varNamed (Json.Object dict) name = case (Dict.get name dict) of
  Just j -> j

    |]