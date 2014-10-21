 {-# LANGUAGE TemplateHaskell #-}
module Language.Elm.TH.BaseDecs where

--import AST.Annotation
import AST.Expression.General
import AST.Expression.Canonical
import AST.Declaration
import AST.Literal
import AST.Pattern
import AST.Annotation
import AST.Variable
import qualified AST.Module as Module
import qualified Data.Map as Map
import qualified Parse.Parse as Parse
import Data.ByteString.UTF8 as UTF8
import qualified Text.PrettyPrint as Pretty

import Data.FileEmbed

baseDecsElm = $(embedFile "BaseDecs.elm")

infixes = Map.fromList . map (\(assoc,lvl,op) -> (op,(lvl,assoc))) . concatMap Module.iFixities $ []
--TODO add NthVar to unpack ADTs
baseDecs = case (Parse.program infixes $ UTF8.toString baseDecsElm) of
    Left doc -> error $ concatMap Pretty.render doc
    Right modul -> Module.body modul

{-
baseDecs = [AST.Declaration.Definition
          
          (AST.Expression.Canonical.Definition
             (AST.Pattern.Var "getCtor")
             (AST.Annotation.A
                (AST.Annotation.Span
                   (AST.Annotation.Position (1) (1))
                   (AST.Annotation.Position (1) (1)) (""))
                (AST.Expression.General.Lambda
                   (AST.Pattern.Data
                      (Canonical Local "Json.Object") [AST.Pattern.Var "d"])
                   (AST.Annotation.A
                      (AST.Annotation.Span
                         (AST.Annotation.Position (1) (1))
                         (AST.Annotation.Position (1) (1)) (""))
                      (AST.Expression.General.Case
                         (AST.Annotation.A
                            (AST.Annotation.None "")
                            (AST.Expression.General.App
                               (AST.Annotation.A
                                  (AST.Annotation.None "")
                                  (AST.Expression.General.App
                                     (AST.Annotation.A
                                        (AST.Annotation.Span
                                           (AST.Annotation.Position (1) (1))
                                           (AST.Annotation.Position (1) (1))
                                           (""))
                                        (AST.Expression.General.Var (Canonical (Module "Dict") "lookup") ))
                                     (AST.Annotation.A
                                        (AST.Annotation.Span
                                           (AST.Annotation.Position (1) (1))
                                           (AST.Annotation.Position (1) (1))
                                           (""))
                                        (AST.Expression.General.Literal
                                           (AST.Literal.Str "tag")))))
                               (AST.Annotation.A
                                  (AST.Annotation.Span
                                     (AST.Annotation.Position (1) (1))
                                     (AST.Annotation.Position (1) (1))
                                     (""))
                                  (AST.Expression.General.Var (Canonical Local "d") ))))
                         [(AST.Pattern.Data
                             (Canonical Local "Just")
                             [AST.Pattern.Data
                                (Canonical (Module "Json") "String")
                                [AST.Pattern.Var "c"]],AST.Annotation.A
                                                                   (AST.Annotation.Span
                                                                      (AST.Annotation.Position
                                                                         (1) (1))
                                                                      (AST.Annotation.Position
                                                                         (1) (1))
                                                                      (""))
                                                                   (AST.Expression.General.Var
                                                                      (Canonical Local "c" ) ))]))))
             Nothing),
        AST.Declaration.Definition
          (AST.Expression.Canonical.Definition
             (AST.Pattern.Var "varNamed")
             (AST.Annotation.A
                (AST.Annotation.Span
                   (AST.Annotation.Position (1) (1))
                   (AST.Annotation.Position (1) (1)) (""))
                (AST.Expression.General.Lambda
                   (AST.Pattern.Data
                      (Canonical Local "Json.Object") [AST.Pattern.Var "d"])
                   (AST.Annotation.A
                      (AST.Annotation.Span
                         (AST.Annotation.Position (1) (1))
                         (AST.Annotation.Position (1) (1)) (""))
                      (AST.Expression.General.Lambda
                         (AST.Pattern.Var "n")
                         (AST.Annotation.A
                            (AST.Annotation.Span
                               (AST.Annotation.Position (1) (1))
                               (AST.Annotation.Position (1) (1)) (""))
                            (AST.Expression.General.Case
                               (AST.Annotation.A
                                  (AST.Annotation.None "")
                                  (AST.Expression.General.App
                                     (AST.Annotation.A
                                        (AST.Annotation.None "")
                                        (AST.Expression.General.App
                                           (AST.Annotation.A
                                              (AST.Annotation.Span
                                                 (AST.Annotation.Position (1) (1))
                                                 (AST.Annotation.Position (1) (1))
                                                 (""))
                                              (AST.Expression.General.Var (Canonical Local "Dict.lookup") ))
                                           (AST.Annotation.A
                                              (AST.Annotation.None "")
                                              (AST.Expression.General.App
                                                 (AST.Annotation.A
                                                    (AST.Annotation.Span
                                                       (AST.Annotation.Position
                                                          (1) (1))
                                                       (AST.Annotation.Position
                                                          (1) (1))
                                                       (""))
                                                    (AST.Expression.General.Var (Canonical Local "show") ))
                                                 (AST.Annotation.A
                                                    (AST.Annotation.Span
                                                       (AST.Annotation.Position
                                                          (1) (1))
                                                       (AST.Annotation.Position
                                                          (1) (1))
                                                       (""))
                                                    (AST.Expression.General.Var (Canonical Local "n") ))))))
                                     (AST.Annotation.A
                                        (AST.Annotation.Span
                                           (AST.Annotation.Position (1) (1))
                                           (AST.Annotation.Position (1) (1))
                                           (""))
                                        (AST.Expression.General.Var (Canonical Local "d") ))))
                               [(AST.Pattern.Data
                                   (Canonical Local "Just") [AST.Pattern.Var "val"],AST.Annotation.A
                                                                              (AST.Annotation.Span
                                                                                 (AST.Annotation.Position
                                                                                    (1)
                                                                                    (1))
                                                                                 (AST.Annotation.Position
                                                                                    (1)
                                                                                    (1))
                                                                                 (""))
                                                                              (AST.Expression.General.Var
                                                                                 (Canonical Local "val") ))]))))))
             Nothing),
        AST.Declaration.Definition
          (AST.Expression.Canonical.Definition
             (AST.Pattern.Var "mapJson")
             (AST.Annotation.A
                (AST.Annotation.None "")
                (AST.Expression.General.Lambda
                   (AST.Pattern.Var "f")
                   (AST.Annotation.A
                      (AST.Annotation.None "")
                      (AST.Expression.General.Lambda
                         (AST.Pattern.Data
                            (Canonical Local "Json.Array") [AST.Pattern.Var "l"])
                         (AST.Annotation.A
                            (AST.Annotation.None "")
                            (AST.Expression.General.App
                               (AST.Annotation.A
                                  (AST.Annotation.None "")
                                  (AST.Expression.General.App
                                     (AST.Annotation.A
                                        (AST.Annotation.Span
                                           (AST.Annotation.Position (1) (1))
                                           (AST.Annotation.Position (1) (1))
                                           (""))
                                        (AST.Expression.General.Var (Canonical Local "map") ))
                                     (AST.Annotation.A
                                        (AST.Annotation.Span
                                           (AST.Annotation.Position (1) (1))
                                           (AST.Annotation.Position (1) (1))
                                           (""))
                                        (AST.Expression.General.Var (Canonical Local "f")  ))))
                               (AST.Annotation.A
                                  (AST.Annotation.Span
                                     (AST.Annotation.Position (1) (1))
                                     (AST.Annotation.Position (1) (1))
                                     (""))
                                  (AST.Expression.General.Var "l"))))))))
             Nothing),
        AST.Declaration.Definition
          (AST.Expression.Canonical.Definition
             (AST.Pattern.Var "makeList")
             (AST.Annotation.A
                (AST.Annotation.Span
                   (AST.Annotation.Position (1) (1))
                   (AST.Annotation.Position (1) (1))
                   (""))
                (AST.Expression.General.Lambda
                   (AST.Pattern.Data
                      "Json.Array" [AST.Pattern.Var "l"])
                   (AST.Annotation.A
                      (AST.Annotation.Span
                         (AST.Annotation.Position (1) (1))
                         (AST.Annotation.Position (1) (1))
                         (""))
                      (AST.Expression.General.Var "l"))))
             Nothing),
        AST.Declaration.Definition
          (AST.Expression.Canonical.Definition
             (AST.Pattern.Var "error")
             (AST.Annotation.A
                (AST.Annotation.Span
                   (AST.Annotation.Position (1) (1))
                   (AST.Annotation.Position (1) (1))
                   (""))
                (AST.Expression.General.Lambda
                   (AST.Pattern.Var "s")
                   (AST.Annotation.A
                      (AST.Annotation.Span
                         (AST.Annotation.Position (1) (1))
                         (AST.Annotation.Position (1) (1))
                         (""))
                      (AST.Expression.General.Case
                         (AST.Annotation.A
                            (AST.Annotation.Span
                               (AST.Annotation.Position (1) (1))
                               (AST.Annotation.Position (1) (1))
                               (""))
                            (AST.Expression.General.Literal
                               (AST.Literal.Boolean True)))
                         [(AST.Pattern.Literal
                             (AST.Literal.Boolean False),AST.Annotation.A
                                                                    (AST.Annotation.Span
                                                                       (AST.Annotation.Position
                                                                          (1) (1))
                                                                       (AST.Annotation.Position
                                                                          (1) (1))
                                                                       (""))
                                                                    (AST.Expression.General.Var
                                                                       "s"))]))))
             Nothing)]
-}
