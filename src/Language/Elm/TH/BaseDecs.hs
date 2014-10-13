module Language.Elm.TH.BaseDecs where

--import AST.Annotation
import AST.Expression.General
import AST.Expression.Canonical
import AST.Declaration
import AST.Literal
import AST.Pattern
import AST.Annotation
import AST.Variable

--TODO add NthVar to unpack ADTs
baseDecs = []

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
