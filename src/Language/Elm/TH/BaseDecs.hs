module Language.Elm.TH.BaseDecs where

--import AST.Annotation
import AST.Expression.General
import AST.Expression.Canonical
import AST.Declaration
import AST.Literal
import AST.Pattern
import AST.Annotation

--TODO add NthVar to unpack ADTs
baseDecs = [AST.Declaration.Definition
          
          (AST.Expression.Canonical.Definition
             (AST.Pattern.Var "getCtor")
             (AST.Annotation.A
                (AST.Annotation.Span
                   (AST.Annotation.Position (1) (1))
                   (AST.Annotation.Position (1) (1)) (""))
                (AST.Expression.General.Lambda
                   (AST.Pattern.Data
                      "Json.Object" [AST.Pattern.Var "d"])
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
                                        (AST.Expression.General.Var "Dict.lookup"))
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
                                  (AST.Expression.General.Var "d"))))
                         [(AST.Pattern.Data
                             "Just"
                             [AST.Pattern.Data
                                "Json.String"
                                [AST.Pattern.Var "c"]],AST.Annotation.A
                                                                   (AST.Annotation.Span
                                                                      (AST.Annotation.Position
                                                                         (1) (1))
                                                                      (AST.Annotation.Position
                                                                         (1) (1))
                                                                      (""))
                                                                   (AST.Expression.General.Var
                                                                      "c"))]))))
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
                      "Json.Object" [AST.Pattern.Var "d"])
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
                                              (AST.Expression.General.Var "Dict.lookup"))
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
                                                    (AST.Expression.General.Var "show"))
                                                 (AST.Annotation.A
                                                    (AST.Annotation.Span
                                                       (AST.Annotation.Position
                                                          (1) (1))
                                                       (AST.Annotation.Position
                                                          (1) (1))
                                                       (""))
                                                    (AST.Expression.General.Var "n"))))))
                                     (AST.Annotation.A
                                        (AST.Annotation.Span
                                           (AST.Annotation.Position (1) (1))
                                           (AST.Annotation.Position (1) (1))
                                           (""))
                                        (AST.Expression.General.Var "d"))))
                               [(AST.Pattern.Data
                                   "Just" [AST.Pattern.Var "val"],AST.Annotation.A
                                                                              (AST.Annotation.Span
                                                                                 (AST.Annotation.Position
                                                                                    (1)
                                                                                    (1))
                                                                                 (AST.Annotation.Position
                                                                                    (1)
                                                                                    (1))
                                                                                 (""))
                                                                              (AST.Expression.General.Var
                                                                                 "val"))]))))))
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
                            "Json.Array" [AST.Pattern.Var "l"])
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
                                        (AST.Expression.General.Var "map"))
                                     (AST.Annotation.A
                                        (AST.Annotation.Span
                                           (AST.Annotation.Position (1) (1))
                                           (AST.Annotation.Position (1) (1))
                                           (""))
                                        (AST.Expression.General.Var "f"))))
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
