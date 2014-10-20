import Json 
import Dict

getCtor = \(Json.Object d) -> case Dict.get "tag" d of
                           Just (Json.String c) -> c
varNamed = \(Json.Object d) n -> case Dict.get (show n) d of
                              Just val -> val
mapJson = \f (Json.Array l) -> map f l
makeList = \(Json.Array l) -> l
error = \s -> case True of False -> s