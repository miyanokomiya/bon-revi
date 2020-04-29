module Request exposing (mutationRequest, queryRequest, withHeader)

import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.SelectionSet exposing (SelectionSet)


path : String
path =
    "https://graphql.fauna.com/graphql"


secret : String
secret =
    "fnADqm2f06ACCVSReqgm7pwizGzleAmVZ3Ev5PWs"


queryRequest : SelectionSet decodesTo RootQuery -> Graphql.Http.Request decodesTo
queryRequest a =
    a
        |> Graphql.Http.queryRequest path


mutationRequest : SelectionSet decodesTo RootMutation -> Graphql.Http.Request decodesTo
mutationRequest a =
    a
        |> Graphql.Http.mutationRequest path


withHeader : Graphql.Http.Request decodesTo -> Graphql.Http.Request decodesTo
withHeader a =
    a
        |> Graphql.Http.withHeader "authorization" ("Bearer " ++ secret)
