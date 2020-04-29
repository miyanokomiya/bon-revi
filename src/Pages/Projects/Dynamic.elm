module Pages.Projects.Dynamic exposing (Flags, Model, Msg, page)

import Api.Object
import Api.Object.Project
import Api.Object.ProjectPage
import Api.Query
import Api.Scalar
import Api.ScalarCodecs
import Generated.Route as Route
import Global
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html)
import Html.Attributes exposing (class, href)
import Page exposing (Document, Page)
import RemoteData exposing (RemoteData)
import Request


type alias Flags =
    { param1 : String }


type alias Model =
    { data : RemoteData (Graphql.Http.Error Response) Response
    , id : Api.ScalarCodecs.Id
    }


type Msg
    = NoOp
    | GotResponse (RemoteData (Graphql.Http.Error Response) Response)


page : Page Flags Model Msg
page =
    Page.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        id =
            Api.Scalar.Id flags.param1
    in
    ( { data = RemoteData.Loading
      , id = id
      }
    , execQuery id
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotResponse response ->
            ( { model | data = response }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Document Msg
view model =
    { title = "Projects.Dynamic"
    , body =
        let
            content =
                case model.data of
                    RemoteData.NotAsked ->
                        Html.div [] [ Html.text "Not Asked" ]

                    RemoteData.Loading ->
                        Html.div [] [ Html.text "Loading" ]

                    RemoteData.Failure e ->
                        Html.div [] [ Html.text "Failure" ]

                    RemoteData.Success response ->
                        case response.maybeProject of
                            Just project ->
                                viewProject project

                            _ ->
                                Html.div [] [ Html.text "Not Found" ]
        in
        [ content
        ]
    }


viewProject : Project -> Html msg
viewProject project =
    Html.div []
        [ Html.div [ class "mb-2 pb-1 flex border-b-2 text-xl" ]
            [ Html.p [ class "w-32" ] [ Html.text "Project:" ]
            , Html.h2 [] [ Html.text project.name ]
            ]
        , Html.div [ class "flex" ]
            [ Html.p [ class "w-32" ] [ Html.text "Description:" ]
            , Html.pre [] [ Html.text project.description ]
            ]
        ]


type alias Project =
    { id_ : Api.ScalarCodecs.Id
    , name : String
    , description : String
    }


type alias Response =
    { maybeProject : Maybe Project
    }


query : Api.ScalarCodecs.Id -> SelectionSet Response RootQuery
query id =
    SelectionSet.succeed Response
        |> SelectionSet.with
            (Api.Query.findProjectByID { id = id } projectSelection)


projectSelection : SelectionSet Project Api.Object.Project
projectSelection =
    SelectionSet.succeed Project
        |> SelectionSet.with Api.Object.Project.id_
        |> SelectionSet.with Api.Object.Project.name
        |> SelectionSet.with Api.Object.Project.description


execQuery : Api.ScalarCodecs.Id -> Cmd Msg
execQuery id =
    query id
        |> Request.queryRequest
        |> Request.withHeader
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)
