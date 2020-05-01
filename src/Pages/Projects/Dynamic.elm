module Pages.Projects.Dynamic exposing (Flags, Model, Msg, page)

import Api.Object
import Api.Object.Project
import Api.Object.ProjectPage
import Api.Object.ProjectSecret
import Api.Query
import Api.Scalar
import Api.ScalarCodecs
import Components
import Generated.Route as Route
import Global
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html)
import Html.Attributes exposing (class, href, value)
import Html.Events exposing (onClick, onInput)
import Page exposing (Document, Page)
import RemoteData exposing (RemoteData)
import Request


type alias Flags =
    { param1 : String }


type alias Model =
    { data : RemoteData (Graphql.Http.Error Response) Response
    , deleteResponse : RemoteData (Graphql.Http.Error DeleteResponse) DeleteResponse
    , id : Api.ScalarCodecs.Id
    , projectSecret : String
    }


type Msg
    = NoOp
    | GotResponse (RemoteData (Graphql.Http.Error Response) Response)
    | InputSecretCode String
    | Delete
    | GotDeleteResponse (RemoteData (Graphql.Http.Error DeleteResponse) DeleteResponse)


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
      , deleteResponse = RemoteData.Loading
      , id = id
      , projectSecret = ""
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

        InputSecretCode val ->
            ( { model | projectSecret = val }, Cmd.none )

        Delete ->
            ( model, execDeleteQuery model.projectSecret model.id )

        GotDeleteResponse response ->
            ( { model | data = RemoteData.NotAsked, deleteResponse = response }, Cmd.none )


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
                        Html.div [] [ Html.text "No Data" ]

                    RemoteData.Loading ->
                        Components.loading

                    RemoteData.Failure e ->
                        Html.div [] [ Html.text "Failure" ]

                    RemoteData.Success response ->
                        case response.maybeProject of
                            Just project ->
                                Html.div []
                                    [ viewProject project
                                    , Html.div [ class "mt-6 flex items-center" ]
                                        [ Html.label [ class "mr-2 block" ] [ Html.text "Code:" ]
                                        , Html.input [ value model.projectSecret, onInput InputSecretCode, class "mr-2 w-32 py-2 px-4 border rounded" ] []
                                        , Html.button [ class "border py-2 px-4 bg-red-500 text-white", onClick Delete ] [ Html.text "Delete" ]
                                        ]
                                    ]

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
        , Html.div [ class "flex" ]
            [ Html.p [ class "w-32" ] [ Html.text "Secret:" ]
            , Html.pre []
                [ Html.text
                    (case project.secret of
                        Just secret ->
                            secret.code

                        _ ->
                            "****"
                    )
                ]
            ]
        ]


type alias Project =
    { id_ : Api.ScalarCodecs.Id
    , name : String
    , description : String
    , secret : Maybe ProjectSecret
    }


type alias ProjectSecret =
    { code : String
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
        |> SelectionSet.with (Api.Object.Project.secret projectSecretSelection)


projectSecretSelection : SelectionSet ProjectSecret Api.Object.ProjectSecret
projectSecretSelection =
    SelectionSet.succeed ProjectSecret
        |> SelectionSet.with Api.Object.ProjectSecret.code


execQuery : Api.ScalarCodecs.Id -> Cmd Msg
execQuery id =
    query id
        |> Request.queryRequest
        |> Request.withHeader
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)


type alias DeleteResponse =
    { project : Project
    }


deleteQuery : String -> Api.ScalarCodecs.Id -> SelectionSet DeleteResponse RootQuery
deleteQuery code id =
    SelectionSet.succeed DeleteResponse
        |> SelectionSet.with
            (Api.Query.deleteProjectFn { id = Request.idToString id, code = code } projectSelection)


execDeleteQuery : String -> Api.ScalarCodecs.Id -> Cmd Msg
execDeleteQuery code id =
    deleteQuery code id
        |> Request.queryRequest
        |> Request.withHeader
        |> Graphql.Http.send (RemoteData.fromResult >> GotDeleteResponse)
