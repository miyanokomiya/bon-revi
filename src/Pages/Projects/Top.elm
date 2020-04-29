module Pages.Projects.Top exposing (Flags, Model, Msg, page)

import Api.Object
import Api.Object.Project
import Api.Object.ProjectPage
import Api.Query
import Api.ScalarCodecs
import Api.Scalar
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
    ()


type alias Model =
    { data : RemoteData (Graphql.Http.Error Response) Response
    }


type alias Response =
    { projectPage : ProjectPage
    }


type alias Project =
    { id_ : Api.ScalarCodecs.Id
    , name : String
    , description : String
    }


type alias ProjectPage =
    { data : List Project
    }


query : SelectionSet Response RootQuery
query =
    SelectionSet.succeed Response
        |> SelectionSet.with
            (Api.Query.allProjects identity projectPageSelection)


projectSelection : SelectionSet Project Api.Object.Project
projectSelection =
    SelectionSet.succeed Project
        |> SelectionSet.with Api.Object.Project.id_
        |> SelectionSet.with Api.Object.Project.name
        |> SelectionSet.with Api.Object.Project.description


projectPageSelection : SelectionSet ProjectPage Api.Object.ProjectPage
projectPageSelection =
    SelectionSet.succeed ProjectPage
        |> SelectionSet.with
            (Api.Object.ProjectPage.data projectSelection
                |> SelectionSet.nonNullElementsOrFail
            )


execQuery : Cmd Msg
execQuery =
    query
        |> Request.queryRequest
        |> Request.withHeader
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)


type Msg
    = NoOp
    | GotResponse (RemoteData (Graphql.Http.Error Response) Response)


page : Page Flags Model Msg
page =
    Page.component
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    ( { data = RemoteData.Loading }, execQuery, Cmd.none )


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update global msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, Cmd.none )

        GotResponse response ->
            ( { model | data = response }, Cmd.none, Cmd.none )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions global model =
    Sub.none


view : Global.Model -> Model -> Document Msg
view global model =
    { title = "Project List"
    , body =
        let
            message =
                case model.data of
                    RemoteData.NotAsked ->
                        Html.div [] [ Html.text "Not Asked" ]

                    RemoteData.Loading ->
                        Html.div [] [ Html.text "Loading" ]

                    RemoteData.Failure e ->
                        Html.div [] [ Html.text "Failure" ]

                    RemoteData.Success response ->
                        viewResponse response
        in
        [ Html.a [ class "border py-2 px-4 bg-blue-500 text-white", href (Route.toHref Route.Projects_New) ] [ Html.text "New" ]
        , Html.div [ class "mt-4 mb-2" ] [ Html.text "Project List" ]
        , message
        ]
    }


viewResponse : Response -> Html msg
viewResponse response =
    Html.div [] (List.map viewProjectRow response.projectPage.data)


viewProjectRow : Project -> Html msg
viewProjectRow project =
  let
      id = case project.id_ of
        Api.Scalar.Id i -> i
  in
    Html.a [ href (Route.toHref (Route.Projects_Dynamic { param1 = id })), class "block py-2 px-4 border rounded mt-2 first:mt-0" ]
        [ Html.div [ class "text-lg font-bold" ] [ Html.text project.name ]
        , Html.div [ class "mt-1 border-t" ] [ Html.text project.description ]
        ]
