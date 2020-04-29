module Pages.Projects.New exposing (Flags, Model, Msg, page)

import Api.InputObject
import Api.Mutation
import Api.Object
import Api.Object.Project
import Graphql.Http
import Graphql.Operation exposing (RootMutation)
import Graphql.OptionalArgument
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onInput, onSubmit)
import Page exposing (Document, Page)
import RemoteData exposing (RemoteData)
import Request


type alias Flags =
    ()


type alias Model =
    { projectInput : ProjectInput
    , response : RemoteData (Graphql.Http.Error Response) Response
    }


type Msg
    = NoOp
    | InputName String
    | InputDescription String
    | Submit
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
    ( { projectInput =
            { name = ""
            , description = ""
            }
      , response = RemoteData.Loading
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        projectInput =
            model.projectInput
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InputName val ->
            ( { model | projectInput = { projectInput | name = val } }, Cmd.none )

        InputDescription val ->
            ( { model | projectInput = { projectInput | description = val } }, Cmd.none )

        Submit ->
            ( model, makeRequest projectInput )

        GotResponse response ->
            ( { model | projectInput = { projectInput | name = "", description = "" }, response = response }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Document Msg
view model =
    { title = "New Project"
    , body =
        [ Html.div []
            [ Html.form [ onSubmit Submit ]
                [ Html.h2 [ class "mb-4 text-xl" ] [ Html.text "New Project" ]
                , Html.div [ class "mb-2" ]
                    [ viewLabel "Name"
                    , Html.input [ value model.projectInput.name, onInput InputName, class "w-full py-2 px-4 border rounded" ] []
                    ]
                , Html.div [ class "mb-2" ]
                    [ viewLabel "Description"
                    , Html.textarea [ value model.projectInput.description, onInput InputDescription, class "w-full py-2 px-4 border rounded" ] []
                    ]
                , Html.div [ class "text-right" ]
                    [ Html.button [ class "border py-2 px-4 bg-blue-500 text-white" ] [ Html.text "Create" ]
                    ]
                ]
            ]
        ]
    }


viewLabel : String -> Html msg
viewLabel text =
    Html.label [ class "mb-1 w-full block" ] [ Html.text text ]


type alias Response =
    { project : Project }


type alias Project =
    { name : String
    , description : String
    }


type alias ProjectInput =
    { name : String
    , description : String
    }


mutation : ProjectInput -> SelectionSet Response RootMutation
mutation input =
    SelectionSet.succeed Response
        |> SelectionSet.with
            (Api.Mutation.createProject
                { data =
                    Api.InputObject.ProjectInput
                        { name = input.name
                        , description = input.description
                        , files = Graphql.OptionalArgument.Absent
                        }
                }
                projectSelection
            )


projectSelection : SelectionSet Project Api.Object.Project
projectSelection =
    SelectionSet.succeed Project
        |> SelectionSet.with Api.Object.Project.name
        |> SelectionSet.with Api.Object.Project.description


makeRequest : ProjectInput -> Cmd Msg
makeRequest input =
    mutation input
        |> Request.mutationRequest
        |> Request.withHeader
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)
