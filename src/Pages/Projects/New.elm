module Pages.Projects.New exposing (Flags, Model, Msg, page)

import Api.InputObject
import Api.Mutation
import Api.Object
import Api.Object.File
import Api.Object.FilePage
import Api.Object.Project
import Graphql.Http
import Graphql.Operation exposing (RootMutation)
import Graphql.OptionalArgument
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import List.Extra
import Page exposing (Document, Page)
import RemoteData exposing (RemoteData)
import Request


type alias Flags =
    ()


type alias Model =
    { projectInput : ProjectInput
    , fileInputList : List FileInput
    , response : RemoteData (Graphql.Http.Error Response) Response
    }


type Msg
    = InputName String
    | InputDescription String
    | InputSecretCode String
    | InputFilePath Int String
    | InputFileDescription Int String
    | InputFileContent Int String
    | AddFile
    | DeleteFile Int
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
init _ =
    ( { projectInput = initProjectInput
      , fileInputList = [ initFileInput ]
      , response = RemoteData.Loading
      }
    , Cmd.none
    )


initProjectInput : ProjectInput
initProjectInput =
    { name = "MyProject"
    , description = ""
    , code = ""
    }


initFileInput : FileInput
initFileInput =
    { path = ""
    , description = ""
    , content = ""
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        projectInput =
            model.projectInput
    in
    case msg of
        InputName val ->
            ( { model | projectInput = { projectInput | name = val } }, Cmd.none )

        InputDescription val ->
            ( { model | projectInput = { projectInput | description = val } }, Cmd.none )

        InputSecretCode val ->
            ( { model | projectInput = { projectInput | code = val } }, Cmd.none )

        InputFilePath index val ->
            ( { model | fileInputList = model.fileInputList |> patchFileInput index (\old -> { old | path = val }) }
            , Cmd.none
            )

        InputFileDescription index val ->
            ( { model | fileInputList = model.fileInputList |> patchFileInput index (\old -> { old | description = val }) }
            , Cmd.none
            )

        InputFileContent index val ->
            ( { model | fileInputList = model.fileInputList |> patchFileInput index (\old -> { old | content = val }) }
            , Cmd.none
            )

        AddFile ->
            ( { model | fileInputList = model.fileInputList ++ [ initFileInput ] }, Cmd.none )

        DeleteFile index ->
            ( { model | fileInputList = List.Extra.removeAt index model.fileInputList }, Cmd.none )

        Submit ->
            ( model, makeRequest model.fileInputList projectInput )

        GotResponse response ->
            ( { model | projectInput = initProjectInput, fileInputList = [ initFileInput ], response = response }, Cmd.none )


patchFileInput : Int -> (FileInput -> FileInput) -> List FileInput -> List FileInput
patchFileInput index patch fileInputList =
    fileInputList
        |> List.indexedMap
            (\i input ->
                if i == index then
                    patch input

                else
                    input
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Document Msg
view model =
    { title = "New Project"
    , body =
        [ Html.div []
            [ Html.form []
                [ Html.h2 [ class "mb-4 text-xl" ] [ Html.text "New Project" ]
                , Html.div [ class "mb-2" ]
                    [ viewLabel "Name"
                    , Html.input [ value model.projectInput.name, onInput InputName, class "w-full py-2 px-4 border rounded" ] []
                    ]
                , Html.div [ class "mb-2" ]
                    [ viewLabel "Description"
                    , Html.textarea [ value model.projectInput.description, onInput InputDescription, class "w-full py-2 px-4 border rounded" ] []
                    ]
                , Html.div [ class "mb-4" ]
                    [ viewLabel "Secret Code"
                    , Html.input [ value model.projectInput.code, onInput InputSecretCode, class "w-full py-2 px-4 border rounded" ] []
                    ]
                , Html.div [ class "mb-2" ]
                    [ viewLabel "Files"
                    , Html.div [] (List.indexedMap (\i fileInput -> viewFileInput model.projectInput.name i fileInput) model.fileInputList)
                    ]
                , Html.div [ class "mb-4 text-right" ]
                    [ Html.button [ class "border py-1 px-2 bg-green-500 text-white text-sm", type_ "button", onClick AddFile ] [ Html.text "Add File" ]
                    ]
                , Html.div [ class "text-right" ]
                    [ Html.button [ class "border py-2 px-4 bg-blue-500 text-white", type_ "button", onClick Submit ] [ Html.text "Create Project" ]
                    ]
                ]
            ]
        ]
    }


viewFileInput : String -> Int -> FileInput -> Html Msg
viewFileInput rootName index fileInput =
    Html.div [ class "mx-2 py-2 border-t" ]
        [ Html.div [ class "mb-2" ]
            [ Html.div [ class "mb-1 flex items-center" ]
                [ Html.label [] [ Html.text "Path" ]
                , Html.button [ class "ml-auto border py-1 px-2 bg-red-500 text-white text-sm", type_ "button", onClick (DeleteFile index) ] [ Html.text "Delete" ]
                ]
            , Html.div [ class "flex items-center" ]
                [ Html.span [ class "" ] [ Html.text (rootName ++ " / ") ]
                , Html.input [ value fileInput.path, onInput (InputFilePath index), class "flex-grow py-2 px-4 border rounded" ] []
                ]
            ]
        , Html.div [ class "mb-2" ]
            [ viewLabel "Description"
            , Html.textarea [ value fileInput.description, onInput (InputFileDescription index), class "w-full py-2 px-4 border rounded" ] []
            ]
        , Html.div [ class "mb-2" ]
            [ viewLabel "Content"
            , Html.textarea [ value fileInput.content, onInput (InputFileContent index), class "w-full py-2 px-4 border rounded" ] []
            ]
        ]


viewLabel : String -> Html msg
viewLabel text =
    Html.label [ class "mb-1 w-full block" ] [ Html.text text ]


type alias Response =
    { project : Project }


type alias Project =
    { name : String
    , description : String
    , files : FilePage
    }


type alias File =
    { path : String
    , description : String
    , content : String
    }


type alias FilePage =
    { data : List File
    }


type alias ProjectInput =
    { name : String
    , description : String
    , code : String
    }


type alias FileInput =
    { path : String
    , description : String
    , content : String
    }


mutation : List FileInput -> ProjectInput -> SelectionSet Response RootMutation
mutation fileInputList input =
    SelectionSet.succeed Response
        |> SelectionSet.with
            (Api.Mutation.createProject
                { data =
                    Api.InputObject.ProjectInput
                        { name = input.name
                        , description = input.description
                        , files =
                            Graphql.OptionalArgument.Present
                                (Api.InputObject.ProjectFilesRelation
                                    { create =
                                        Graphql.OptionalArgument.Present
                                            (List.map
                                                (\f ->
                                                    Just
                                                        (Api.InputObject.FileInput
                                                            { path = f.path
                                                            , description = f.description
                                                            , content = f.content
                                                            , project = Graphql.OptionalArgument.Absent
                                                            }
                                                        )
                                                )
                                                fileInputList
                                            )
                                    , connect = Graphql.OptionalArgument.Absent
                                    , disconnect = Graphql.OptionalArgument.Absent
                                    }
                                )
                        , secret =
                            Graphql.OptionalArgument.Present
                                { create = Graphql.OptionalArgument.Present { code = input.code }
                                , connect = Graphql.OptionalArgument.Absent
                                , disconnect = Graphql.OptionalArgument.Absent
                                }
                        }
                }
                projectSelection
            )


projectSelection : SelectionSet Project Api.Object.Project
projectSelection =
    SelectionSet.succeed Project
        |> SelectionSet.with Api.Object.Project.name
        |> SelectionSet.with Api.Object.Project.description
        |> SelectionSet.with (Api.Object.Project.files identity filePageSelection)


fileSelection : SelectionSet File Api.Object.File
fileSelection =
    SelectionSet.succeed File
        |> SelectionSet.with Api.Object.File.path
        |> SelectionSet.with Api.Object.File.description
        |> SelectionSet.with Api.Object.File.content


filePageSelection : SelectionSet FilePage Api.Object.FilePage
filePageSelection =
    SelectionSet.succeed FilePage
        |> SelectionSet.with
            (Api.Object.FilePage.data fileSelection
                |> SelectionSet.nonNullElementsOrFail
            )


makeRequest : List FileInput -> ProjectInput -> Cmd Msg
makeRequest fileInputList input =
    mutation fileInputList input
        |> Request.mutationRequest
        |> Request.withHeader
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)
