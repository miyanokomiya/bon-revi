module Pages.Projects.New exposing (Flags, Model, Msg, page)

import Html
import Page exposing (Document, Page)


type alias Flags =
    ()


type alias Model =
    {}


type Msg
    = NoOp


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
    ( {}, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Document Msg
view model =
    { title = "Projects.New"
    , body = [ Html.text "Projects.New" ]
    }