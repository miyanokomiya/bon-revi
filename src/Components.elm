module Components exposing (layout)

import Browser exposing (Document)
import Generated.Route as Route exposing (Route)
import Html exposing (..)
import Html.Attributes as Attr exposing (class, href, style)


layout : { page : Document msg } -> Document msg
layout { page } =
    { title = page.title
    , body =
        [ div []
            [ navbar
            , div [ class "container mx-auto", style "flex" "1 0 auto" ] page.body
            , footer
            ]
        ]
    }


navbar : Html msg
navbar =
    header [ class "mb-4 py-2 border" ]
        [ div [ class "container mx-auto flex justify-between" ]
            [ a [ class "", href (Route.toHref Route.Top) ] [ text "home" ]
            , div [ class "flex" ]
                [ a [ class "link", href (Route.toHref Route.Projects_Index) ] [ text "projects" ]
                ]
            ]
        ]


footer : Html msg
footer =
    Html.footer [ class "mt-8 py-2 border-t" ]
        [ div
            [ class "container mx-auto" ]
            [ text "built with elm ❤" ]
        ]
