module Main exposing (main)

import Browser
import Html as H
import Html.Attributes as A
import Html.Events as E


type Msg
    = Input String


type alias Model =
    { text : String }


main : Program {} Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : {} -> ( Model, Cmd Msg )
init _ =
    ( {}, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    H.node "editor"
        [ H.class "h-full w-full block relative overflow-hidden p-0 m-0 align-left bg-stone-800" ]
        [ H.node "overlay"
            [ styles
            , H.class "block absolute top-0 left-0 z-1 pointer-events-none"
            ]
            [ pre [ H.class "p-2 m-0 align-left" ]
                [ code []
                    [ H.node "code-line" [ H.class "block pl-16", attribute "data-lino" "1" ] [ text "lallerkok" ]
                    , H.node "code-line" [ H.class "block pl-16", attribute "data-lino" "2" ] [ text "snallerkok" ]
                    ]
                ]
            ]
        , H.textarea
            [ styles
            , A.spellcheck False
            , H.class "pl-16 caret-red-500 text-black resize-none z-2 relative p-2"
            , E.onInput Input
            ]
            [ H.text model.text
            ]
        ]


styles : H.Attribute msg
styles =
    A.class """
    border-box text-base 
    font-mono tracking-normal whitespace-pre leading-snug 
    w-full h-full 
    p-0 m-0 b-0 bg-transparent text-white
    """
