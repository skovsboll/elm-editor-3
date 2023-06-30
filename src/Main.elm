module Main exposing (main)

import Browser
import Html as H
import Html.Attributes as A
import Html.Events as E
import Json.Decode as J
import Parser as P


type Msg
    = Input String
    | Scroll ScrollPos


type alias ScrollPos =
    { top : Float
    , left : Float
    }


type alias Model =
    { text : String
    , scroll : ScrollPos
    }


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
    ( { text =
            """url=/^\\/add_to_cart\\/(?<product_id>\\d+)/
| match(file="products.csv", column=product_id, field=product_id)
| sum(product_price, as="Total revenue")
| #host=github #parser=json

// This I dunno about
| repo.name=docker/*
| groupBy(repo.name, function=count())
| sort()

// Maybe add this
| timechart(field=#kind)
| sankey(from=lallerko, to=nollerkok)"""
      , scroll = scrollTop
      }
    , Cmd.none
    )


scrollTop : ScrollPos
scrollTop =
    { top = 0
    , left = 0
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input text ->
            ( { model | text = text }, Cmd.none )

        Scroll pos ->
            ( { model | scroll = pos }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


format : Model -> List String
format model =
    model.text
        |> String.split "\n"


view : Model -> H.Html Msg
view model =
    H.node "editor"
        [ A.class "h-full w-full block relative overflow-hidden p-0 m-0 align-left bg-stone-800" ]
        [ H.node "overlay"
            [ styles
            , A.class "block absolute top-0 left-0 z-1 pointer-events-none will-change-scroll h-auto"
            , A.style "transform"
                ("translate("
                    ++ String.fromFloat -model.scroll.left
                    ++ "px, "
                    ++ String.fromFloat -model.scroll.top
                    ++ "px)"
                )
            ]
            [ H.pre [ A.class "p-2 m-0 align-left h-full bg-transparent" ]
                [ H.code []
                    (format model
                        |> List.indexedMap
                            (\idx line ->
                                H.node "code-line"
                                    [ A.class "block pl-[0px] text-orange-600"
                                    , A.class "before:content-[attr(data-lino)] before:inline-block before:align-right before:w-[40px] before:p-0 before:pr-[20px] before:opacity-25"
                                    , A.attribute "data-lino" (String.fromInt idx)
                                    ]
                                    [ H.text (line ++ "\n") ]
                            )
                    )
                ]
            ]
        , H.textarea
            [ styles
            , A.spellcheck False
            , A.class "h-full p-2 pl-[48px] caret-red-500 text-transparent resize-none z-2 relative"
            , E.onInput Input
            , E.on "scroll" parseScrollEvent
            ]
            [ H.text model.text
            ]
        ]


parseScrollEvent : J.Decoder Msg
parseScrollEvent =
    J.map2 ScrollPos
        (J.at [ "target", "scrollTop" ] J.float)
        (J.at [ "target", "scrollLeft" ] J.float)
        |> J.map Scroll


styles : H.Attribute msg
styles =
    A.class """
    border-box 
    text-base font-mono tracking-normal whitespace-pre leading-snug 
    w-full p-0 m-0 b-0 
    bg-transparent 
    """
