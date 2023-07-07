module Main exposing (main)

import Browser
import Diff exposing (Change)
import Hql.Parser exposing (..)
import Html as H
import Html.Attributes as A
import Html.Events as E
import Json.Decode as J
import Lsp.Ports
import Lsp.Up.DocumentChange as DocumentChange exposing (DocumentChangeParams)


type Msg
    = Input String
    | Move Int Int
    | Scroll ScrollPos
    | ToggleSuggestions


type alias ScrollPos =
    { top : Float
    , left : Float
    }


type alias Model =
    { text : String
    , origText : String
    , diff : List Change
    , ast : List Line
    , scroll : ScrollPos
    , cursor : Cursor
    , suggestions : List ( String, String )
    , adorns : List Adorn
    , version : Int
    }


type alias Adorn =
    List AdornFragment


type AdornFragment
    = Normal String
    | Error String


type alias Cursor =
    { x : Float
    , y : Float
    , col : Int
    , row : Int
    , pos : Int
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
    let
        defaultSrc =
            """url=/^\\/add_to_cart\\/(?<product_id>\\d+)/
| match(file="products.csv", column=product_id, field=product_id)
| sum(product_price, as="Total revenue")
| #host=github #parser=json

/* Log everything.
   Search in real time.
*/
| repo.name=docker
| groupBy(repo.name, function=count())
| sort()

// Time is of essense
| timechart(field=#kind)
| sankey(from=lallerko, to=nollerkok)
"""
    in
    ( { text = defaultSrc
      , origText = defaultSrc
      , diff = []
      , scroll = scrollTop
      , ast = parseHql defaultSrc
      , cursor = { x = 0, y = 0, col = 0, row = 0, pos = 0 }
      , suggestions = []
      , adorns = fakeAdorns defaultSrc
      , version = 1
      }
    , Cmd.none
    )


fakeAdorns : String -> List Adorn
fakeAdorns src =
    src
        |> String.split "\n"
        |> List.map
            (\line ->
                line
                    |> String.split " "
                    |> List.concatMap
                        (\token ->
                            [ if token == "column=product_id," then
                                Error token

                              else
                                Normal token
                            , Normal " "
                            ]
                        )
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
            ( { model
                | text = text
                , ast = parseHql text
                , version = model.version + 1
                , adorns = fakeAdorns text
                , diff =
                    Diff.diffLines model.origText text
                        |> List.concatMap
                            (\change ->
                                case change of
                                    Diff.NoChange lines ->
                                        lines
                                            |> String.slice 0 -1
                                            |> String.split "\n"
                                            |> List.map Diff.NoChange

                                    Diff.Added lines ->
                                        lines
                                            |> String.slice 0 -1
                                            |> String.split "\n"
                                            |> List.map Diff.Added

                                    Diff.Changed _ lines ->
                                        lines
                                            |> String.slice 0 -1
                                            |> String.split "\n"
                                            |> List.map (Diff.Changed "")

                                    Diff.Removed lines ->
                                        lines
                                            |> String.slice 0 -1
                                            |> String.split "\n"
                                            |> List.map Diff.Removed
                            )
              }
            , Lsp.Ports.outgoingMessage
                (DocumentChange.encodeDocumentChangeMessage
                    { uri = "document-1"
                    , version = model.version + 1
                    , text = text
                    }
                )
            )

        Scroll pos ->
            ( { model | scroll = pos }, Cmd.none )

        Move _ end ->
            ( { model | cursor = calculateCursorCoordinates model.text end }, Cmd.none )

        ToggleSuggestions ->
            case model.suggestions of
                [] ->
                    ( { model
                        | suggestions =
                            [ ( "✸", "timechart()" )
                            , ( "◎", "parseCsv()" )
                            , ( "✢", "mjallo()" )
                            , ( "❍", "djallo()" )
                            ]
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model
                        | suggestions = []
                      }
                    , Cmd.none
                    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


keyDecoder : J.Decoder Msg
keyDecoder =
    J.map4
        (\key ctrl start end ->
            case ( key, ctrl ) of
                ( " ", True ) ->
                    ToggleSuggestions

                _ ->
                    Move start end
        )
        (J.field "key" J.string)
        (J.field "ctrlKey" J.bool)
        (J.at [ "target", "selectionStart" ] J.int)
        (J.at [ "target", "selectionEnd" ] J.int)


parseScrollEvent : J.Decoder Msg
parseScrollEvent =
    J.map2 ScrollPos
        (J.at [ "target", "scrollTop" ] J.float)
        (J.at [ "target", "scrollLeft" ] J.float)
        |> J.map Scroll



--
-- VIEW
--


view : Model -> H.Html Msg
view model =
    H.node "editor"
        [ A.class "h-full w-full block relative overflow-hidden p-0 m-0 align-left bg-stone-800" ]
        [ viewTextArea model
        , viewDiffGutter model
        , viewErrorOverlay model
        , viewSyntaxOverlay model
        , viewAutoCompleteOverlay model
        ]


viewTextArea : Model -> H.Html Msg
viewTextArea model =
    H.textarea
        [ styles
        , E.on "keyup" keyDecoder
        , E.on "click" keyDecoder
        , A.spellcheck False
        , A.class "h-full z-0 p-2 pl-[48px] caret-red-500 text-transparent resize-none"
        , E.onInput Input
        , E.on "scroll" parseScrollEvent
        ]
        [ H.text model.text
        ]


viewSyntaxOverlay : Model -> H.Html msg
viewSyntaxOverlay model =
    H.node "syntax"
        [ styles
        , A.class "block absolute z-10 top-0 left-0 z-1 pointer-events-none will-change-transform h-auto"
        , A.style "transform"
            ("translate("
                ++ String.fromFloat -model.scroll.left
                ++ "px, "
                ++ String.fromFloat -model.scroll.top
                ++ "px)"
            )
        ]
        [ H.pre [ A.class "p-2 m-0 align-left h-full bg-transparent border-none" ]
            [ H.code []
                (model.ast
                    |> List.indexedMap
                        (\idx fragments ->
                            H.node "code-line"
                                [ A.class "block pl-[0px]"
                                , A.class "before:content-[attr(data-lino)] before:inline-block before:text-right before:w-[40px] before:p-0 before:pr-[20px] before:opacity-25 before:text-white"
                                , A.attribute "data-lino" (String.fromInt idx)
                                ]
                                (fragments
                                    |> List.map (\{ text, tag, color } -> H.node tag [ A.class color ] [ H.text text ])
                                )
                        )
                )
            ]
        ]


viewAutoCompleteOverlay : Model -> H.Html Msg
viewAutoCompleteOverlay model =
    case model.suggestions of
        [] ->
            H.node "no-suggestions" [] []

        items ->
            H.node "auto-complete"
                [ A.class "absolute z-50 h-32 bg-slate-700 w-64 shadow rounded border border-slate-600 font-mono tracking-normal whitespace-pre leading-snug text-sm p-1"
                , A.style "left" (String.fromFloat (64 + model.cursor.x) ++ "px")
                , A.style "top" (String.fromFloat (model.cursor.y - model.scroll.top) ++ "px")
                ]
                [ H.ul [ A.class "text-white overflow-scroll h-full" ]
                    (items |> List.map (\( icon, code ) -> H.li [] [ H.text icon, H.text " ", H.text code ]))
                ]


viewErrorOverlay : Model -> H.Html msg
viewErrorOverlay model =
    H.node "errors"
        [ styles
        , A.class "block absolute top-0 left-0 z-20 pointer-events-none will-change-transform h-auto"
        , A.style "transform"
            ("translate("
                ++ String.fromFloat -model.scroll.left
                ++ "px, "
                ++ String.fromFloat -model.scroll.top
                ++ "px)"
            )
        ]
        [ H.pre [ A.class "p-2 m-0 align-left h-full bg-transparent border-none" ]
            [ H.code []
                (model.adorns
                    |> List.map
                        (\fragments ->
                            H.node
                                "adorn-line"
                                [ A.class "block pl-[40px] translate-y-[0.5rem]" ]
                                (fragments
                                    |> List.map
                                        (\fragment ->
                                            case fragment of
                                                Error txt ->
                                                    H.node "error" [ A.class "text-red-500 " ] [ String.repeat (String.length txt) "~" |> H.text ]

                                                Normal txt ->
                                                    H.node "no-error" [] [ String.repeat (String.length txt) " " |> H.text ]
                                        )
                                )
                        )
                )
            ]
        ]


viewDiffGutter : Model -> H.Html msg
viewDiffGutter model =
    H.node "diff"
        [ styles
        , A.class "block absolute top-0 left-8 z-50 pointer-events-none will-change-transform h-auto"
        , A.style "transform"
            ("translate("
                ++ String.fromFloat -model.scroll.left
                ++ "px, "
                ++ String.fromFloat -model.scroll.top
                ++ "px)"
            )
        ]
        [ H.pre [ A.class "p-2 m-0 align-left h-full bg-transparent border-none" ]
            [ H.code []
                (model.diff
                    |> List.concatMap
                        (\line ->
                            case line of
                                Diff.NoChange _ ->
                                    [ H.node "no-change" [ A.class "block w-1" ] [ H.text " " ]
                                    ]

                                Diff.Added _ ->
                                    [ H.node "line-added" [ A.class "block bg-green-600 w-1" ] [ H.text " " ] ]

                                Diff.Changed before after ->
                                    [ H.button
                                        [ A.id "a"
                                        , A.class "block bg-yellow-600 w-1"
                                        , A.attribute "popovertarget" "b"
                                        ]
                                        [ H.text " " ]
                                    , H.node "pop-over"
                                        [ A.id "b"
                                        , A.class "bottom-[calc(anchor(bottom))] left-[anchor(center)] translate-[-50% 0]"
                                        , A.attribute "popover" "auto"
                                        , A.attribute "anchor" "a"
                                        ]
                                        [ H.node "diff"
                                            [ A.class "flex flex-col" ]
                                            [ H.node "diff-before" [ A.class "text-red-500" ] [ H.text before ]
                                            , H.node "diff-after" [ A.class "text-green-500" ] [ H.text after ]
                                            , H.button [ A.class "button" ] [ H.text "Revert" ]
                                            ]
                                        ]
                                    ]

                                Diff.Removed _ ->
                                    [ H.node "line-removed" [ A.class "block bg-red-600 w-1" ] [ H.text " " ] ]
                        )
                )
            ]
        ]


fontHeight : Float
fontHeight =
    22.0


fontWidth : Float
fontWidth =
    9.6


calculateCursorCoordinates : String -> Int -> Cursor
calculateCursorCoordinates text position =
    let
        lines =
            text
                |> String.left position
                |> String.split "\n"

        row =
            lines |> List.length

        y =
            toFloat row * fontHeight - 10

        col =
            case List.reverse lines of
                hd :: _ ->
                    String.length hd + 1

                [] ->
                    0

        x =
            toFloat (col - 1) * fontWidth
    in
    { x = x, y = y, row = row, col = col, pos = position }



-- text-base is:
--
--font-size: 1rem; /* 16px */
--line-height: 1.5rem; /* 24px */


styles : H.Attribute msg
styles =
    A.class """
    border-box 
    text-base font-mono tracking-normal whitespace-pre leading-snug 
    w-full p-0 m-0 b-0 
    bg-transparent 
    """
