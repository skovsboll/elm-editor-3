module Main exposing (main)

import Browser
import Diff exposing (Change)
import Hql.Parser exposing (..)
import Html as H
import Html.Attributes as A
import Html.Events as E
import Json.Decode as J
import Layers.Adorns exposing (Adorn)
import Layers.DiffGutter
import Layers.Types exposing (Cursor, ScrollPos)
import Lsp.Ports
import Lsp.Up.DocumentChange as DocumentChange


type Msg
    = Input String
    | Move Int Int
    | Scroll ScrollPos
    | ToggleSuggestions
    | NextSuggestion
    | PrevSuggestion
    | InsertSuggestion
    | Noop


type alias Suggestion =
    { icon : String, code : String }


type alias Model =
    { text : String
    , origText : String
    , diff : List Change
    , ast : List Line
    , scroll : ScrollPos
    , cursor : Cursor
    , suggestions : Maybe ( List Suggestion, Suggestion, List Suggestion )
    , adorns : List Adorn
    , version : Int
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
      , suggestions = Nothing
      , adorns = Layers.Adorns.fakeAdorns defaultSrc
      , version = 1
      }
    , Cmd.none
    )


scrollTop : ScrollPos
scrollTop =
    { top = 0
    , left = 0
    }


updateText : String -> Model -> ( Model, Cmd Msg )
updateText text model =
    ( { model
        | text = text
        , ast = parseHql text
        , version = model.version + 1
        , adorns = Layers.Adorns.fakeAdorns text
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input text ->
            updateText text model

        Scroll pos ->
            ( { model | scroll = pos }, Cmd.none )

        Move _ end ->
            ( { model | cursor = calculateCursorCoordinates model.text end }, Cmd.none )

        ToggleSuggestions ->
            case model.suggestions of
                Nothing ->
                    ( { model
                        | suggestions =
                            Just
                                ( []
                                , { icon = "✸", code = "timechart()" }
                                , [ { icon = "◎", code = "parseCsv()" }
                                  , { icon = "✢", code = "mjallo()" }
                                  , { icon = "❍", code = "djallo()" }
                                  ]
                                )
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model
                        | suggestions = Nothing
                      }
                    , Cmd.none
                    )

        PrevSuggestion ->
            case model.suggestions of
                Just ( prev :: ptail, current, next ) ->
                    ( { model | suggestions = Just ( ptail, prev, current :: next ) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        NextSuggestion ->
            case model.suggestions of
                Just ( prev, current, next :: ntail ) ->
                    ( { model | suggestions = Just ( current :: prev, next, ntail ) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        InsertSuggestion ->
            case model.suggestions of
                Just ( _, current, _ ) ->
                    let
                        newText : String
                        newText =
                            String.slice 0 model.cursor.pos model.text
                                ++ current.code
                                ++ String.slice model.cursor.pos -1 model.text
                                |> Debug.log "newText"

                        ( newModel, cmd ) =
                            updateText newText model
                    in
                    ( { newModel | suggestions = Nothing }, cmd )

                _ ->
                    ( model, Cmd.none )

        Noop ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


keyDecoder : Model -> J.Decoder { message : Msg, stopPropagation : Bool, preventDefault : Bool }
keyDecoder model =
    let
        simpleMessage msg =
            { message = msg
            , stopPropagation = False
            , preventDefault = False
            }

        noBubble msg =
            { message = msg
            , stopPropagation = True
            , preventDefault = True
            }
    in
    J.map4
        (\key ctrl start end ->
            let
                _ =
                    Debug.log "key" key
            in
            case ( key, ctrl ) of
                ( " ", True ) ->
                    simpleMessage ToggleSuggestions

                ( "Enter", False ) ->
                    case model.suggestions of
                        Nothing ->
                            simpleMessage Noop

                        _ ->
                            noBubble InsertSuggestion

                ( "Escape", False ) ->
                    case model.suggestions of
                        Nothing ->
                            simpleMessage Noop

                        _ ->
                            simpleMessage ToggleSuggestions

                ( "ArrowUp", False ) ->
                    case model.suggestions of
                        Nothing ->
                            simpleMessage Noop

                        _ ->
                            noBubble PrevSuggestion

                ( "ArrowDown", False ) ->
                    case model.suggestions of
                        Nothing ->
                            simpleMessage Noop

                        _ ->
                            noBubble NextSuggestion

                _ ->
                    simpleMessage (Move start end)
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
        , Layers.DiffGutter.view model styles
        , Layers.Adorns.view model styles
        , viewSyntaxOverlay model
        , viewAutoCompleteOverlay model
        ]


viewTextArea : Model -> H.Html Msg
viewTextArea model =
    H.textarea
        [ styles
        , E.custom "keyup" (keyDecoder model)
        , E.custom "click" (keyDecoder model)
        , A.spellcheck False
        , A.class "absolute z-0 h-full p-2 pl-[48px] caret-red-500 text-transparent resize-none"
        , E.onInput Input
        , E.on "scroll" parseScrollEvent
        ]
        [ H.text model.text
        ]


viewSyntaxOverlay : Model -> H.Html msg
viewSyntaxOverlay model =
    H.node "syntax"
        [ styles
        , A.class "block absolute z-10 top-0 left-0 pointer-events-none will-change-transform h-auto transition-transform"
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
        Nothing ->
            H.node "no-suggestions" [] []

        Just ( pre, selected, post ) ->
            H.node "auto-complete"
                [ A.class "block absolute z-50 h-32 bg-slate-700 w-64 shadow rounded border border-slate-600 font-mono tracking-normal whitespace-pre leading-snug text-sm p-1"
                , A.style "left" (String.fromFloat (64 + model.cursor.x) ++ "px")
                , A.style "top" (String.fromFloat (model.cursor.y - model.scroll.top) ++ "px")
                ]
                [ H.ul [ A.class "text-white overflow-scroll h-full" ]
                    ((pre |> List.reverse |> List.map (\{ icon, code } -> H.li [] [ H.text icon, H.text " ", H.text code ]))
                        ++ [ H.li [ A.class "bg-gray-600" ] [ H.text selected.icon, H.text " ", H.text selected.code ]
                           ]
                        ++ (post |> List.map (\{ icon, code } -> H.li [] [ H.text icon, H.text " ", H.text code ]))
                    )
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
