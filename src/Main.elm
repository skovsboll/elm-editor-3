module Main exposing (main)

import Browser
import Cursor
import Diff exposing (Change)
import Hql.Parser exposing (..)
import Html as H
import Html.Attributes as A
import Json.Decode as J
import Json.Encode as E
import Layers.Adorns exposing (Adorn)
import Layers.AutoComplete exposing (Suggestion)
import Layers.DiffGutter
import Layers.Syntax
import Layers.TextArea
import Layers.Types exposing (Cursor, ScrollPos)
import Lsp.Down exposing (MessageType(..))
import Lsp.Down.Completion as Completion
import Lsp.Ports
import Lsp.Up.DidChange
import Lsp.Up.Initialize as Initialize


type Msg
    = Noop
    | Input String
    | Move Int Int
    | Scroll ScrollPos
    | ToggleSuggestions
    | NextSuggestion
    | PrevSuggestion
    | InsertSuggestion
    | WebSocketConnectionReady
    | LspMessageReceived String


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
    , sendLsp
        (Lsp.Up.DidChange.encode
            { uri = "document-1"
            , version = model.version + 1
            , text = text
            }
        )
    )


sendLsp : E.Value -> Cmd Msg
sendLsp json =
    json
        |> E.encode 0
        |> Lsp.Ports.outgoingMessage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input text ->
            updateText text model

        Scroll pos ->
            ( { model | scroll = pos }, Cmd.none )

        Move _ end ->
            ( { model | cursor = Cursor.fromTextPosition model.text end }, Cmd.none )

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

        WebSocketConnectionReady ->
            ( model
            , sendLsp
                (Initialize.encode
                    { processId = Nothing
                    , rootUri = Nothing
                    , id = 0
                    , capabilities =
                        { textDocument =
                            { completion =
                                { completionItem = { snippetSupport = False } }
                            }
                        , workspace =
                            { workspaceFolders = False }
                        }
                    , trace = ""
                    }
                )
            )

        LspMessageReceived message ->
            parseMessage model message

        Noop ->
            ( model, Cmd.none )


parseMessage : Model -> String -> ( Model, Cmd Msg )
parseMessage model message =
    J.decodeString
        Lsp.Down.messageDecoder
        message
        |> Result.map (updateLspMessage model)
        |> Result.withDefault ( model, Cmd.none )


updateLspMessage : Model -> MessageType -> ( Model, Cmd Msg )
updateLspMessage model msg =
    case msg of
        Completion result ->
            let
                suggestions : List Suggestion
                suggestions =
                    result.result.items |> List.map itemToSuggestion

                modelWithSuggestions : Model
                modelWithSuggestions =
                    { model
                        | suggestions =
                            case suggestions of
                                h :: t ->
                                    Just ( [], h, t )

                                [] ->
                                    Nothing
                    }
            in
            ( modelWithSuggestions, Cmd.none )

        Hover result ->
            ( model, Cmd.none )

        Initialize result ->
            ( model, Cmd.none )


itemToSuggestion : Completion.CompletionItem -> Suggestion
itemToSuggestion item =
    { icon = "›", code = item.label }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Lsp.Ports.webSocketReady (always WebSocketConnectionReady)
        , Lsp.Ports.incomingMessage LspMessageReceived
        ]



--
-- VIEW
--


view : Model -> H.Html Msg
view model =
    H.node "editor"
        [ A.class "h-full w-full block relative overflow-hidden p-0 m-0 align-left bg-stone-800" ]
        [ Layers.TextArea.view model styles (keyDecoder model) parseScrollEvent Input
        , Layers.DiffGutter.view model styles
        , Layers.Adorns.view model styles
        , Layers.Syntax.view model styles
        , Layers.AutoComplete.view model
        ]


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
