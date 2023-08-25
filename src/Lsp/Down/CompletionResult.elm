module Lsp.Down.CompletionResult exposing (CompletionItem, CompletionList, CompletionResult, decoder)

import Json.Decode as Decode exposing (Decoder, bool, field, int, list, string)


type alias CompletionList =
    { isIncomplete : Bool
    , items : List CompletionItem
    }


type CompletionKind
    = Text
    | Method
    | Function


type alias CompletionDocumentation =
    { kind : String
    , value : String
    }


type InsertTextFormat
    = Plain
    | Template


type alias CompletionItem =
    { label : String
    , kind : CompletionKind
    , detail : String
    , documentation : CompletionDocumentation
    , insertText : String
    , insertTextFormat : InsertTextFormat
    , sortText : String
    }


type alias CompletionResult =
    { jsonRpcVersion : String
    , id : String
    , result : CompletionList
    }


decoder : Decoder CompletionResult
decoder =
    Decode.map3 CompletionResult
        (field "jsonrpc" string)
        (field "id" string)
        (field "result" completionListDecoder)


completionKindDecoder : Decoder CompletionKind
completionKindDecoder =
    Decode.int
        |> Decode.andThen
            (\i ->
                case i of
                    1 ->
                        Decode.succeed Text

                    2 ->
                        Decode.succeed Method

                    3 ->
                        Decode.succeed Function

                    _ ->
                        Decode.fail "Unsupported completion item kind"
            )


completionListDecoder : Decoder CompletionList
completionListDecoder =
    Decode.map2 CompletionList
        (field "isIncomplete" bool)
        (field "items" (list completionItemDecoder))


documentationDecoder : Decoder CompletionDocumentation
documentationDecoder =
    Decode.map2 CompletionDocumentation
        (field "kind" string)
        (field "value" string)


insertTextFormatDecoder : Decoder InsertTextFormat
insertTextFormatDecoder =
    Decode.int
        |> Decode.andThen
            (\i ->
                case i of
                    1 ->
                        Decode.succeed Plain

                    2 ->
                        Decode.succeed Template

                    _ ->
                        Decode.fail "Unknown insertTextFormat"
            )


completionItemDecoder : Decoder CompletionItem
completionItemDecoder =
    Decode.map7 CompletionItem
        (field "label" string)
        (field "kind" completionKindDecoder)
        (field "detail" string)
        (field "documentation" documentationDecoder)
        (field "insertText" string)
        (field "insertTextFormat" insertTextFormatDecoder)
        (field "sortText" string)
