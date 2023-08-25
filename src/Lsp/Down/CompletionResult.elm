module Lsp.Down.CompletionResult exposing (CompletionItem, CompletionList, CompletionResult, decoder)

import Json.Decode as Decode exposing (Decoder, bool, field, int, list, string)


type alias CompletionList =
    { isIncomplete : Bool
    , items : List CompletionItem
    }


type alias CompletionItem =
    { label : String
    , kind : Maybe Int -- The LSP specifies this as an integer, with each number mapping to a specific kind (e.g., 1 for Text, 2 for Method, 3 for Function, etc.)

    -- ... you might want to add more fields here depending on the details you want
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


completionListDecoder : Decoder CompletionList
completionListDecoder =
    Decode.map2 CompletionList
        (field "isIncomplete" bool)
        (field "items" (list completionItemDecoder))


completionItemDecoder : Decoder CompletionItem
completionItemDecoder =
    Decode.map2 CompletionItem
        (field "label" string)
        (field "kind" (Decode.maybe int))
