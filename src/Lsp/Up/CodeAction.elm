module Lsp.Up.CodeAction exposing (CompletionParams, encoder)

import Json.Encode as E
import Lsp.Diagnostics exposing (Diagnostic, Range, textDocumentEncoder)


type alias CompletionParams =
    { uri : String
    , id : Int
    , range : Range
    , context : Diagnostic
    }


encoder : CompletionParams -> E.Value
encoder params =
    let
        paramsObject : E.Value
        paramsObject =
            E.object
                [ ( "textDocument", textDocumentEncoder params.uri )
                ]
    in
    E.object
        [ ( "jsonrpc", E.string "2.0" )
        , ( "method", E.string "textDocument/codeAction" )
        , ( "id", E.int params.id )
        , ( "params", paramsObject )
        ]
