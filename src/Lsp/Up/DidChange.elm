module Lsp.Up.DidChange exposing (DocumentChangeParams, encoder)

import Json.Encode as E


type alias DocumentChangeParams =
    { uri : String
    , version : Int
    , text : String
    }


encoder : DocumentChangeParams -> E.Value
encoder params =
    E.object
        [ ( "jsonrpc", E.string "2.0" )
        , ( "method", E.string "textDocument/didChange" )
        , ( "params", paramsObject params )
        ]


uriObject : DocumentChangeParams -> E.Value
uriObject params =
    E.object [ ( "uri", E.string params.uri ), ( "version", E.int params.version ) ]


contentChange : DocumentChangeParams -> E.Value
contentChange params =
    E.object [ ( "text", E.string params.text ) ]


paramsObject : DocumentChangeParams -> E.Value
paramsObject params =
    E.object
        [ ( "textDocument", uriObject params )
        , ( "contentChanges", E.list identity [ contentChange params ] )
        ]
