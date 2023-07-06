module Lsp.DocumentChange exposing (DocumentChangeParams, encodeDocumentChangeMessage)

import Json.Encode as E


type alias DocumentChangeParams =
    { uri : String
    , version : Int
    , text : String
    }


encodeDocumentChangeMessage : DocumentChangeParams -> E.Value
encodeDocumentChangeMessage params =
    let
        uriObject : E.Value
        uriObject =
            E.object [ ( "uri", E.string params.uri ), ( "version", E.int params.version ) ]

        contentChange : E.Value
        contentChange =
            E.object [ ( "text", E.string params.text ) ]

        paramsObject : E.Value
        paramsObject =
            E.object
                [ ( "textDocument", uriObject )
                , ( "contentChanges", E.list identity [ contentChange ] )
                ]
    in
    E.object
        [ ( "jsonrpc", E.string "2.0" )
        , ( "method", E.string "textDocument/didChange" )
        , ( "params", paramsObject )
        ]
