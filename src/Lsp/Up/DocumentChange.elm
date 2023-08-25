module Lsp.Up.DocumentChange exposing (DocumentChangeParams, toString)

import Json.Encode as E


type alias DocumentChangeParams =
    { uri : String
    , version : Int
    , text : String
    }


toString : DocumentChangeParams -> String
toString params =
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
        |> E.encode 0
