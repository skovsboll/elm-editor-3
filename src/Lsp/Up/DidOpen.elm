module Lsp.Up.DidOpen exposing (DidOpenTextDocumentParams, TextDocumentItem, encode)

import Json.Encode as E


type alias DidOpenTextDocumentParams =
    { textDocument : TextDocumentItem
    , id : Int
    }


type alias TextDocumentItem =
    { uri : String
    , languageId : String
    , version : Int
    , text : String
    }


encode : DidOpenTextDocumentParams -> E.Value
encode params =
    E.object
        [ ( "jsonrpc", E.string "2.0" )
        , ( "method", E.string "textDocument/didOpen" )
        , ( "id", E.int params.id )
        , ( "params", paramsEncoder params )
        ]


paramsEncoder : DidOpenTextDocumentParams -> E.Value
paramsEncoder params =
    E.object
        [ ( "textDocument", textDocumentItemEncoder params.textDocument ) ]


textDocumentItemEncoder : TextDocumentItem -> E.Value
textDocumentItemEncoder item =
    E.object
        [ ( "uri", E.string item.uri )
        , ( "languageId", E.string item.languageId )
        , ( "version", E.int item.version )
        , ( "text", E.string item.text )
        ]
