module Lsp.Up.DidOpenTextDocument exposing (DidOpenTextDocumentParams, TextDocumentItem, didOpenTextDocumentParamsEncoder)

import Json.Encode as E


type alias DidOpenTextDocumentParams =
    { textDocument : TextDocumentItem
    }


type alias TextDocumentItem =
    { uri : String
    , languageId : String
    , version : Int
    , text : String
    }


didOpenTextDocumentParamsEncoder : DidOpenTextDocumentParams -> E.Value
didOpenTextDocumentParamsEncoder params =
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
