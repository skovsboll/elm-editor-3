module Lsp.Up.SemanticTokensFull exposing (encode)

import Json.Encode as E
import Lsp.Diagnostics exposing (textDocumentEncoder)


type alias SemanticTokensParams =
    { uri : String
    , languageId : String
    , id : Int
    }


encode : SemanticTokensParams -> E.Value
encode params =
    E.object
        [ ( "jsonrpc", E.string "2.0" )
        , ( "method", E.string "textDocument/semanticTokens/full" )
        , ( "id", E.int params.id )
        , ( "params", paramsEncoder params )
        ]


paramsEncoder : SemanticTokensParams -> E.Value
paramsEncoder params =
    E.object [ ( "textDocument", textDocumentEncoder params.uri ) ]
