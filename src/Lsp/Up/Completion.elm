module Lsp.Up.Completion exposing (CompletionParams, encode)

import Json.Encode as E
import Lsp.Diagnostics


type alias CompletionParams =
    { uri : String
    , id : Int
    , position : Lsp.Diagnostics.Position
    }


encode : CompletionParams -> E.Value
encode params =
    let
        uriObject : E.Value
        uriObject =
            E.object [ ( "uri", E.string params.uri ) ]

        paramsObject : E.Value
        paramsObject =
            E.object
                [ ( "textDocument", uriObject )
                , ( "position", Lsp.Diagnostics.encodePosition params.position )
                ]
    in
    E.object
        [ ( "jsonrpc", E.string "2.0" )
        , ( "method", E.string "textDocument/completion" )
        , ( "id", E.int params.id )
        , ( "params", paramsObject )
        ]
