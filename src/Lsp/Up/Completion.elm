module Lsp.Up.Completion exposing (CompletionParams, encoder)

import Json.Encode as E
import Lsp.Up.Context exposing (Context)
import Lsp.Up.Position exposing (Position)


type alias CompletionParams =
    { uri : String
    , id : Int
    , position : Position
    , context : Context
    }


encoder : CompletionParams -> E.Value
encoder params =
    let
        uriObject : E.Value
        uriObject =
            E.object [ ( "uri", E.string params.uri ) ]

        paramsObject : E.Value
        paramsObject =
            E.object
                [ ( "textDocument", uriObject )
                , ( "position", Lsp.Up.Position.encode params.position )
                , ( "context", Lsp.Up.Context.encode params.context )
                ]
    in
    E.object
        [ ( "jsonrpc", E.string "2.0" )
        , ( "method", E.string "textDocument/completion" )
        , ( "id", E.int params.id )
        , ( "params", paramsObject )
        ]
