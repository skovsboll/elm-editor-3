module Lsp.Up.Context exposing (Context, encode)

import Json.Encode as E


type alias Context =
    { triggerKind : Int }


encode : Context -> E.Value
encode context =
    E.object
        [ ( "triggerKind", E.int context.triggerKind )
        ]
