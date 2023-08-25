module Lsp.Up.Position exposing (Position, encode)

import Json.Encode as E


type alias Position =
    { line : Int
    , character : Int
    }


encode : Position -> E.Value
encode pos =
    E.object
        [ ( "line", E.int pos.line )
        , ( "character", E.int pos.character )
        ]
