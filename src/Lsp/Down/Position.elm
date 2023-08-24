module Lsp.Down.Position exposing (..)

import Json.Decode as Decode exposing (Decoder, field, int, string)


type alias TextDocumentPositionParams =
    { textDocument : TextDocumentIdentifier
    , position : Position
    }


type alias TextDocumentIdentifier =
    { uri : String }


type alias Position =
    { line : Int
    , character : Int
    }



-- Common textDocument decoder


textDocumentPositionParamsDecoder : Decoder TextDocumentPositionParams
textDocumentPositionParamsDecoder =
    Decode.map2 TextDocumentPositionParams
        (field "textDocument" textDocumentIdentifierDecoder)
        (field "position" positionDecoder)


textDocumentIdentifierDecoder : Decoder TextDocumentIdentifier
textDocumentIdentifierDecoder =
    Decode.map TextDocumentIdentifier (field "uri" string)


positionDecoder : Decoder Position
positionDecoder =
    Decode.map2 Position
        (field "line" int)
        (field "character" int)
