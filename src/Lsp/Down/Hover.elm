module Lsp.Down.Hover exposing (..)

import Json.Decode as Decode exposing (Decoder, field, maybe, string)
import Lsp.Diagnostics exposing (Position, positionDecoder)


type alias Hover =
    { contents : MarkupContent
    , range : Maybe Range
    }


type alias MarkupContent =
    { kind : String
    , value : String
    }


type alias Range =
    { start : Position
    , end : Position
    }


hoverDecoder : Decoder Hover
hoverDecoder =
    Decode.map2 Hover
        (field "contents" markupContentDecoder)
        (field "range" (maybe rangeDecoder))


markupContentDecoder : Decoder MarkupContent
markupContentDecoder =
    Decode.map2 MarkupContent
        (field "kind" string)
        (field "value" string)


rangeDecoder : Decoder Range
rangeDecoder =
    Decode.map2 Range
        (field "start" positionDecoder)
        (field "end" positionDecoder)
