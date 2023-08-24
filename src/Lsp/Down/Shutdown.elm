module Lsp.Down.Shutdown exposing (..)

import Json.Decode as Decode exposing (Decoder)



-- Shutdown doesn't typically carry parameters


type alias ShutdownParams =
    {}


shutdownDecoder : Decoder ShutdownParams
shutdownDecoder =
    Decode.succeed ShutdownParams
