module Lsp.Down exposing (MessageType(..), messageDecoder)

import Json.Decode as D exposing (Decoder)
import Lsp.Down.Completion as Completion
import Lsp.Down.Hover as Hover
import Lsp.Down.Initialize as Initialize


type MessageType
    = Completion Completion.CompletionResult
    | Hover Hover.Result
    | Initialize Initialize.Result


messageDecoder : Decoder MessageType
messageDecoder =
    D.oneOf
        [ Completion.decoder |> D.map Completion
        , Hover.decoder |> D.map Hover
        , Initialize.decoder |> D.map Initialize
        ]
