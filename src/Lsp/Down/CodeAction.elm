module Lsp.Down.CodeAction exposing (..)

import Json.Decode as Decode exposing (Decoder, field, string)
import Lsp.Down.Diagnostics exposing (Range)
import Lsp.Down.Hover exposing (rangeDecoder)
import Lsp.Down.Position exposing (TextDocumentIdentifier, textDocumentIdentifierDecoder)


type alias CodeActionRequest =
    { textDocument : TextDocumentIdentifier
    , range : Range
    , context : CodeActionContext
    }


type alias CodeActionContext =
    { diagnostics : List Diagnostic }


type alias Diagnostic =
    { range : Range
    , message : String

    -- ... potentially other fields like severity, etc.
    }


codeActionRequestDecoder : Decoder CodeActionRequest
codeActionRequestDecoder =
    Decode.map3 CodeActionRequest
        (field "textDocument" textDocumentIdentifierDecoder)
        (field "range" rangeDecoder)
        (field "context" codeActionContextDecoder)


codeActionContextDecoder : Decoder CodeActionContext
codeActionContextDecoder =
    Decode.map CodeActionContext (field "diagnostics" (Decode.list diagnosticDecoder))


diagnosticDecoder : Decoder Diagnostic
diagnosticDecoder =
    Decode.map2 Diagnostic
        (field "range" rangeDecoder)
        (field "message" string)
