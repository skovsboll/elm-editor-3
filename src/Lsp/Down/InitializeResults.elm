module Lsp.Down.InitializeResults exposing (CompletionProvider, InitializeResult, ServerCapabilities, SignatureHelpProvider, initializeResultDecoder)

import Json.Decode as Decode exposing (Decoder, bool, field, int, string)


type alias InitializeResult =
    { capabilities : ServerCapabilities
    }


type alias ServerCapabilities =
    { textDocumentSync : Maybe Int
    , hoverProvider : Maybe Bool
    , completionProvider : Maybe CompletionProvider
    , signatureHelpProvider : Maybe SignatureHelpProvider

    -- ... potentially other capabilities
    }


type alias CompletionProvider =
    { resolveProvider : Bool
    , triggerCharacters : List String
    }


type alias SignatureHelpProvider =
    { triggerCharacters : List String
    , retriggerCharacters : List String
    }


initializeResultDecoder : Decoder InitializeResult
initializeResultDecoder =
    Decode.map InitializeResult (field "capabilities" serverCapabilitiesDecoder)


serverCapabilitiesDecoder : Decoder ServerCapabilities
serverCapabilitiesDecoder =
    Decode.map4 ServerCapabilities
        (field "textDocumentSync" (Decode.maybe int))
        (field "hoverProvider" (Decode.maybe bool))
        (field "completionProvider" (Decode.maybe completionProviderDecoder))
        (field "signatureHelpProvider" (Decode.maybe signatureHelpProviderDecoder))


completionProviderDecoder : Decoder CompletionProvider
completionProviderDecoder =
    Decode.map2 CompletionProvider
        (field "resolveProvider" bool)
        (field "triggerCharacters" (Decode.list string))


signatureHelpProviderDecoder : Decoder SignatureHelpProvider
signatureHelpProviderDecoder =
    Decode.map2 SignatureHelpProvider
        (field "triggerCharacters" (Decode.list string))
        (field "retriggerCharacters" (Decode.list string))
