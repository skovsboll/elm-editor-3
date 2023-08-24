module Lsp.Up.Initialize exposing
    ( ClientCapabilities
    , CompletionCapabilities
    , CompletionItemCapabilities
    , InitializeParams
    , TextDocumentCapabilities
    , WorkspaceCapabilities
    , initializeParamsEncoder
    , toString
    )

import Json.Encode as Encode


type alias InitializeParams =
    { processId : Maybe Int
    , rootUri : Maybe String
    , capabilities : ClientCapabilities
    , trace : String
    }


type alias ClientCapabilities =
    { textDocument : TextDocumentCapabilities
    , workspace : WorkspaceCapabilities
    }


type alias TextDocumentCapabilities =
    { completion : CompletionCapabilities }


type alias CompletionCapabilities =
    { completionItem : CompletionItemCapabilities }


type alias CompletionItemCapabilities =
    { snippetSupport : Bool }


type alias WorkspaceCapabilities =
    { workspaceFolders : Bool }


toString : InitializeParams -> String
toString params =
    Encode.object
        [ ( "jsonrpc", Encode.string "2.0" )
        , ( "method", Encode.string "initialize" )
        , ( "params", initializeParamsEncoder params )
        ]
        |> Encode.encode 0


initializeParamsEncoder : InitializeParams -> Encode.Value
initializeParamsEncoder params =
    Encode.object
        [ ( "processId", Maybe.map Encode.int params.processId |> Maybe.withDefault Encode.null )
        , ( "rootUri", Maybe.map Encode.string params.rootUri |> Maybe.withDefault Encode.null )
        , ( "capabilities", clientCapabilitiesEncoder params.capabilities )
        , ( "trace", Encode.string params.trace )
        ]


clientCapabilitiesEncoder : ClientCapabilities -> Encode.Value
clientCapabilitiesEncoder capabilities =
    Encode.object
        [ ( "textDocument", textDocumentCapabilitiesEncoder capabilities.textDocument )
        , ( "workspace", workspaceCapabilitiesEncoder capabilities.workspace )
        ]


textDocumentCapabilitiesEncoder : TextDocumentCapabilities -> Encode.Value
textDocumentCapabilitiesEncoder capabilities =
    Encode.object
        [ ( "completion", completionCapabilitiesEncoder capabilities.completion ) ]


completionCapabilitiesEncoder : CompletionCapabilities -> Encode.Value
completionCapabilitiesEncoder capabilities =
    Encode.object
        [ ( "completionItem", completionItemCapabilitiesEncoder capabilities.completionItem ) ]


completionItemCapabilitiesEncoder : CompletionItemCapabilities -> Encode.Value
completionItemCapabilitiesEncoder capabilities =
    Encode.object
        [ ( "snippetSupport", Encode.bool capabilities.snippetSupport ) ]


workspaceCapabilitiesEncoder : WorkspaceCapabilities -> Encode.Value
workspaceCapabilitiesEncoder capabilities =
    Encode.object
        [ ( "workspaceFolders", Encode.bool capabilities.workspaceFolders ) ]
