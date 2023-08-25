module Lsp.Up.Initialize exposing
    ( ClientCapabilities
    , CompletionCapabilities
    , CompletionItemCapabilities
    , InitializeParams
    , TextDocumentCapabilities
    , WorkspaceCapabilities
    , encode
    , initializeParamsEncoder
    )

import Json.Encode as E


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


encode : InitializeParams -> E.Value
encode params =
    E.object
        [ ( "jsonrpc", E.string "2.0" )
        , ( "method", E.string "initialize" )
        , ( "params", initializeParamsEncoder params )
        ]


initializeParamsEncoder : InitializeParams -> E.Value
initializeParamsEncoder params =
    E.object
        [ ( "processId", Maybe.map E.int params.processId |> Maybe.withDefault E.null )
        , ( "rootUri", Maybe.map E.string params.rootUri |> Maybe.withDefault E.null )
        , ( "capabilities", clientCapabilitiesEncoder params.capabilities )
        , ( "trace", E.string params.trace )
        ]


clientCapabilitiesEncoder : ClientCapabilities -> E.Value
clientCapabilitiesEncoder capabilities =
    E.object
        [ ( "textDocument", textDocumentCapabilitiesEncoder capabilities.textDocument )
        , ( "workspace", workspaceCapabilitiesEncoder capabilities.workspace )
        ]


textDocumentCapabilitiesEncoder : TextDocumentCapabilities -> E.Value
textDocumentCapabilitiesEncoder capabilities =
    E.object
        [ ( "completion", completionCapabilitiesEncoder capabilities.completion ) ]


completionCapabilitiesEncoder : CompletionCapabilities -> E.Value
completionCapabilitiesEncoder capabilities =
    E.object
        [ ( "completionItem", completionItemCapabilitiesEncoder capabilities.completionItem ) ]


completionItemCapabilitiesEncoder : CompletionItemCapabilities -> E.Value
completionItemCapabilitiesEncoder capabilities =
    E.object
        [ ( "snippetSupport", E.bool capabilities.snippetSupport ) ]


workspaceCapabilitiesEncoder : WorkspaceCapabilities -> E.Value
workspaceCapabilitiesEncoder capabilities =
    E.object
        [ ( "workspaceFolders", E.bool capabilities.workspaceFolders ) ]
