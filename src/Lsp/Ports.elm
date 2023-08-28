port module Lsp.Ports exposing (incomingMessage, outgoingMessage, webSocketReady)


port outgoingMessage : String -> Cmd msg


port incomingMessage : (String -> msg) -> Sub msg


port webSocketReady : (String -> msg) -> Sub msg
