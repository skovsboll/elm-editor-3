port module Lsp.Ports exposing (incomingMessage, outgoingMessage)


port outgoingMessage : String -> Cmd msg


port incomingMessage : (String -> msg) -> Sub msg
