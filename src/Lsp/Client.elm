port module Lsp.Client exposing (..)


port outgoingMessage : String -> Cmd msg


port incomingMessage : (String -> msg) -> Sub msg
