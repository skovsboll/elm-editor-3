port module Lsp.Ports exposing (incomingMessage, outgoingMessage)

import Json.Encode as J


port outgoingMessage : String -> Cmd msg


port incomingMessage : (String -> msg) -> Sub msg
