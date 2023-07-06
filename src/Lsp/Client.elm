port module Lsp.Client exposing (..)

import Json.Encode as J


port outgoingMessage : J.Value -> Cmd msg


port incomingMessage : (J.Value -> msg) -> Sub msg
