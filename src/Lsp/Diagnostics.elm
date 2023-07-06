module Lsp.Diagnostics exposing (Diagnostic, Position, Range, Severity, parseDiagnostics)

import Json.Decode as Decode exposing (Decoder)


type alias Position =
    { line : Int
    , character : Int
    }


type alias Range =
    { start : Position
    , end : Position
    }


type Severity
    = Error
    | Warning
    | Information
    | Hint


type alias Diagnostic =
    { range : Range
    , severity : Severity
    , code : String
    , source : String
    , message : String
    }


positionDecoder : Decoder Position
positionDecoder =
    Decode.map2 Position
        (Decode.field "line" Decode.int)
        (Decode.field "character" Decode.int)


rangeDecoder : Decoder Range
rangeDecoder =
    Decode.map2 Range
        (Decode.field "start" positionDecoder)
        (Decode.field "end" positionDecoder)


severityDecoder : Decoder Severity
severityDecoder =
    Decode.int
        |> Decode.andThen
            (\num ->
                case num of
                    1 ->
                        Decode.succeed Error

                    2 ->
                        Decode.succeed Warning

                    3 ->
                        Decode.succeed Information

                    4 ->
                        Decode.succeed Hint

                    _ ->
                        Decode.fail "Invalid severity"
            )


diagnosticDecoder : Decoder Diagnostic
diagnosticDecoder =
    Decode.map5 Diagnostic
        (Decode.field "range" rangeDecoder)
        (Decode.field "severity" severityDecoder)
        (Decode.field "code" Decode.string)
        (Decode.field "source" Decode.string)
        (Decode.field "message" Decode.string)


parseDiagnostics : String -> Result Decode.Error Diagnostic
parseDiagnostics jsonString =
    Decode.decodeString diagnosticDecoder jsonString
