module Lsp.Diagnostics exposing (Diagnostic, Position, Range, Severity, diagnosticDecoder, encodeDiagnostics, encodeDiagnosticsContext, encodePosition, encodeRange, parseDiagnostics, textDocumentEncoder)

import Html.Attributes exposing (disabled)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E


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



-- Encoders


textDocumentEncoder : String -> E.Value
textDocumentEncoder uri =
    E.object [ ( "uri", E.string uri ) ]


encodePosition : Position -> E.Value
encodePosition pos =
    E.object
        [ ( "line", E.int pos.line )
        , ( "character", E.int pos.character )
        ]


encodeRange : Range -> E.Value
encodeRange range =
    E.object
        [ ( "start", encodePosition range.start )
        , ( "end", encodePosition range.end )
        ]


encodeDiagnosticsContext : List Diagnostic -> E.Value
encodeDiagnosticsContext range =
    E.array
        [ ( "start", encodePosition range.start )
        , ( "end", encodePosition range.end )
        ]


encodeDiagnostics : Diagnostic -> E.Value
encodeDiagnostics diagnostic =
    E.object
        [ ( "range", encodeRange diagnostic.range )
        , ( "severity", encodeSeverity diagnostic.severity )
        , ( "code", E.string diagnostic.code )
        , ( "source", E.string diagnostic.source )
        , ( "message", E.string diagnostic.message )
        ]


encodeSeverity : Severity -> E.Value
encodeSeverity sev =
    E.int
        (case sev of
            Error ->
                1

            Warning ->
                2

            Information ->
                3

            Hint ->
                4
        )



-- Decoders


positionDecoder : Decoder Position
positionDecoder =
    D.map2 Position
        (D.field "line" D.int)
        (D.field "character" D.int)


rangeDecoder : Decoder Range
rangeDecoder =
    D.map2 Range
        (D.field "start" positionDecoder)
        (D.field "end" positionDecoder)


severityDecoder : Decoder Severity
severityDecoder =
    D.int
        |> D.andThen
            (\num ->
                case num of
                    1 ->
                        D.succeed Error

                    2 ->
                        D.succeed Warning

                    3 ->
                        D.succeed Information

                    4 ->
                        D.succeed Hint

                    _ ->
                        D.fail "Invalid severity"
            )


diagnosticDecoder : Decoder Diagnostic
diagnosticDecoder =
    D.map5 Diagnostic
        (D.field "range" rangeDecoder)
        (D.field "severity" severityDecoder)
        (D.field "code" D.string)
        (D.field "source" D.string)
        (D.field "message" D.string)
