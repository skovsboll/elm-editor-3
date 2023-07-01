module Hql.Parser exposing (Fragment, Line, parseHql)

--import Json.Decode exposing (maybe)

import Char exposing (isAlphaNum)
import Parser as P exposing ((|.), (|=))


type alias Token =
    ( Syntax, String )


type Syntax
    = Other
    | LineBreak
    | Comment
    | Symbol
    | StringLiteral
    | RegexLiteral
    | EscapeCharacter


tokenToFragment : Token -> Fragment
tokenToFragment ( syntax, text ) =
    case syntax of
        Other ->
            { text = text, color = "text-slate-200", tag = "other" }

        Comment ->
            { text = text, color = "text-green-400", tag = "comment" }

        Symbol ->
            { text = text, color = "text-slate-400", tag = "symbol" }

        StringLiteral ->
            { text = text, color = "text-orange-300", tag = "string" }

        RegexLiteral ->
            { text = text, color = "text-blue-300", tag = "regex" }

        EscapeCharacter ->
            { text = text, color = "text-orange-400", tag = "escape-char" }

        LineBreak ->
            { text = text, color = "", tag = "" }


parseHql : String -> List Line
parseHql src =
    --let
    --    defaultLines : List Line
    --    defaultLines =
    --        src
    --            |> String.split "\n"
    --            |> List.map (\l -> [ { text = l ++ "\n", tag = "no-syntax", color = "" } ])
    --in
    P.run toRevTokens src
        |> Result.map toLines
        |> Result.mapError
            (\deadends ->
                deadends
                    |> deadEndsToLines
            )
        |> errorAsDefault


deadEndsToLines : List P.DeadEnd -> List Line
deadEndsToLines deadends =
    deadends
        |> List.map
            (\l ->
                case l.problem of
                    P.Expecting s ->
                        s

                    P.ExpectingSymbol s ->
                        "symbol: " ++ s

                    P.ExpectingKeyword s ->
                        "keyword: " ++ s

                    P.ExpectingEnd ->
                        "end"

                    P.UnexpectedChar ->
                        "char"

                    P.Problem s ->
                        "problem: " ++ s

                    P.BadRepeat ->
                        "repeat"

                    _ ->
                        "something else"
            )
        |> List.map
            (\l -> [ { text = l ++ "\n", tag = "deadend", color = "text-red-300" } ])


errorAsDefault : Result a a -> a
errorAsDefault res =
    case res of
        Ok a ->
            a

        Err a ->
            a


type alias Line =
    List Fragment


type alias Fragment =
    { text : String
    , tag : String
    , color : String
    }


toLines : List Token -> List (List Fragment)
toLines revTokens =
    let
        help : Token -> ( List Line, List Fragment, Maybe Syntax ) -> ( List Line, List Fragment, Maybe Syntax )
        help (( syntax, text ) as token) ( lines, fragments, maybeLastSyntax ) =
            case syntax of
                LineBreak ->
                    ( fragments :: lines, [], Nothing )

                _ ->
                    if Just syntax == maybeLastSyntax then
                        -- Concat same syntax sequence to reduce html elements.
                        case fragments of
                            headFrag :: tailFrags ->
                                ( lines
                                , { headFrag | text = text ++ headFrag.text }
                                    :: tailFrags
                                , maybeLastSyntax
                                )

                            _ ->
                                ( lines
                                , tokenToFragment token :: fragments
                                , Just syntax
                                )

                    else
                        ( lines, tokenToFragment token :: fragments, Just syntax )
    in
    List.foldl help ( [], [], Nothing ) revTokens
        |> (\( lines, fragments, _ ) -> fragments :: lines)


toRevTokens : P.Parser (List Token)
toRevTokens =
    P.loop [] mainLoop


mainLoop : List Token -> P.Parser (P.Step (List Token) (List Token))
mainLoop revTokens =
    P.oneOf
        [ whitespace |> P.map (\n -> P.Loop (n :: revTokens))
        , stringLiteral revTokens
        , singleComment revTokens
        , regex revTokens

        --, function revTokens
        , otherSymbols revTokens
        , anything revTokens
        , P.succeed (P.Done revTokens)
        ]



-- Helpers
--function : List Token -> P.Parser (P.Step (List Token) (List Token))
--function =
--    P.succeed ()
--        |= P.chompIf Char.isAlphaNum
--        |= P.chompWhile Char.isAlphaNum
--        |= P.symbol "("


otherSymbols : List Token -> P.Parser (P.Step (List Token) (List Token))
otherSymbols revTokens =
    P.chompIf isSymbol
        |> P.getChompedString
        |> P.map (\b -> P.Loop (( Symbol, b ) :: revTokens))


anything : List ( Syntax, String ) -> P.Parser (P.Step (List Token) (List Token))
anything revTokens =
    P.chompIf (always True)
        |> P.getChompedString
        |> P.map (\b -> P.Loop (( Other, b ) :: revTokens))


stringLiteral : List Token -> P.Parser (P.Step (List Token) (List Token))
stringLiteral revTokens =
    P.symbol "\""
        |> P.andThen
            (\() ->
                P.loop (( StringLiteral, "\"" ) :: revTokens) stringLoop
            )
        |> P.map P.Loop


stringLoop : List Token -> P.Parser (P.Step (List Token) (List Token))
stringLoop revTokens =
    P.oneOf
        [ P.symbol "\\"
            |. P.chompIf (always True)
            |> P.getChompedString
            |> P.map (\str -> ( EscapeCharacter, str ))
            |> P.map (\ns -> P.Loop (ns :: revTokens))
        , P.symbol "\""
            |> P.map (\() -> ( StringLiteral, "\"" ))
            |> P.map (\ns -> P.Done (ns :: revTokens))
        , P.chompIf (always True)
            |> P.getChompedString
            |> P.map (\str -> ( StringLiteral, str ))
            |> P.map (\ns -> P.Loop (ns :: revTokens))
        ]


regex : List Token -> P.Parser (P.Step (List Token) (List Token))
regex revTokens =
    P.symbol "/"
        |> P.andThen
            (\() ->
                P.loop (( RegexLiteral, "/" ) :: revTokens) regexLoop
            )
        |> P.map P.Loop


regexLoop : List Token -> P.Parser (P.Step (List Token) (List Token))
regexLoop revTokens =
    P.oneOf
        [ P.symbol "\\"
            |. P.chompIf (always True)
            |> P.getChompedString
            |> P.map (\str -> ( EscapeCharacter, str ))
            |> P.map (\ns -> P.Loop (ns :: revTokens))
        , P.symbol "/"
            |> P.map (\() -> ( RegexLiteral, "/" ))
            |> P.map (\ns -> P.Done (ns :: revTokens))
        , P.chompIf (always True)
            |> P.getChompedString
            |> P.map (\str -> ( RegexLiteral, str ))
            |> P.map (\ns -> P.Loop (ns :: revTokens))
        ]


singleComment : List Token -> P.Parser (P.Step (List Token) (List Token))
singleComment revTokens =
    P.symbol "//"
        |> P.andThen
            (\() ->
                P.loop (( Comment, "//" ) :: revTokens) commentLoop
            )
        |> P.map P.Loop


commentLoop : List Token -> P.Parser (P.Step (List Token) (List Token))
commentLoop revTokens =
    P.oneOf
        [ P.symbol "\n"
            |> P.map (\_ -> ( LineBreak, "" ))
            |> P.map (\ns -> P.Done (ns :: revTokens))
        , P.chompIf (always True)
            |> P.getChompedString
            |> P.map (\str -> ( Comment, str ))
            |> P.map (\ns -> P.Loop (ns :: revTokens))
        ]


isSymbol : Char -> Bool
isSymbol char =
    String.contains (String.fromChar char) symbols


symbols : String
symbols =
    ".,_-!*?+;:<>%&/|\\"


whitespace : P.Parser Token
whitespace =
    P.oneOf
        [ space
        , lineBreak
        ]


isSpace : Char -> Bool
isSpace c =
    String.contains (String.fromChar c) " \t"


space : P.Parser Token
space =
    chompIfThenWhile isSpace
        |> P.getChompedString
        |> P.map (\b -> ( Other, b ))


lineBreak : P.Parser Token
lineBreak =
    P.symbol "\n"
        |> P.map (\_ -> ( LineBreak, "\n" ))


chompIfThenWhile : (Char -> Bool) -> P.Parser ()
chompIfThenWhile isNotRelevant =
    P.succeed ()
        |. P.chompIf isNotRelevant
        |. P.chompWhile isNotRelevant
