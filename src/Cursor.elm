module Cursor exposing (fromTextPosition, toPosition)

import Layers.Types exposing (Cursor)
import Lsp.Diagnostics


fontHeight : Float
fontHeight =
    22.0


fontWidth : Float
fontWidth =
    9.6


fromTextPosition : String -> Int -> Cursor
fromTextPosition text position =
    let
        lines =
            text
                |> String.left position
                |> String.split "\n"

        row =
            lines |> List.length

        y =
            toFloat row * fontHeight - 10

        col =
            case List.reverse lines of
                hd :: _ ->
                    String.length hd + 1

                [] ->
                    0

        x =
            toFloat (col - 1) * fontWidth
    in
    { x = x, y = y, row = row, col = col, pos = position }


toPosition : Cursor -> Lsp.Diagnostics.Position
toPosition cursor =
    { line = cursor.row
    , character = cursor.col
    }
