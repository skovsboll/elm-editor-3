module Layers.Types exposing (ScrollPos, Cursor)


type alias ScrollPos =
    { top : Float
    , left : Float
    }


type alias Cursor =
    { x : Float
    , y : Float
    , col : Int
    , row : Int
    , pos : Int
    }
