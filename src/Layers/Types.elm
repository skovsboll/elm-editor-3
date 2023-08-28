module Layers.Types exposing (Cursor, ScrollPos, Suggestion, SuggestionsList)


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


type alias Suggestion =
    { icon : String, code : String }


type alias SuggestionsList =
    Maybe ( List Suggestion, Suggestion, List Suggestion )
