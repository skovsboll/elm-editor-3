module Layers.TextArea exposing (view)

import Html as H
import Html.Attributes as A
import Html.Events as E
import Json.Decode as J
import Layers.Types exposing (ScrollPos, SuggestionsList)


view :
    { a | scroll : ScrollPos, text : String, suggestions : SuggestionsList }
    -> H.Attribute msg
    -> J.Decoder { message : msg, stopPropagation : Bool, preventDefault : Bool }
    -> J.Decoder msg
    -> (String -> msg)
    -> H.Html msg
view model styles keyDecoder parseScrollEvent onInput =
    H.textarea
        [ styles
        , A.readonly (model.suggestions /= Nothing)
        , E.custom "keyup" keyDecoder
        , E.custom "click" keyDecoder
        , A.spellcheck False
        , A.class "absolute z-0 h-full p-2 pl-[48px] caret-red-500 text-transparent resize-none"
        , E.onInput onInput
        , E.on "scroll" parseScrollEvent
        ]
        [ H.text model.text
        ]
