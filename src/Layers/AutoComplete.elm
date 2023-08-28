module Layers.AutoComplete exposing (view)

import Html as H
import Html.Attributes as A
import Layers.Types exposing (Cursor, ScrollPos, SuggestionsList)


view : { a | scroll : ScrollPos, cursor : Cursor, suggestions : SuggestionsList } -> H.Html msg
view model =
    case model.suggestions of
        Nothing ->
            H.node "no-suggestions" [] []

        Just ( pre, selected, post ) ->
            H.node "auto-complete"
                [ A.class "block absolute z-50 h-32 bg-slate-700 w-64 shadow rounded border border-slate-600 font-mono tracking-normal whitespace-pre leading-snug text-sm p-1"
                , A.style "left" (String.fromFloat (64 + model.cursor.x) ++ "px")
                , A.style "top" (String.fromFloat (model.cursor.y - model.scroll.top) ++ "px")
                ]
                [ H.ul [ A.class "text-white overflow-scroll h-full" ]
                    ((pre |> List.reverse |> List.map (\{ icon, code } -> H.li [] [ H.text icon, H.text " ", H.text code ]))
                        ++ [ H.li [ A.class "bg-gray-600" ] [ H.text selected.icon, H.text " ", H.text selected.code ]
                           ]
                        ++ (post |> List.map (\{ icon, code } -> H.li [] [ H.text icon, H.text " ", H.text code ]))
                    )
                ]
