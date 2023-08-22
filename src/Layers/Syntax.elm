module Layers.Syntax exposing (view)

import Hql.Parser exposing (Line)
import Html as H
import Html.Attributes as A
import Layers.Types exposing (ScrollPos)


view : { a | scroll : ScrollPos, ast : List Line } -> H.Attribute msg -> H.Html msg
view model styles =
    H.node "syntax"
        [ styles
        , A.class "block absolute z-10 top-0 left-0 pointer-events-none will-change-transform h-auto transition-transform"
        , A.style "transform"
            ("translate("
                ++ String.fromFloat -model.scroll.left
                ++ "px, "
                ++ String.fromFloat -model.scroll.top
                ++ "px)"
            )
        ]
        [ H.pre [ A.class "p-2 m-0 align-left h-full bg-transparent border-none" ]
            [ H.code []
                (model.ast
                    |> List.indexedMap
                        (\idx fragments ->
                            H.node "code-line"
                                [ A.class "block pl-[0px]"
                                , A.class "before:content-[attr(data-lino)] before:inline-block before:text-right before:w-[40px] before:p-0 before:pr-[20px] before:opacity-25 before:text-white"
                                , A.attribute "data-lino" (String.fromInt idx)
                                ]
                                (fragments
                                    |> List.map (\{ text, tag, color } -> H.node tag [ A.class color ] [ H.text text ])
                                )
                        )
                )
            ]
        ]
