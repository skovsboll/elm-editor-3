module Layers.DiffGutter exposing (view)

import Diff exposing (Change)
import Html as H
import Html.Attributes as A
import Layers.Types exposing (ScrollPos)


view : { a | scroll : ScrollPos, diff : List Change } -> H.Attribute msg -> H.Html msg
view model styles =
    H.node "diff"
        [ styles
        , A.class "block absolute z-50 top-0 left-8 pointer-events-none will-change-transform h-auto transition-transform"
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
                (model.diff
                    |> List.concatMap
                        (\line ->
                            case line of
                                Diff.NoChange _ ->
                                    [ H.node "no-change" [ A.class "block w-1" ] [ H.text " " ]
                                    ]

                                Diff.Added _ ->
                                    [ H.node "line-added" [ A.class "block bg-green-600 w-1" ] [ H.text " " ] ]

                                Diff.Changed _ _ ->
                                    [ H.button
                                        [ A.id "a"
                                        , A.class "block bg-yellow-600 w-1"
                                        , A.attribute "popovertarget" "b"
                                        ]
                                        [ H.text " " ]

                                    --, H.node "pop-over"
                                    --    [ A.id "b"
                                    --    , A.class "block bottom-[calc(anchor(bottom))] left-[anchor(center)] translate-[-50% 0]"
                                    --    , A.attribute "popover" "auto"
                                    --    , A.attribute "anchor" "a"
                                    --    ]
                                    --    [ H.node "diff"
                                    --        [ A.class "block flex flex-col" ]
                                    --        [ H.node "diff-before" [ A.class "text-red-500" ] [ H.text before ]
                                    --        , H.node "diff-after" [ A.class "text-green-500" ] [ H.text after ]
                                    --        , H.button [ A.class "button" ] [ H.text "Revert" ]
                                    --        ]
                                    --    ]
                                    ]

                                Diff.Removed _ ->
                                    [ H.node "line-removed" [ A.class "block bg-red-600 w-1" ] [ H.text " " ] ]
                        )
                )
            ]
        ]
