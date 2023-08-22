module Layers.Adorns exposing (Adorn, AdornFragment(..), fakeAdorns, view)

import Html as H
import Html.Attributes as A
import Layers.Types exposing (ScrollPos)


type alias Adorn =
    List AdornFragment


type AdornFragment
    = Normal String
    | Error String


view : { scroll : ScrollPos, adorns : List Adorn } -> H.Attribute msg -> H.Html msg
view model styles =
    H.node "errors"
        [ styles
        , A.class "block absolute top-0 left-0 z-20 pointer-events-none will-change-transform h-auto transition-transform"
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
                (model.adorns
                    |> List.map
                        (\fragments ->
                            H.node
                                "adorn-line"
                                [ A.class "block pl-[40px] translate-y-[0.5rem]" ]
                                (fragments
                                    |> List.map
                                        (\fragment ->
                                            case fragment of
                                                Error txt ->
                                                    H.node "error" [ A.class "text-red-500 " ] [ String.repeat (String.length txt) "~" |> H.text ]

                                                Normal txt ->
                                                    H.node "no-error" [] [ String.repeat (String.length txt) " " |> H.text ]
                                        )
                                )
                        )
                )
            ]
        ]


fakeAdorns : String -> List Adorn
fakeAdorns src =
    src
        |> String.split "\n"
        |> List.map
            (\line ->
                line
                    |> String.split " "
                    |> List.concatMap
                        (\token ->
                            [ if token == "column=product_id," then
                                Error token

                              else
                                Normal token
                            , Normal " "
                            ]
                        )
            )
