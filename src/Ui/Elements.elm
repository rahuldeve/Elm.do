module Ui.Elements exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Array exposing  (..)



bullet icon_attrs icon_desc desc_attr desc_contents = 
    let
        bulletDiv = div [ class "pretty p-default p-round p-fill p-locked" ]  
                          [ input ( [type_ "checkbox" ] ++ icon_attrs ) [text "[]"]
                          , div [class "state p-success"] [ label [] [] ]
                          ]

        wrapper = div icon_attrs [bulletDiv]

        descDiv = p ([class "checkbox-label"] ++ desc_attr)
                    desc_contents
    in
    [ wrapper
    , descDiv
    ]


checkbox : List (Attribute msg) -> a -> List (Attribute msg) -> List (Html msg) -> List (Html msg)
checkbox icon_attrs icon_desc desc_attr desc_contents = 
    let
        checkboxDiv = div ([ class "checkbox-icon pretty p-default p-curve p-smooth" ]  ++ icon_attrs)
                          [ input ( [type_ "checkbox" ] ++ icon_attrs ) [text "[]"]
                          , div [class "state"] [ label [] [] ]
                          ]

        wrapper = div icon_attrs [checkboxDiv]

        descDiv = p ([class "checkbox-label"] ++ desc_attr)
                    desc_contents
    in
    [ checkboxDiv
    , descDiv
    ]



numbered num icon_attrs icon_desc desc_attr desc_contents =
    let
        iconDiv = label ([class "number-icon"] ++ icon_attrs) [(text <| num ++ ".")] 
        wrapper = div icon_attrs [iconDiv]

        descDiv = p ([class "checkbox-label"] ++ desc_attr) desc_contents
    in
    [ iconDiv
    , descDiv
    ]


heading num icon_attrs icon_desc desc_attr desc_contents =
    let 
        hfuncs = Array.fromList [h4, h5, h6]
        bulmaClasses = Array.fromList ["is-4", "is-5", "is-6"]

        f = Maybe.withDefault p <| Array.get (num-1) hfuncs
        appendClass = Maybe.withDefault "" <| Array.get (num-1) bulmaClasses

        str = String.repeat num "#"
        iconDiv = f [class ("heading-icon title " ++ appendClass) ] [(text str)]

        descDiv = f ( [class ("description title " ++ appendClass) ] ++ desc_attr ) desc_contents
    in
    [ iconDiv
    , descDiv
    ]
    

blank _ _ desc_attr desc_contents  = [ p desc_attr desc_contents ]
