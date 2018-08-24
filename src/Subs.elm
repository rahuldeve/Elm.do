module Subs exposing (..)

import Ports exposing (..)

import Update exposing (..)

import Debug exposing (..)


handleEvent : JQEvent -> Msg
handleEvent ({name, target, value} as ev) = 
    let
        asInt val = String.toInt val |> Result.withDefault 0
    in
    case name of
        "clickforedit"      -> SingleChange target ToggleEdit
        -- "enterpressed"      -> 
        -- "tabpressed"        -> MakeChildOfPrevEntry target
        "backspaceonempty"  -> MultipleChange <| DeleteEntry target
        "attemptupdate"     -> UpdateAndBlur target value
        "toggle"            -> SingleChange target ToggleEntry
        "zoomto"            -> MultipleChange <| ChangeFocusTo target
        "delete"            -> MultipleChange <| DeleteEntry target
        -- "dropafter"         -> DropAfter target value
        _                   -> NoOp


