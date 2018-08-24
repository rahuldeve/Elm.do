port module Ports exposing (..)

-- import Model exposing (..)
import Json.Encode exposing (Value)
-- import Update exposing (..)
-- import Subs exposing (..)

port setStorage : Value -> Cmd msg

port refreshJQuery : () -> Cmd msg


type alias JQEvent = {name : String, target : Int, value: String}
port jqevents : (JQEvent -> msg) -> Sub msg
