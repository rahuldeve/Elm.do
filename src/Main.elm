import Html exposing (..)
import Json.Encode

import Tasks.Collection.Model exposing (..)
import Tasks.Collection.Actions exposing (initCollection)
import Update exposing (..)
import View exposing (..)
import Ports
import Subs exposing (..)


main : Program (Maybe Json.Encode.Value) Collection Msg
main =
  Html.programWithFlags
    { init = init
    , view = view
    , update = updateWithStorage
    , subscriptions = subscriptions
    }

init : Maybe Json.Encode.Value -> ( Collection, Cmd Msg )
init storedState = 
  case storedState of
          Just value -> (initCollection) ! []
          _ -> (initCollection) ! []


-- init : (Collection, Cmd Msg)
-- init = (initCollection, Cmd.none)
subscriptions : Collection -> Sub Msg
subscriptions _ = Ports.jqevents handleEvent 
