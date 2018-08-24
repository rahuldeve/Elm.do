module Tasks.Collection.Model exposing (..)

import Dict exposing (..)
import Tasks.Entry.Model exposing (..)


type alias Data = Dict Id Entry

type alias Collection =
    { root : Id, focus : Id, data : Data }
