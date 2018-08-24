module Tasks.Entry.Model exposing (..)


type alias Id = Int


type EntryType = Checkbox
               | Bullet
               | Numbered String
               | Heading Int

               | LineBreak
               | Text

               | Empty


type Optional
    = Due String
    | Priority Int


type alias Details =
    { etype: EntryType
    , desc : String
    , opt : List Optional
    }


type alias ChildOrder = List Id

type alias Meta =
    { id : Id
    , done : Bool
    , editing : Bool
    , parent : Id
    , children : ChildOrder
    }


type alias Props = Bool

type alias Entry = {meta : Meta, details : Details}
