module Update exposing (..)

import Task
import Dom
import Update.Extra exposing (..)

import Tasks.Collection.Actions exposing (..)
import Tasks.Collection.Model exposing (..)
import Tasks.Entry.Actions exposing (..)
import Tasks.Entry.Model exposing (..)

import Persistence.Serialization exposing (..)
import Ports exposing (..)

type Msg =  Focus Int
            | FocusResult (Result Dom.Error ())

            | SingleChange Int SingleMsg
            | MultipleChange MultipleMsg

            | MakeChildOf Id Id Id
            | AddEmptyForAndFocus Id Id
            | UpdateAndBlur Id String

            | NoOp


type SingleMsg = ToggleEntry
                | UpdateEntryDescription String
                | ToggleEdit

                | UpdateChildrenOrder ChildOrder
            
                | ReorderChildren Id Id
                | CreateEmpty Id Id

type MultipleMsg = DeleteEntry Id
                 | ChangeFocusTo Id
                 | UpdateParentIdTo Id Id
                 

{-| We want to `setStorage` on every update. This function adds the setStorage
command for every step of the update function.
-}
updateWithStorage : Msg -> Collection -> ( Collection, Cmd Msg )
updateWithStorage msg collection =
    let
        ( newCollection, cmds ) =
            update msg collection

    in
        ( newCollection
        , Cmd.batch [ encodeModel newCollection |> setStorage, cmds, refreshJQuery ()]
        )


update : Msg -> Collection -> (Collection, Cmd Msg)
update msg collection =
    case msg of
        NoOp -> (collection, Cmd.none)
        Focus id -> (collection, Dom.focus (toString id) |> Task.attempt FocusResult)
        FocusResult _ -> (collection, Cmd.none)

        SingleChange id msg -> singleUpdate msg id collection
        MultipleChange msg -> multipleUpdate msg collection

        UpdateAndBlur id str -> update (SingleChange id <| UpdateEntryDescription str) collection
                                    |> andThen update (SingleChange id <| ToggleEdit)

        MakeChildOf targetId newParentId prevEntryId -> 
            -- collection
                update (MultipleChange <| UpdateParentIdTo newParentId targetId) collection
                    |> andThen update (SingleChange newParentId <| ReorderChildren targetId prevEntryId)
                    -- |> andThen (singleUpdate (ReorderChildren targetId prevEntryId) newParentId)
                -- |> (\c -> c ! [])

        AddEmptyForAndFocus parentId prevEntryId ->
            let
                newId =  genNewId collection
            in
                -- collection
                update (SingleChange parentId <| CreateEmpty newId parentId) collection
                    |> andThen update (SingleChange parentId <| ReorderChildren newId prevEntryId)
                    |> andThen update (Focus newId)



multipleUpdate : MultipleMsg -> Collection -> (Collection, Cmd Msg)
multipleUpdate msg collection = case msg of
    ChangeFocusTo id -> changeFocus id collection ! []
    DeleteEntry id -> deleteEntryWithChildren id collection ! []

    UpdateParentIdTo targetId newParentId ->  updateParentTo targetId newParentId collection ! []



singleUpdate : SingleMsg -> Int -> Collection -> (Collection, Cmd Msg)
singleUpdate msg id collection = 
    let
        maybeEntry = getEntry id collection
        -- updateCollection maybeEntry = maybeEntry
        --                                 |> Maybe.map (\e -> updateMapping id (\_ -> Just e) collection)
        --                                 |> Maybe.withDefault collection

        updateAndSave f = maybeEntry
                            |> Maybe.map f
                            |> Maybe.map (\e -> updateMapping id (\_ -> Just e) collection)
                            |> Maybe.withDefault collection
    in
        case msg of
            ToggleEntry -> updateAndSave (updateMeta (\m -> { m | done = not m.done })) ! []

            ToggleEdit -> updateAndSave (updateMeta (\m-> {m | editing = not m.editing}))
                                |> update (Focus id)

            UpdateChildrenOrder newOrder -> updateAndSave (updateMeta (\m-> {m | children = newOrder}))  ! []

            UpdateEntryDescription newDesc -> updateAndSave (updateDescription newDesc)  ! []

            ReorderChildren targetId prevId -> updateAndSave (reorderChildren targetId prevId)  ! []

            CreateEmpty id parentId -> createEmptyEntry id parentId
                                            |> (\e -> updateMapping id (\_ -> Just e) collection)
                                            |> (\c -> c ! [])

