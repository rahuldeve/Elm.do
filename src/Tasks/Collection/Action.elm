module Tasks.Collection.Actions exposing (..)

import Dict

import Tasks.Collection.Model exposing (..)
import Tasks.Entry.Model exposing (..)
import Tasks.Entry.Actions exposing (..)
import Tasks.Parser.Actions exposing (..)


genNewId : Collection -> Id
genNewId model = model.data
                    |> Dict.keys
                    |> List.maximum
                    |> Maybe.withDefault 0
                    |> (+) 1


changeFocus : Id -> Collection -> Collection
changeFocus id collection = {collection | focus = id}


getEntry : Id -> Collection -> Maybe Entry
getEntry id collection = Dict.get id collection.data

-- updateEntry : Id -> (Entry -> Entry) -> Collection -> Collection
-- updateEntry id func collection =
--     { collection | data = Dict.update id (Maybe.map func) collection.data }


-- deleteEntry : Id -> Collection -> Collection
-- deleteEntry id collection = 
--     { collection | data = Dict.update id (\_ -> Nothing) collection.data }




removeMapping : Id -> Collection -> Collection
removeMapping id collection = 
    { collection | data = Dict.update id (\_ -> Nothing) collection.data }

updateMapping : Id -> (Maybe Entry -> Maybe Entry) -> Collection -> Collection
updateMapping id func collection = 
    { collection | data = Dict.update id func collection.data }



bulkRemoveMapping : List Id -> Collection -> Collection
bulkRemoveMapping ids collection = List.foldr removeMapping collection ids



deleteEntryWithChildren : Id -> Collection -> Collection
deleteEntryWithChildren id collection = 
    let
        maybeEntry = getEntry id collection

        updateParent : Collection -> Maybe Collection
        updateParent collection =
            maybeEntry
                |> Maybe.map getParentId
                |> Maybe.andThen (\x -> getEntry x collection)
                |> Maybe.map (\pe -> updateMapping (getId pe) (\_ -> Just pe) collection)

    in

        maybeEntry
            |> Maybe.map getChildrenId
            |> Maybe.map (\childrenIds -> id :: childrenIds)
            |> Maybe.map (\childrenIds -> bulkRemoveMapping childrenIds collection)
            -- update parentEntry
            |> Maybe.andThen updateParent
            |> Maybe.withDefault collection
            

updateParentTo : Id -> Id -> Collection -> Collection
updateParentTo targetId newParentId collection =
    getEntry targetId collection
        --update old parent
        |> Maybe.map getParentId
        |> Maybe.andThen (\id -> getEntry id collection)
        |> Maybe.map (removeChildId targetId)
        |> Maybe.map (\e -> updateMapping e.meta.id (\_ -> Just e) collection)

        -- update target
        |> Maybe.andThen (getEntry targetId)
        |> Maybe.map (updateMeta (\m -> {m | parent = newParentId}))
        |> Maybe.map (\e -> updateMapping targetId (\_ -> Just e) collection)

        -- update newParent; append child
        |> Maybe.andThen (getEntry newParentId)
        |> Maybe.map (updateMeta (\m -> {m | children = targetId :: m.children}))
        |> Maybe.map (\e -> updateMapping newParentId (\_ -> Just e) collection)

        |> Maybe.withDefault collection


initCollection : Collection
initCollection =
    let
        meta = { id = 0, done = False, editing = False, parent = 0, children = [] }
        defDesc = { etype = Empty, desc = "", opt = [] }

        toDetails desc = Maybe.withDefault defDesc (parseDescription desc)

        data = Dict.fromList [ ( 0, {meta = {meta| children=[1,2]}, details = toDetails "# Title"} )
                             , ( 1,  {meta = {meta| id=1}, details = toDetails "### asdasdasd"} )
                             , ( 2, {meta = {meta| id=2}, details = toDetails "[] qqq"} )
                             ]
        
        -- focus = 0
    in
    {data = data, focus=0, root=0}

