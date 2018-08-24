module Tasks.Entry.Actions exposing (..)

import List.Extra exposing (..)
import Maybe

import Tasks.Entry.Model exposing (..)
import Tasks.Parser.Actions exposing (..)

------------------------------------------------Meta
getMeta : Entry -> Meta
getMeta e = e.meta

updateMeta : (Meta -> Meta) -> Entry -> Entry
updateMeta f e = {e | meta = f e.meta}


getId : Entry -> Id
getId e = getMeta e |> (\m -> m.id)



getParentId : Entry -> Id
getParentId e = getMeta e |> (\m -> m.parent)

updateParent newParentId e = updateMeta (\m-> {m | parent = newParentId}) e


---------------------------------------------------------Details
getDetails : Entry -> Details
getDetails e = e.details


updateDescription : String -> Entry -> Entry
updateDescription newDesc entry = 
    case parseDescription newDesc of
        Just newDetails -> {entry | details = newDetails}
        Nothing -> entry


---------------------------------------------------Children
hasChildren : Entry -> Bool
hasChildren e = List.length (e.meta.children) > 0

getChildrenId : Entry -> ChildOrder
getChildrenId e = e.meta.children


removeId : Id -> ChildOrder -> ChildOrder
removeId id order = List.filter (\x -> x /= id) order

removeChildId : Id -> Entry -> Entry
removeChildId id entry = 
    updateMeta (\m -> {m | children = removeId id m.children}) entry




getNextId : Id -> ChildOrder -> Maybe Id
getNextId id order = 
    let
        currPos = elemIndex id order
    in
    Maybe.andThen (\x -> getAt (x+1) order) currPos


getPrevId : Id -> ChildOrder -> Maybe Id
getPrevId id order = 
    let
        currPos = elemIndex id order
    in
    Maybe.andThen (\x -> getAt (x-1) order) currPos




setNextId : Id -> Id -> ChildOrder -> ChildOrder
setNextId id next order = 
    let
        go xs = case xs of
                    [] -> []
                    (x :: xs) ->    if x == id
                                    then (x :: next :: go xs)
                                    else x :: (go xs)
    in
    unique <| go order


setPrevId : Id -> Id -> ChildOrder -> ChildOrder
setPrevId id prev order = 
    let
        go xs = case xs of
                    [] -> []
                    (x :: xs) ->    if x == id
                                    then (prev :: x :: go xs)
                                    else x :: (go xs)
    in
    unique <| go order



createEmptyEntry : Id -> Id -> Entry
createEmptyEntry id parentId = 
    let
        meta = { id = id
               , done = False
               , editing = False
               , parent = parentId
               , children = []
               }

        details = { etype = Empty
                  , desc = ""
                  , opt = []
                  }
    in
    {meta = meta, details = details}


reorderChildren : Id -> Id -> Entry -> Entry
reorderChildren targetId prevId parentEntry = 
    let
        meta = parentEntry.meta
        newOrder = setNextId targetId prevId parentEntry.meta.children
        updatedMeta = {meta | children = newOrder}
    in
        {parentEntry | meta = updatedMeta}
