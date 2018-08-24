module Persistence.Serialization exposing (..)

import Json.Decode as Decode
import Json.Decode exposing (andThen, decodeString)
import Json.Encode as Encode
import Dict exposing (..)

import Tasks.Collection.Model exposing (..)
import Tasks.Entry.Model exposing (..)
import Tasks.Parser.Actions exposing (..)



entryTypeToString : EntryType -> String
entryTypeToString e = 
    let str = case e of
                Checkbox        -> "[]"
                Bullet          -> "*"
                (Numbered n)    -> n ++ "."
                (Heading n)     -> String.repeat n "#"

                LineBreak       -> ""
                Text            -> ""
                Empty           -> ""
    in
    str


optionalToString : Optional -> String
optionalToString o = 
    let str = case o of
                (Due str) -> "@" ++ str
                (Priority n) -> String.repeat n "!"
    in
    str


encodeDetails : Details -> Encode.Value
encodeDetails d = 
    let
        etypeStr = entryTypeToString d.etype
        descStr = d.desc
        optValStr = if List.isEmpty d.opt
                    then ""
                    else (List.map optionalToString d.opt)
                        |> String.join " "
                        |> (\x -> ">> " ++ x)
    in
        [etypeStr, descStr, optValStr]
            |> String.join " "

            |> String.trim
            
            |> Encode.string


encodeMeta : Meta -> Encode.Value
encodeMeta m = 
    Encode.object
        [ ("id", Encode.int m.id)
        , ("done", Encode.bool m.done)
        , ("editing", Encode.bool m.editing)
        , ("parent", Encode.int m.parent)
        , ("children", List.map Encode.int m.children |> Encode.list)
        ]


encodeEntry : Entry -> Encode.Value
encodeEntry e = 
    Encode.object
        [ ("meta", encodeMeta e.meta)
        , ("str", encodeDetails e.details)
        ]
    


encodeModel : Collection -> Encode.Value
encodeModel m = 
    let
        data = Dict.toList m.data
                |> List.map (Tuple.second >> encodeEntry)
                |> Encode.list

        focus = Encode.int m.focus

        root = Encode.int m.root
                        

    in
        Encode.object
        [ ("focus", focus)
        , ("root", root)
        , ("data", data)
        ]





decodeMeta : Decode.Decoder Meta
decodeMeta = 
    Decode.map5 Meta
        (Decode.field "id" Decode.int)
        (Decode.field "done" Decode.bool)
        (Decode.field "editing" Decode.bool)
        (Decode.field "parent" Decode.int)
        (Decode.field "children" (Decode.list Decode.int))


type alias JsonListEntry = {meta : Meta, str : String}

decodeJsonEntry : Decode.Decoder JsonListEntry
decodeJsonEntry = Decode.map2 JsonListEntry
                    (Decode.field "meta" decodeMeta)
                    (Decode.field "str" Decode.string)



decodeModel : Encode.Value -> Collection
decodeModel xs = 
    let
        decoder = Decode.field "data" (Decode.list decodeJsonEntry)
        parsedList =  Result.withDefault []
                        <| Decode.decodeValue decoder xs

        defDesc = { etype = Empty, desc = "", opt = [] }

        constructor : JsonListEntry -> Entry
        constructor e = 
            let
                result = parseDescription e.str
            in
            case result of
                Just details -> {meta = e.meta, details = details}
                Nothing -> {meta = e.meta, details = defDesc}
                -- _ -> Empty e.meta False

        constructed = List.map constructor parsedList
                        |> List.map (\x -> (x.meta.id, x) )
                        
        data = Dict.fromList constructed

        root = Result.withDefault 0
                        <| Decode.decodeValue (Decode.field "root" Decode.int) xs

        focus = Result.withDefault 0 
                        <| Decode.decodeValue (Decode.field "focus" Decode.int) xs

    in
    { root = root, data = data, focus=focus }

