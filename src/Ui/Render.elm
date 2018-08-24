module Ui.Render exposing (..)

import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)

import Markdown

import Tasks.Collection.Model exposing (..)
import Tasks.Entry.Model exposing (..)
import Tasks.Entry.Actions exposing (..)

import Update exposing (..)
import Ui.Elements exposing (..)

import Debug exposing (..)


textOf : EntryType -> String
textOf etype = 
    let text = case etype of
                    Checkbox -> "[]"
                    Bullet -> "*"
                    (Numbered num) -> num ++ ". "
                    (Heading num) -> String.repeat num "#"
                    LineBreak -> ""
                    Empty -> ""
                    Text -> ""
    in
    text ++ " "


renderEditEntry : Entry -> Html Msg
renderEditEntry e = div [ id (toString e.meta.id)
                        , class "edit"
                        , contenteditable True
                        ] 
                        [ span [] 
                            [text <| 
                                String.append (textOf e.details.etype) e.details.desc ]
                        ]


renderEmpty meta props = 
    let
        node = if props
               then label [] []
               else label [] []
    in
    div [  id (toString meta.id)
        , class "display empty level level-left level-item is-marginless"
        , contenteditable False 
        -- , E.onClick (EditEntry meta.id) 
        ]
        [ node ]


renderLine meta details = 
    let
        
        viewFunc = case details.etype of
                Checkbox -> checkbox
                Bullet -> bullet
                (Numbered num) -> numbered num
                (Heading num) -> heading num
                Empty -> blank
                LineBreak -> blank
                Text -> blank

        icon_attrs = [ checked meta.done ]
        icon_contents = []
        desc_attrs = [ class "description"]
        desc_contents = [Markdown.toHtml [] (String.trim details.desc)]
    in
    div [ id (toString meta.id)
        , class "display line level level-left level-item is-marginless"
        , contenteditable False
        -- , E.onClick (EditEntry meta.id) 
        ]

        (viewFunc icon_attrs icon_contents desc_attrs desc_contents)
        



renderDisplayMode : Entry -> Html Msg
renderDisplayMode e = renderLine e.meta e.details



-- renderChildren : Entry -> data -> List (Html Msg)
renderChildren e data = 
    let
        f id = case Dict.get id data of
                        (Just e) -> div [class "level level-left level-item is-padingless is-marginless"] 
                                        [renderEntry e data]
                        Nothing -> span [] []

        childrenIds = getChildrenId e

        childrenEntry = List.map f childrenIds
    in
    childrenEntry
    -- if List.length childrenEntry >0 then
    --     div [class "child-level-wrapper drop-hook child-hook"]
    --         childrenEntry
    -- else
    --     div [class "drop-hook order-hook"] []
            
   

renderEntry : Entry -> Data -> Html Msg
renderEntry e data = 
    let
        renderFunction editmode = if editmode then renderEditEntry else renderDisplayMode
    
        renderedEntry = renderFunction e.meta.editing e

        renderedChildrenList = renderChildren e data

        renderedChildrenNode = div [class "child-level-wrapper"]
                                   (div [class "drop-hook child-hook"] [] :: renderedChildrenList)

    in
        if List.length renderedChildrenList > 0 then
            div [class "entry section content is-paddingless is-marginless"]
                [ renderedEntry
                , renderedChildrenNode
                , div [class "drop-hook order-hook"] []
                ]
        else
            div [class "entry section content is-paddingless is-marginless"]
                [ renderedEntry
                , div [class "drop-hook order-hook"] []
                ]
            -- , div [class "level level-left level-item"]
                --   [ div [class "child-level-wrapper"] (renderChildren e data) ]
            

