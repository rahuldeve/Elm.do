module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Dict exposing (..)

import Tasks.Collection.Model exposing (..)
import Tasks.Entry.Model exposing (..)
import Tasks.Entry.Actions exposing (..)
-- import Tasks.Collection.Actions exposing (..)
import Update exposing (..)
import Ui.Render exposing (..)
    




zoomcrumbs : Collection -> Html Msg
zoomcrumbs collection = 
    let
        getParentId id = case Dict.get id collection.data of
                            (Just e) -> (getMeta e).parent
                            _         -> id
        
        getCrumbs focusid = if focusid == collection.root
                            then [Dict.get focusid collection.data]
                            else (Dict.get focusid collection.data) :: (getCrumbs <| getParentId focusid)

        getIdandText : Maybe Entry -> (Id, String)
        getIdandText entry = 
            let
                getDesc entry = Maybe.map getDetails entry
                                    |> Maybe.map (\details -> details.desc)
                                    |> Maybe.withDefault ""

                getId entry = Maybe.map getMeta entry
                                |> Maybe.map (\meta -> meta.id)
                                |> Maybe.withDefault -1

            in
                (getId entry, getDesc entry)

        crumbs = getCrumbs collection.focus
                    |> List.drop 1
                    |> List.reverse
                    |> List.map getIdandText
    in
        div [class "zoom-crumb content"] 
        (List.map (\(id,crumb) -> a [href (id |> toString)] [text crumb]) crumbs)



footer : Html Msg
footer = 
    div [ id "footer", class "level"]
        [ div [class "level-item"] 
              [ button []  [text "left"]
              , button [] [text "right"] 
              ]
        ]



page contents = 
    section [class "content-wrapper section content is-marginless"]
            contents

-- burgerNav : Collection -> Html Msg
-- burgerNav collection = 
--     div [class ""]
        
burgerMenu : Collection -> Html Msg
burgerMenu collection = 
    div [id "menu", class "burger-panel section"]
        [header [] [text "menu"]]


view : Collection -> Html Msg
view collection =
    let
        f id = case Dict.get id collection.data of
                    (Just e) -> renderEntry e collection.data
                    Nothing -> span [] []
    in
    div [class "outermost-wrapper"] 
        [ 
        burgerMenu collection,
        --   page (List.map f <| collection.rootOrder)
        div [id "panel"]
            [ page [zoomcrumbs collection
                   , (f collection.focus)
                   ]
            , footer
            ]
        ]
