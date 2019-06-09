module Main exposing (Arrow, ArrowDirection(..), Item, ItemName(..), Model, Msg(..), Scene, SceneId(..), init, main, update, view)

import Browser
import Html exposing (Html, aside, button, div, h1, h2, img, li, text, ul)
import Html.Attributes exposing (class, classList, src, style)
import Html.Events exposing (onClick)



---- MODEL ----
-- global state needs to contain: active scene, a list of items, a list of key items, and a list of equipped items.


type Secret
    = CategoryTheory


type alias Model =
    { activeSceneId : SceneId
    , inventory : List Item
    , equipment : List Item
    , knowledge : List Secret
    , openDialog : Maybe (Html Msg)
    }


type alias AssetPath =
    Maybe String



-- SCENES


type SceneId
    = StartMenu
    | SittingRoom
    | Temple
    | PhoneBooth
    | Storefronts
    | Spire
    | ToolsForThought


type alias Scene =
    { id : SceneId
    , assetPath : AssetPath
    , navigationOptions : List Arrow
    , onClick : Maybe Msg
    }


scenes : Model -> List Scene
scenes model =
    [ { id = StartMenu, navigationOptions = [ ( North, SittingRoom ) ], assetPath = Nothing, onClick = Nothing }
    , { id = SittingRoom
      , navigationOptions = [ ( East, Temple ), ( West, PhoneBooth ) ]
      , assetPath = Just "SittingRoom"
      , onClick = Nothing
      }
    , { id = Temple
      , navigationOptions =
            [ ( West, SittingRoom )
            ]
                ++ maybeArrow ( North, Storefronts ) (List.any (\item -> item.name == GateKey) model.inventory)
      , assetPath = Just "Temple"
      , onClick = Nothing
      }
    , { id = PhoneBooth
      , navigationOptions = [ ( East, SittingRoom ) ]
      , assetPath = Just "PhoneBooth"
      , onClick = Just (AcquireKeyItem GateKey)
      }
    , { id = Storefronts
      , navigationOptions = [ ( South, Temple ), ( North, Spire ) ] ++ maybeArrow ( West, ToolsForThought ) (List.any (\item -> item.name == Sunglasses) model.equipment)
      , assetPath = Just "Storefronts"
      , onClick = Nothing
      }
    , { id = Spire
      , navigationOptions = [ ( South, Storefronts ) ]
      , assetPath = Just "Spire"
      , onClick = Just (AcquireEquipment Sunglasses)
      }
    , { id = ToolsForThought
      , navigationOptions = [ ( East, Storefronts ) ]
      , assetPath = Just "ToolsForThought"
      , onClick = Just (AcquireKnowledge CategoryTheory)
      }
    ]


getSceneById : SceneId -> Model -> Maybe Scene
getSceneById sceneId model =
    List.head (List.filter (\scene -> scene.id == sceneId) (scenes model))



-- ARROWS


type alias Arrow =
    ( ArrowDirection, SceneId )


type ArrowDirection
    = North
    | NorthEast
    | East
    | SouthEast
    | South
    | SouthWest
    | West
    | NorthWest


maybeArrow : Arrow -> Bool -> List Arrow
maybeArrow arrow condition =
    if condition == True then
        [ arrow ]

    else
        []


init : ( Model, Cmd Msg )
init =
    ( { activeSceneId = StartMenu, inventory = [], equipment = [], openDialog = Nothing, knowledge = [] }, Cmd.none )



-- ITEMS


type ItemType
    = Key
    | Equipment


allItems : List Item
allItems =
    [ { name = GateKey
      , itemType = Key
      , stringName = "Gate Key"
      , description = Nothing
      }
    , { name = Sunglasses
      , itemType = Equipment
      , stringName = "Sunglasses"
      , description = Just "These glasses will help you see things you might have missed."
      }
    ]



-- returns a list with just the item we are looking for or an empty list.


getItemByName : ItemName -> List Item -> List Item
getItemByName itemName items =
    List.filter (\item -> item.name == itemName) items


type ItemName
    = GateKey
    | Sunglasses


type alias Item =
    { name : ItemName
    , itemType : ItemType
    , stringName : String
    , description : Maybe String
    }



-- DIALOGS


renderDialog : List (Html Msg) -> Html Msg
renderDialog children =
    div [ class "dialog", onClick HideDialog ] children


renderItemDescription : Item -> List (Html Msg)
renderItemDescription item =
    case item.description of
        Nothing ->
            []

        Just description ->
            [ text description ]


acquiredKeyItemDialog : Maybe Item -> Maybe (Html Msg)
acquiredKeyItemDialog maybeItem =
    case maybeItem of
        Nothing ->
            Nothing

        Just item ->
            Just
                (renderDialog
                    ([ text ("Congratulations! You've acquired the " ++ item.stringName ++ ". This is a key item. Key items will be automatically used when needed and don't need to be equipped.")
                     ]
                        ++ renderItemDescription item
                    )
                )


acquiredEquipmentDialog : Maybe Item -> Maybe (Html Msg)
acquiredEquipmentDialog maybeItem =
    case maybeItem of
        Nothing ->
            Nothing

        Just item ->
            Just
                (renderDialog
                    ([ text ("Congratulations! You've acquired the " ++ item.stringName ++ ". This is a piece of equipment. You can only equip one item at a time.") ] ++ renderItemDescription item)
                )


secretsDialog : Html Msg
secretsDialog =
    renderDialog [ text "This is a secret. Secrets will help you make more sense of the world and unlock hidden information." ]



---- UPDATE ----


type Msg
    = ChangeScene SceneId
    | AcquireKeyItem ItemName
    | AcquireEquipment ItemName
    | AcquireKnowledge Secret
    | ChangeEquipStatus ItemName
    | HideDialog


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeScene sceneId ->
            ( { model | activeSceneId = sceneId }, Cmd.none )

        -- Should open dialog saying you've acquired a key item
        AcquireKeyItem keyItemName ->
            let
                item =
                    getItemByName keyItemName allItems

                alreadyAquired =
                    List.any (\inventoryItem -> inventoryItem.name == keyItemName) model.inventory
            in
            if alreadyAquired then
                ( model, Cmd.none )

            else
                ( { model
                    | inventory = model.inventory ++ item
                    , openDialog = acquiredKeyItemDialog (List.head item)
                  }
                , Cmd.none
                )

        AcquireEquipment equipmentName ->
            let
                item =
                    getItemByName equipmentName allItems

                alreadyAquired =
                    List.any (\inventoryItem -> inventoryItem.name == equipmentName) model.inventory
            in
            if alreadyAquired then
                ( model, Cmd.none )

            else
                ( { model
                    | inventory = model.inventory ++ item
                    , openDialog = acquiredEquipmentDialog (List.head item)
                  }
                , Cmd.none
                )

        AcquireKnowledge secret ->
            let
                alreadyAquired =
                    List.any (\knowledge -> knowledge == secret) model.knowledge
            in
            if alreadyAquired then
                ( model, Cmd.none )

            else
                ( { model
                    | knowledge = model.knowledge ++ [ secret ]
                    , openDialog = Just secretsDialog
                  }
                , Cmd.none
                )

        HideDialog ->
            ( { model | openDialog = Nothing }, Cmd.none )
        ChangeEquipStatus equipmentName ->
            let
                currentlyEquipped =
                    List.any (\equippedItem -> equippedItem.name == equipmentName) model.equipment
            in
            if currentlyEquipped then
                ( { model | equipment = List.filter (\equippedItem -> equippedItem.name /= equipmentName) model.equipment }, Cmd.none )

            else
                ( { model | equipment = model.equipment ++ getItemByName equipmentName allItems }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        maybeScene =
            getSceneById model.activeSceneId model

        dialog =
            case model.openDialog of
                Nothing ->
                    div [] []

                Just dialogHtml ->
                    dialogHtml
    in
    div [ classList [ ( "grayscale", noGlassesEquipped model ) ] ]
        [ renderScene maybeScene
        , renderGui model
        , dialog
        ]


noGlassesEquipped : Model -> Bool
noGlassesEquipped model =
    case List.head (getItemByName Sunglasses model.equipment) of
        Nothing ->
            True

        Just _ ->
            False



-- TODO: Render inventory and equipment


renderGui : Model -> Html Msg
renderGui model =
    let
        arrows =
            case getSceneById model.activeSceneId model of
                Nothing ->
                    div [] []

                Just scene ->
                    div [] (List.map renderArrow scene.navigationOptions ++ renderInventory model)
    in
    arrows


renderInventory : Model -> List (Html Msg)
renderInventory model =
    [ aside [ class "inventory" ]
        [ div [] ([ h2 [] [ text "Inventory" ] ] ++ renderItemList model.inventory model.equipment)
        ]
    ]


renderItem : Item -> List Item -> Html Msg
renderItem item equippedItems =
    let
        isEquipped =
            List.any (\equippedItem -> equippedItem.name == item.name) equippedItems

        itemText =
            if isEquipped then
                item.stringName ++ " (equipped)"

            else
                item.stringName

        itemAttrs =
            if item.itemType == Equipment then
                [ onClick (ChangeEquipStatus item.name) ]

            else
                []
    in
    li itemAttrs [ text itemText ]


renderItemList : List Item -> List Item -> List (Html Msg)
renderItemList items equippedItems =
    [ ul [] (List.map (\item -> renderItem item equippedItems) items) ]


renderArrow : Arrow -> Html Msg
renderArrow arrow =
    let
        ( className, value ) =
            case Tuple.first arrow of
                North ->
                    ( "north", [ text "^" ] )

                NorthEast ->
                    ( "north-east", [ text "^>" ] )

                East ->
                    ( "east", [ text ">" ] )

                SouthEast ->
                    ( "south-east", [ text "v>" ] )

                South ->
                    ( "south", [ text "v" ] )

                SouthWest ->
                    ( "south-west", [ text "<v" ] )

                West ->
                    ( "west", [ text "<" ] )

                NorthWest ->
                    ( "north-west", [ text "<^" ] )

        container =
            button [ classList [ ( "arrow", True ), ( className, True ) ], onClick (ChangeScene (Tuple.second arrow)) ]
    in
    container value


renderScene : Maybe Scene -> Html Msg
renderScene maybeScene =
    case maybeScene of
        Nothing ->
            h1 [] [ text "Something went wrong - there's no scene." ]

        Just sceneData ->
            div (withClick sceneData [ class "scene", calculateImagePath sceneData.assetPath ]) []


withClick : Scene -> List (Html.Attribute Msg) -> List (Html.Attribute Msg)
withClick scene attrs =
    case scene.onClick of
        Nothing ->
            attrs

        Just clickHandler ->
            attrs ++ [ onClick clickHandler ]


calculateImagePath : AssetPath -> Html.Attribute Msg
calculateImagePath assetPath =
    case assetPath of
        Nothing ->
            style "background-image" "none"

        Just imagePath ->
            style "background-image" ("url(" ++ imagePath ++ ".jpg)")



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
