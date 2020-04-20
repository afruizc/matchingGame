module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, h2, img, text)
import Html.Attributes exposing (class, src, style, width)
import Html.Events exposing (onClick)



---- MODEL ----


type alias Id =
    Int


type alias Category =
    { name : String
    , id : Id
    }


type alias Points =
    Int


type alias Item =
    { belongsTo : Category
    , name : String
    , id : Id
    }


type alias PlayingGameState =
    { items : List Item
    , score : Points
    , selectedItemId : Maybe Int
    }


initialItems =
    [ { belongsTo = greenCategory, name = "/images/g_frog.jpg", id = 0 }
    , { belongsTo = greenCategory, name = "/images/g_parrot.jpg", id = 1 }
    , { belongsTo = greenCategory, name = "/images/g_slug.jpg", id = 2 }
    , { belongsTo = greenCategory, name = "/images/g_turtle.jpg", id = 3 }
    , { belongsTo = orangeCategory, name = "/images/o_clownfish.jpg", id = 4 }
    , { belongsTo = orangeCategory, name = "/images/o_tiger.jpg", id = 5 }
    , { belongsTo = orangeCategory, name = "/images/o_tucan.jpg", id = 6 }
    , { belongsTo = redCategory, name = "/images/r_butterfly.jpg", id = 7 }
    , { belongsTo = redCategory, name = "/images/r_cardinal.jpg", id = 8 }
    , { belongsTo = redCategory, name = "/images/r_crab.jpg", id = 9 }
    , { belongsTo = redCategory, name = "/images/r_ladyBug.jpg", id = 10 }
    , { belongsTo = yellowCategory, name = "/images/y_bettle.jpg", id = 11 }
    , { belongsTo = yellowCategory, name = "/images/y_chicks.jpg", id = 12 }
    , { belongsTo = yellowCategory, name = "/images/y_seahorse.jpg", id = 13 }
    , { belongsTo = yellowCategory, name = "/images/y_snake.jpg", id = 14 }
    ]


noCategory =
    { name = "", id = -1 }


noItem =
    { belongsTo = noCategory, name = "", id = -1 }


orangeCategory =
    { name = "orange", id = 0 }


redCategory =
    { name = "red", id = 1 }


yellowCategory =
    { name = "yellow", id = 2 }


greenCategory =
    { name = "green", id = 3 }


blueCategory =
    { name = "blue", id = 5 }


initialGameState =
    PlayingGameState initialItems 0 Nothing


categories =
    [ orangeCategory
    , redCategory
    , yellowCategory
    , greenCategory
    ]


type GameState
    = Playing PlayingGameState
    | GameWon Points
    | GameLost Points


type alias Model =
    { colors : List Category
    , gameState : GameState
    }


init : ( Model, Cmd Msg )
init =
    ( { colors = categories, gameState = Playing initialGameState }, Cmd.none )



---- UPDATE ----


type Msg
    = PlayItem Id
    | PlayCategory Id


makeItemPlay : Id -> PlayingGameState -> GameState
makeItemPlay id state =
    Playing { state | selectedItemId = Just id }


makeCategoryPlay : Id -> PlayingGameState -> GameState
makeCategoryPlay id state =
    case state.selectedItemId of
        Just x ->
            let
                selectedItem : Item
                selectedItem =
                    List.filter (\item -> item.id == x) state.items
                        |> Debug.log "selectedItem"
                        |> List.head
                        |> Maybe.withDefault noItem

                correctCategory =
                    selectedItem.belongsTo.id == id

                newScore =
                    if correctCategory then
                        state.score + 1

                    else
                        state.score - 1

                itemsLeft =
                    if correctCategory then
                        List.filter (\item -> item.id /= x) state.items

                    else
                        state.items
            in
            case itemsLeft of
                [] ->
                    if newScore > 0 then
                        GameWon newScore

                    else
                        GameLost newScore

                _ ->
                    Playing
                        { state
                            | selectedItemId = Nothing
                            , score = newScore
                            , items = itemsLeft
                        }

        Nothing ->
            Playing state


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        processPlaying state =
            case msg of
                PlayItem id ->
                    makeItemPlay id state

                PlayCategory id ->
                    makeCategoryPlay id state

        newState =
            case model.gameState of
                Playing state ->
                    processPlaying state

                _ ->
                    model.gameState

        newModel =
            { model
                | gameState = newState
            }
    in
    ( newModel, Cmd.none )



---- VIEW ----


buttonCSS color =
    [ style "background-color" color
    , style "font-family" "CopperHewitt"
    , style "font-size" "1.2rem"
    , style "color" "white"
    , style "text-transform" "uppercase"
    , style "text-shadow" "-1px 0 black, 0 1px black, 1px 0 black, 0 -1px black"
    , class "col"
    , style "min-height" "150px"
    , class "d-flex align-items-center justify-content-center"
    ]


view : Model -> Html Msg
view model =
    renderModel model


getPoints gameState =
    let
        intPoints =
            case gameState of
                Playing state ->
                    state.score

                GameWon points ->
                    points

                GameLost points ->
                    points
    in
    String.fromInt intPoints


renderModel : Model -> Html Msg
renderModel model =
    let
        drawCategory : Category -> Html Msg
        drawCategory cat =
            div (onClick (PlayCategory cat.id) :: buttonCSS cat.name)
                [ h2 [] [ text cat.name ] ]

        points =
            getPoints model.gameState

        renderPoints =
            case model.gameState of
                Playing _ ->
                    div [] [ h1 [] [ text <| points ++ " point(s)." ] ]

                GameWon _ ->
                    div [] [ h1 [] [ text <| "Game won! " ++ points ++ " point(s)." ] ]

                GameLost _ ->
                    div [] [ h1 [] [ text <| "Game lost! " ++ points ++ " point(s)." ] ]

        categoriesView =
            div [ class "row" ] <|
                List.map
                    drawCategory
                    model.colors

        renderItems items =
            let
                renderItem item =
                    img
                        [ style "height" "150px"
                        , onClick <| PlayItem item.id
                        , src item.name
                        , class "col"
                        ]
                        [ text item.name ]
            in
            List.map renderItem items

        itemsView =
            let
                itemList =
                    case model.gameState of
                        Playing state ->
                            renderItems state.items

                        _ ->
                            []
            in
            div [ class "row" ] itemList
    in
    div
        [ style "width" "100%"
        ]
        [ categoriesView
        , itemsView
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
