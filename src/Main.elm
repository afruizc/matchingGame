module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, h2, img, text)
import Html.Attributes exposing (class, src, style)
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
    [ { belongsTo = purpleCategory, name = "/images/p_salamander.jpg", id = 15 }
    , { belongsTo = greenCategory, name = "/images/g_slug.jpg", id = 2 }
    , { belongsTo = blueCategory, name = "/images/b_whale.jpg", id = 12 }
    , { belongsTo = redCategory, name = "/images/r_cardinal.jpg", id = 4 }
    , { belongsTo = orangeCategory, name = "/images/o_clownfish.jpg", id = 5 }
    , { belongsTo = yellowCategory, name = "/images/y_chicks.jpg", id = 6 }
    , { belongsTo = redCategory, name = "/images/r_ladyBug.jpg", id = 7 }
    , { belongsTo = yellowCategory, name = "/images/y_bettle.jpg", id = 8 }
    , { belongsTo = greenCategory, name = "/images/g_turtle.jpg", id = 3 }
    , { belongsTo = blueCategory, name = "/images/b_jelly.jpg", id = 13 }
    , { belongsTo = purpleCategory, name = "/images/p_bat.jpg", id = 14 }
    , { belongsTo = greenCategory, name = "/images/g_frog.jpg", id = 0 }
    , { belongsTo = orangeCategory, name = "/images/o_tucan.jpg", id = 9 }
    , { belongsTo = redCategory, name = "/images/r_butterfly.jpg", id = 10 }
    , { belongsTo = orangeCategory, name = "/images/o_tiger.jpg", id = 11 }
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
    { name = "blue", id = 4 }


purpleCategory =
    { name = "purple", id = 5 }


initialGameState =
    PlayingGameState initialItems 0 Nothing


categories =
    [ yellowCategory
    , orangeCategory
    , redCategory
    , greenCategory
    , blueCategory
    , purpleCategory
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
    , style "cursor" "pointer"
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

        renderItems items selectedItemId =
            let
                renderItem item =
                    div
                        [ class "col-2"
                        , class "p-0"
                        , style "max-width" "100%"
                        , style "height" "auto"
                        , style "cursor" "pointer"
                        , onClick <| PlayItem item.id
                        ]
                        [ img
                            [ --, style "-webkit-transform" "rotate(-6deg)"
                              --, style "-o-transform" "rotate(-6deg)"
                              --, style "-moz-transform" "rotate(-6deg)"
                              --, style "transform" "rotate(-6deg)"
                              src ("%PUBLIC_URL%" ++ item.name)
                            , style "margin" "1rem"
                            , style "box-shadow" "5px 5px 7px rgba(33,33,33,.7)"
                            , class "img-fluid"
                            , class "img-thumbnail"
                            , style "transform"
                                (if item.id == selectedItemId then
                                    "scale(1.25)"

                                 else
                                    "scale(1.0)"
                                )
                            ]
                            [ text item.name ]
                        ]
            in
            List.map renderItem items

        itemsView =
            let
                itemList =
                    case model.gameState of
                        Playing state ->
                            renderItems state.items <| Maybe.withDefault -1 state.selectedItemId

                        _ ->
                            []
            in
            div [ class "row" ] itemList
    in
    div
        [ style "width" "100%"
        , class "mb-4"
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
