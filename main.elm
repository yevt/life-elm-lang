module Main exposing (..)

import Html exposing (Html, button, div, input, text, label)
import Html.Attributes exposing (class, style, value)
import Html.Events exposing (onClick, onInput)
import Time
import Ports exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type Mode
    = Setup
    | Simulation


type alias Model =
    { mode : Mode
    , worldWidth : Int
    , worldHeight : Int
    , cells : List Int
    , tick : Int -- iteration counter
    , tickDuration: Float -- ms
    , screenWidth: Int
    , screenHeight: Int
    }



-- INITIAL FIELD


initialAliveCells : List ( Int, Int )
initialAliveCells =
    [ ( 1, 1 ), ( 2, 2 ), ( 1, 2 ) ]


init : ( Model, Cmd Msg )
init =
    let
        worldWidth =
            20

        worldHeight =
            20
    in
        (
            { mode = Setup
            , worldWidth = worldWidth -- cells
            , worldHeight = worldHeight
            , cells = cellsFieldFromList worldWidth worldHeight initialAliveCells
            , tick = 0 -- iteration counter
            , tickDuration = 1000 -- ms
            , screenWidth = 500 -- px
            , screenHeight = 50
            , cellWidth = 25
            , cellHeight =
            }
            , Cmd.none
        )


cellsFieldFromList : Int -> Int -> List ( Int, Int ) -> List Int
cellsFieldFromList w h coordsList =
    let
        cells =
            List.repeat (w * h) 0

        createCell index _ =
            let
                x =
                    index % w

                y =
                    index // w

                inCoordList ( xx, yy ) =
                    xx == x && yy == y
            in
            if List.any inCoordList coordsList then
                1
            else
                0
    in
    List.indexedMap createCell cells



-- UPDATE


type Msg
    = StartSimulation
    | Tick Time.Time
    | SetFieldWidth Int
    | SetFieldHeight Int
    | SetTickDuration Float
    | ToggleCell Int
    | ScreenSize (Int, Int)


evolve : Int -> Int -> List Int -> List Int
evolve width height cells =
    let
        indexedCells =
            List.indexedMap
                (\index cell ->
                    { x = index % width
                    , y = index // width
                    , content = cell
                    }
                )
                cells

        isAdjacent target other =
            let
                xDiff =
                    abs (target.x - other.x)

                yDiff =
                    abs (target.y - other.y)

                xMax =
                    width - 1

                yMax =
                    height - 1
            in
            (xDiff <= 1 || xDiff == xMax) && (yDiff <= 1 || yDiff == yMax) && not (xDiff == 0 && yDiff == 0)

        cellsWithAdjacent =
            List.map
                (\target ->
                    let
                        adjacentCells =
                            List.filter (\other -> isAdjacent target other) indexedCells

                        adjacentCount =
                            List.map (\cell -> cell.content) adjacentCells |> List.sum
                    in
                    { x = target.x
                    , y = target.y
                    , content = target.content
                    , adjacentCount = adjacentCount
                    , adjacentCells = adjacentCells
                    }
                )
                indexedCells
    in
    List.map
        (\target ->
            if target.adjacentCount == 3 then
                1
            else if target.adjacentCount == 2 then
                target.content
            else
                0
        )
        cellsWithAdjacent


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartSimulation ->
            ( { model | mode = Simulation }, Cmd.none )

        Tick time ->
            ( { model
                | tick = model.tick + 1
                , cells = evolve model.worldWidth model.worldHeight model.cells
              }
            , Cmd.none
            )

        SetFieldWidth width ->
            ( { model
                | worldWidth = width
                , cells = cellsFieldFromList width model.worldHeight initialAliveCells
              }
            , Cmd.none
            )

        SetFieldHeight height ->
            ( { model
                | worldHeight = height
                , cells = cellsFieldFromList model.worldWidth height initialAliveCells
              }
            , Cmd.none
            )

        ToggleCell index ->
            ( { model | cells = List.indexedMap (\i v -> 
                if i == index then
                    if v == 1 then 0 else 1
                else
                    v
              ) model.cells }
            , Cmd.none
            )

        SetTickDuration duration ->
            ( { model | tickDuration = duration}
            , Cmd.none
            )        

        ScreenSize (w, h) -> ( { model 
                | screenWidth = w
                , screenHeight = h
            }
        , Cmd.none 
        )




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [
        case model.mode of
            Simulation ->
                Time.every model.tickDuration Tick

            Setup ->
                Sub.none

        , screenSize ScreenSize
    ]



-- VIEW

viewCell : Int -> Int -> Html Msg
viewCell index value =
    let
        color =
            case value of
                1 ->
                    "green"

                _ ->
                    "grey"
            
    in
    Html.div [ 
        style
            [ ( "backgroundColor", color )
            , ( "width", toString model.cellWidth ++ "px" )
            , ( "height", toString model.cellHeight ++ "px" )
            ]
        , onClick ( ToggleCell index )
    ] []


transformIntMsgToStringMsg : (Int -> Msg) -> (String -> Msg)
transformIntMsgToStringMsg intMsg =
    \value -> intMsg (Result.withDefault 0 (String.toInt value))

transformFloatMsgToStringMsg : (Float -> Msg) -> (String -> Msg)
transformFloatMsgToStringMsg floatMsg =
    \string -> floatMsg (Result.withDefault 0 (String.toFloat string))



stylesheet : Html Msg
stylesheet =
    Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "style.css" ] []


view : Model -> Html Msg
view model =
    Html.div []
        [ stylesheet
        , div [ class "world-size-setup" ]
            [ 
            label [] [text "World Width (cells): "] 
            , input
                [ class "world-width"
                , value (toString model.worldWidth)
                , onInput (transformIntMsgToStringMsg SetFieldWidth)
                ]
                []
            , label [] [text "World Heigth (px): "] 
            , input
                [ class "world-heigth"
                , value (toString model.worldHeight)
                , onInput (transformIntMsgToStringMsg SetFieldHeight)
                ]
                []
            , label [] [text "Tick duration (ms): "] 
            , input
                [ class "tick-duration"
                , value (toString model.tickDuration)
                , onInput (transformFloatMsgToStringMsg SetTickDuration)
                ]
                []
            , div [ class "controls" ]
                [ button [ onClick StartSimulation ] [ text "Start Simulation" ]
                ]
            ]
        , div
            [ class "field-setup"
            , style
                [ ( "display", "flex" )
                , ( "flex-wrap", "wrap" )
                , ( "width", toString (model.cellWidth * model.cellHeight) ++ "px" )
                ]
            ]
            (List.indexedMap viewCell model.cells)
        ]
