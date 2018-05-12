module Main exposing (..)

import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, style, value)
import Html.Events exposing (onClick, onInput)
import Time


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
    , width : Int
    , height : Int
    , cells : List Int
    , tick : Int
    }



-- INITIAL FIELD


initialAliveCells : List ( Int, Int )
initialAliveCells =
    [ ( 1, 1 ), ( 2, 2 ), ( 1, 2 ) ]


init : ( Model, Cmd Msg )
init =
    let
        worldWidth =
            5

        worldHeight =
            5
    in
    ( Model Setup worldWidth worldHeight (cellsFieldFromList worldWidth worldHeight initialAliveCells) 0, Cmd.none )


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
                , cells = evolve model.width model.height model.cells
              }
            , Cmd.none
            )

        SetFieldWidth width ->
            ( { model
                | width = width
                , cells = cellsFieldFromList width model.height initialAliveCells
              }
            , Cmd.none
            )

        SetFieldHeight height ->
            ( { model
                | height = height
                , cells = cellsFieldFromList model.width height initialAliveCells
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.mode of
        Simulation ->
            Time.every Time.second Tick

        Setup ->
            Sub.none



-- VIEW


cellWidthPx : Int
cellWidthPx =
    25


cellHeightPx : Int
cellHeightPx =
    25


viewCell : Int -> Html Msg
viewCell n =
    let
        color =
            case n of
                1 ->
                    "green"

                _ ->
                    "grey"

        greenBox =
            style
                [ ( "backgroundColor", color )
                , ( "width", toString cellWidthPx ++ "px" )
                , ( "height", toString cellHeightPx ++ "px" )
                ]
    in
    Html.div [ greenBox ] []


transformIntMsgToStringMsg : (Int -> Msg) -> (String -> Msg)
transformIntMsgToStringMsg intMsg =
    \value -> intMsg (Result.withDefault 0 (String.toInt value))


stylesheet : Html Msg
stylesheet =
    Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "style.css" ] []


view : Model -> Html Msg
view model =
    Html.div []
        [ stylesheet
        , div [ class "world-size-setup" ]
            [ input
                [ class "world-width"
                , value (toString model.width)
                , onInput (transformIntMsgToStringMsg SetFieldWidth)
                ]
                []
            , input
                [ class "world-heigth"
                , value (toString model.height)
                , onInput (transformIntMsgToStringMsg SetFieldHeight)
                ]
                []
            ]
        , div
            [ class "field-setup"
            , style
                [ ( "display", "flex" )
                , ( "flex-wrap", "wrap" )
                , ( "width", toString (cellWidthPx * model.width) ++ "px" )
                ]
            ]
            (List.map viewCell model.cells)
        , div [ class "controls" ]
            [ button [ onClick StartSimulation ] [ text "Start Simulation" ]
            ]
        ]
