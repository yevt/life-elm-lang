module Main exposing (..)

import Html
import Html.Attributes
import Html.Events
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


initialField : List Int
initialField =
    [ 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 1
    , 1
    , 1
    , 0
    , 0
    , 1
    , 0
    , 0
    , 0
    , 0
    , 0
    , 1
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    ]


init : ( Model, Cmd Msg )
init =
    ( Model Setup 5 5 initialField 0, Cmd.none )


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
    | SetFieldWidth String
    | SetFieldHeight String


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

        SetFieldWidth value ->
            let
                width =
                    Result.withDefault 0 (String.toInt value)
            in
            ( { model
                | width = width
                , cells = List.repeat (width * model.height) 0
              }
            , Cmd.none
            )

        SetFieldHeight value ->
            let
                height =
                    Result.withDefault 0 (String.toInt value)
            in
            ( { model
                | height = height
                , cells = List.repeat (height * model.width) 0
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


viewCell : Int -> Html.Html Msg
viewCell n =
    let
        color =
            case n of
                1 ->
                    "green"

                _ ->
                    "grey"

        style =
            Html.Attributes.style
                [ ( "backgroundColor", color )
                , ( "width", toString cellWidthPx ++ "px" )
                , ( "height", toString cellHeightPx ++ "px" )
                ]
    in
    Html.div [ style ] []


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.input
            [ Html.Attributes.value (toString model.width)
            , Html.Events.onInput SetFieldWidth
            ]
            []
        , Html.input
            [ Html.Attributes.value (toString model.height)
            , Html.Events.onInput SetFieldHeight
            ]
            []
        , Html.div
            [ Html.Attributes.style
                [ ( "display", "flex" )
                , ( "flex-wrap", "wrap" )
                , ( "width", toString (cellWidthPx * model.width) ++ "px" )
                ]
            ]
            (List.map viewCell model.cells)
        , Html.button [ Html.Events.onClick StartSimulation ] [ Html.text "Start Simulation" ]
        ]
