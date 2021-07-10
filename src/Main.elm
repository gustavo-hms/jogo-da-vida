module Main exposing (..)

import Browser
import Element exposing (centerX, centerY, fill, height, px, rgb, spacing, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Html.Attributes
import Matrix as M
import Time


main =
    Browser.element
        { init = \() -> ( origin, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Cell
    = Alive
    | Dead


type State
    = Creation
    | Evolution


type alias Universe =
    { livings : M.Matrix Cell
    , state : State
    }


origin =
    { livings = M.repeat ( size, size ) Dead
    , state = Creation
    }


size : Int
size =
    60


speed : Float
speed =
    -- in Hz
    6


type Msg
    = ChangeState ( Int, Int )
    | Start
    | Evolve


update : Msg -> Universe -> ( Universe, Cmd Msg )
update msg universe =
    case msg of
        ChangeState index ->
            let
                swap cell =
                    if cell == Alive then
                        Dead

                    else
                        Alive

                wakeUp idx cell =
                    if idx == index then
                        swap cell

                    else
                        cell
            in
            ( { universe | livings = M.indexedMap wakeUp universe.livings }, Cmd.none )

        Start ->
            ( { universe | state = Evolution }, Cmd.none )

        Evolve ->
            ( evolve universe, Cmd.none )


getFrom : Universe -> ( Int, Int ) -> Cell
getFrom { livings } ( i, j ) =
    M.get ( modBy size i, modBy size j ) livings |> Maybe.withDefault Dead


neighbours : ( Int, Int ) -> Universe -> List Cell
neighbours ( i, j ) universe =
    let
        indices =
            [ ( i - 1, j - 1 )
            , ( i - 1, j )
            , ( i - 1, j + 1 )
            , ( i, j - 1 )
            , ( i, j + 1 )
            , ( i + 1, j - 1 )
            , ( i + 1, j )
            , ( i + 1, j + 1 )
            ]
    in
    List.map (getFrom universe) indices


{-| Calculates the next generation of a cell from the amount of its living neighbours
-}
evolveCell : Int -> Cell -> Cell
evolveCell count cell =
    case ( count, cell ) of
        ( 3, Dead ) ->
            Alive

        ( 2, Alive ) ->
            Alive

        ( 3, Alive ) ->
            Alive

        _ ->
            Dead


evolve : Universe -> Universe
evolve universe =
    let
        nextCell index cell =
            neighbours index universe
                |> List.filter ((==) Alive)
                |> List.length
                |> (\count -> evolveCell count cell)
    in
    { universe | livings = M.indexedMap nextCell universe.livings }


subscriptions : Universe -> Sub Msg
subscriptions universe =
    if universe.state == Evolution then
        Time.every (1000 / speed) (\_ -> Evolve)

    else
        Sub.none


view universe =
    Element.layout [] <|
        Element.column
            [ spacing 10
            , centerX
            , centerY
            ]
            [ board universe, playButton universe.state ]


lightGrey =
    rgb 0.85 0.85 0.85


grey =
    rgb 0.6 0.6 0.6


green =
    rgb 0.5 0.73 0.65


column : State -> Int -> List Cell -> Element.Element Msg
column state j cells =
    let
        background cell =
            case cell of
                Alive ->
                    Background.color (rgb 0 0 0)

                Dead ->
                    Background.color lightGrey

        draw i cell =
            let
                attributes =
                    [ width (px 12)
                    , height (px 12)
                    , Element.htmlAttribute <|
                        Html.Attributes.style "transition" "background-color 50ms linear"
                    , background cell
                    ]

                event =
                    Events.onClick <| ChangeState ( i, j )
            in
            Element.el
                (if state == Creation then
                    event :: attributes

                 else
                    attributes
                )
                Element.none
    in
    Element.column [ spacing 1 ] (List.indexedMap draw cells)


board : Universe -> Element.Element Msg
board universe =
    Element.row
        [ spacing 1
        , Background.color grey
        ]
        (List.indexedMap (column universe.state) (M.toList universe.livings))


playButton : State -> Element.Element Msg
playButton state =
    let
        commonAttributes =
            [ Border.rounded 4
            , Border.glow (rgb 0.3 0.3 0.3) 0.4
            , Font.family [ Font.typeface "Ubuntu" ]
            , Font.size 16
            , Element.padding 10
            , Element.pointer
            , centerX
            ]

        atCreation =
            commonAttributes
                ++ [ Background.color green
                   , Font.color (rgb 0.95 0.95 0.95)
                   , Events.onClick Start
                   ]

        atEvolution =
            commonAttributes
                ++ [ Background.color grey
                   , Font.color lightGrey
                   ]
    in
    Element.el
        (if state == Creation then
            atCreation

         else
            atEvolution
        )
        (Element.text "▶ começar")
