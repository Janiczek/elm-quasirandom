module Visualize exposing (main)

import Browser
import Chart as C
import Chart.Attributes as CA
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Html.Lazy
import List.Cartesian
import Quasirandom
import Random exposing (Generator)
import Random.Extra as Random
import Svg
import Svg.Attributes as SvgAttrs


type alias Flags =
    { seed : Int }


type alias Model =
    { seed : Random.Seed
    , points : Int
    , sequences1D : List ( String, List Float )
    , sequences2D : List ( String, List ( Float, Float ) )
    }


type Msg
    = GenerateNewNumbers
    | SetPointsCount String


seqLength1D : Int
seqLength1D =
    250


seqLength2D : Int
seqLength2D =
    1975


deterministicPoints : List ( Float, Float )
deterministicPoints =
    Quasirandom.points2D (2 * seqLength2D)


seqLength1D_ : Float
seqLength1D_ =
    toFloat seqLength1D


seqLength2D_ : Float
seqLength2D_ =
    toFloat seqLength2D


ideal1DGen : Generator (List Float)
ideal1DGen =
    List.range 1 seqLength1D
        |> List.map (\n -> toFloat n / seqLength1D_)
        |> Random.constant


ideal2DGen : Generator (List ( Float, Float ))
ideal2DGen =
    let
        pointsPerAxis : Int
        pointsPerAxis =
            round (sqrt seqLength2D_)

        pointsPerAxis_ : Float
        pointsPerAxis_ =
            toFloat pointsPerAxis

        seq : List Float
        seq =
            List.range 1 pointsPerAxis
                |> List.map (\n -> toFloat n / pointsPerAxis_)
    in
    List.Cartesian.map2 Tuple.pair seq seq
        |> Random.constant


prng1DGen : Generator (List Float)
prng1DGen =
    Random.list seqLength1D (Random.float 0 1)


prng2DGen : Generator (List ( Float, Float ))
prng2DGen =
    Random.list seqLength2D
        (Random.map2 Tuple.pair
            (Random.float 0 1)
            (Random.float 0 1)
        )


sequences1DGen : Generator (List ( String, List Float ))
sequences1DGen =
    [ ( "Linear interpolation", ideal1DGen )
    , ( "Random.float 0 1", prng1DGen )
    , ( "Quasirandom", Quasirandom.points1DGen seqLength1D )
    ]
        |> List.map (\( label, gen ) -> Random.map (Tuple.pair label) gen)
        |> Random.sequence


sequences2DGen : Generator (List ( String, List ( Float, Float ) ))
sequences2DGen =
    [ ( "Lattice", ideal2DGen )
    , ( "(Random.float 0 1)^2", prng2DGen )
    , ( "Quasirandom", Quasirandom.points2DGen seqLength2D )
    ]
        |> List.map (\( label, gen ) -> Random.map (Tuple.pair label) gen)
        |> Random.sequence


sequencesGen : Generator ( List ( String, List Float ), List ( String, List ( Float, Float ) ) )
sequencesGen =
    Random.pair sequences1DGen sequences2DGen


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init { seed } =
    let
        initialSeed =
            Random.initialSeed seed

        ( ( sequences1D, sequences2D ), newSeed ) =
            Random.step sequencesGen initialSeed
    in
    ( { seed = newSeed
      , points = 335
      , sequences1D = sequences1D
      , sequences2D = sequences2D
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateNewNumbers ->
            let
                ( ( sequences1D, sequences2D ), newSeed ) =
                    Random.step sequencesGen model.seed
            in
            ( { model
                | seed = newSeed
                , sequences1D = sequences1D
                , sequences2D = sequences2D
              }
            , Cmd.none
            )

        SetPointsCount countString ->
            case String.toInt countString of
                Just count ->
                    ( { model | points = count }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.button
            [ Events.onClick GenerateNewNumbers ]
            [ Html.text "Generate new numbers" ]
        , model.sequences1D
            |> Html.Lazy.lazy
                (\sequence ->
                    sequence
                        |> List.map viewSequence1D
                        |> Html.div [ Attrs.class "sequence-1d" ]
                )
        , model.sequences2D
            |> Html.Lazy.lazy
                (\sequence ->
                    sequence
                        |> List.map viewSequence2D
                        |> Html.div [ Attrs.class "sequence-2d" ]
                )
        , model.points
            |> Html.Lazy.lazy
                (\points ->
                    Html.div [ Attrs.class "deterministic" ]
                        [ Html.div []
                            [ Html.input
                                [ Attrs.value (String.fromInt points)
                                , Attrs.type_ "range"
                                , Attrs.min "0"
                                , Attrs.max (String.fromInt (2 * seqLength2D))
                                , Events.onInput SetPointsCount
                                ]
                                []
                            ]
                        , ( "Deterministic (points: " ++ String.fromInt points ++ ")"
                          , List.take points deterministicPoints
                          )
                            |> viewSequence2D
                        ]
                )
        ]


{-| In the [0,1) interval
-}
viewSequence1D : ( String, List Float ) -> Html Msg
viewSequence1D ( label, numbers ) =
    Html.div [ Attrs.class "chart" ]
        [ Html.div [] [ Html.text <| label ++ " (star discrepancy: " ++ String.fromFloat (starDiscrepancy1D numbers) ++ ")" ]
        , Svg.svg [ SvgAttrs.viewBox "0 0 300 5" ]
            (numbers
                |> List.map
                    (\x ->
                        let
                            lineX =
                                String.fromFloat (x * 300)
                        in
                        Svg.line
                            [ SvgAttrs.x1 lineX
                            , SvgAttrs.x2 lineX
                            , SvgAttrs.y1 "-2"
                            , SvgAttrs.y2 "2"
                            , SvgAttrs.stroke "#7b4dff"
                            , SvgAttrs.opacity "0.3"
                            ]
                            []
                    )
            )
        ]


{-| In the [0,1) x [0,1) interval
-}
viewSequence2D : ( String, List ( Float, Float ) ) -> Html Msg
viewSequence2D ( label, points ) =
    Html.div [ Attrs.class "chart" ]
        [ Html.text label
        , C.chart
            [ CA.height 300
            , CA.width 300
            , CA.margin { top = 10, bottom = 20, left = 20, right = 20 }
            , CA.padding { top = 10, bottom = 10, left = 10, right = 10 }
            , CA.range
                [ CA.lowest 0 CA.exactly
                , CA.highest 1 CA.exactly
                ]
            , CA.domain
                [ CA.lowest 0 CA.exactly
                , CA.highest 1 CA.exactly
                ]
            ]
            [ C.xLabels
                [ CA.withGrid
                , CA.amount 2
                ]
            , C.yLabels
                [ CA.withGrid
                , CA.amount 2
                ]
            , C.series Tuple.first
                [ C.scatter Tuple.second
                    [ CA.opacity 0.3
                    , CA.borderWidth 0
                    , CA.size 2
                    ]
                ]
                points
            ]
        ]


starDiscrepancy1D : List Float -> Float
starDiscrepancy1D numbers =
    let
        numbersCount : Int
        numbersCount =
            List.length numbers

        numbersCount_ : Float
        numbersCount_ =
            toFloat numbersCount

        intervalCount : Int
        intervalCount =
            100

        intervalSize : Float
        intervalSize =
            1.0 / toFloat intervalCount

        intervalStarts : List Float
        intervalStarts =
            List.range 0 (intervalCount - 1)
                |> List.map (\n -> toFloat n * intervalSize)

        intervalPoints : List (List Float)
        intervalPoints =
            intervalStarts
                |> List.map
                    (\start ->
                        let
                            end : Float
                            end =
                                start + intervalSize
                        in
                        List.filter (\n -> start <= n && n < end) numbers
                    )

        discrepancy : List Float -> Float
        discrepancy pointsInInterval =
            abs <| (toFloat (List.length pointsInInterval) / numbersCount_) - intervalSize

        intervalDiscrepancies : List Float
        intervalDiscrepancies =
            List.map discrepancy intervalPoints
    in
    intervalDiscrepancies
        |> List.maximum
        |> Maybe.withDefault 0
