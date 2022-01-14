module Visualize exposing (main)

import Browser
import Chart as C
import Chart.Attributes as CA
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import List.Cartesian
import Quasirandom
import Random exposing (Generator)
import Random.Extra as Random


type alias Flags =
    { seed : Int }


type alias Model =
    { seed : Random.Seed
    , sequences1D : List ( String, List Float )
    , sequences2D : List ( String, List ( Float, Float ) )
    }


type Msg
    = GenerateNewNumbers


seqLength1D : Int
seqLength1D =
    250


seqLength2D : Int
seqLength2D =
    1000


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
    [ ( "Lattice", ideal1DGen )
    , ( "Random.float 0 1", prng1DGen )
    , ( "R1 with a = golden ratio", Quasirandom.points1DGen seqLength1D )
    ]
        |> List.map (\( label, gen ) -> Random.map (Tuple.pair label) gen)
        |> Random.sequence


sequences2DGen : Generator (List ( String, List ( Float, Float ) ))
sequences2DGen =
    [ ( "Lattice", ideal2DGen )
    , ( "Random.float 0 1", prng2DGen )
    , ( "R2 with a = golden ratio", Quasirandom.points2DGen seqLength2D )
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
            |> List.map viewSequence1D
            |> Html.div [ Attrs.class "1d" ]
        , model.sequences2D
            |> List.map viewSequence2D
            |> Html.div [ Attrs.class "2d" ]
        ]


{-| In the [0,1) interval
-}
viewSequence1D : ( String, List Float ) -> Html Msg
viewSequence1D ( label, numbers ) =
    Html.div [ Attrs.class "chart" ]
        [ Html.text <| label ++ " (star discrepancy: " ++ String.fromFloat (starDiscrepancy1D numbers) ++ ")"
        , C.chart
            [ CA.height 50
            , CA.width 300
            , CA.margin { top = 10, bottom = 20, left = 20, right = 20 }
            , CA.padding { top = 10, bottom = 10, left = 10, right = 10 }
            , CA.range
                [ CA.lowest 0 CA.orLower
                , CA.highest 1 CA.orHigher
                ]
            ]
            [ C.xLabels
                [ CA.withGrid
                , CA.amount 2
                ]
            , C.series identity
                [ C.scatter (\_ -> 0)
                    [ CA.opacity 0.2
                    , CA.borderWidth 0
                    , CA.size 2
                    ]
                ]
                numbers
            ]
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
                [ CA.lowest 0 CA.orLower
                , CA.highest 1 CA.orHigher
                ]
            , CA.domain
                [ CA.lowest 0 CA.orLower
                , CA.highest 1 CA.orHigher
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
                    [ CA.opacity 0.2
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
