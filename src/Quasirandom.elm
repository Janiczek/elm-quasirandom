module Quasirandom exposing
    ( points1D, points2D, points3D, points
    , points1DGen, points2DGen, points3DGen, pointsGen
    )

{-| These quasirandom (low-discrepancy) sequences are based on the blogpost
"The Unreasonable Effectiveness of Quasirandom Sequences":

<http://extremelearning.com.au/unreasonable-effectiveness-of-quasirandom-sequences/>


# Low-discrepancy sequences

@docs points1D, points2D, points3D, points


# Randomized low-discrepancy sequences

@docs points1DGen, points2DGen, points3DGen, pointsGen

-}

import Random exposing (Generator)



-- Low-discrepancy sequences


points1D : Int -> List Float
points1D count =
    List.range 1 count
        |> List.map (r1 0.5 phi1)


points2D : Int -> List ( Float, Float )
points2D count =
    List.range 1 count
        |> List.map
            (\i ->
                ( r1 0.5 phi1 i
                , r1 0.5 phi2 i
                )
            )


points3D : Int -> List ( Float, Float, Float )
points3D count =
    List.range 1 count
        |> List.map
            (\i ->
                ( r1 0.5 phi1 i
                , r1 0.5 phi2 i
                , r1 0.5 phi3 i
                )
            )


{-| A generalized way to generate N points in D dimensions.

The returned data is in shape:

    [ point0, point1, point2, ... ]

Where each point is structured like

    [ x, y, ... ]

Here's an example:

    points { dimensions = 1, count = 100 }
    --> [ [x0], [x1], [x2], [x3], [x4], ... ]

    points { dimensions = 2, count = 100 }
    --> [ [x0, y0], [x1, y1], [x2, y2], [x3, y3], [x4, y4], ... ]

etc.

If you want a randomized sequence, use `pointsGen`.

-}
points : { dimensions : Int, count : Int } -> List (List Float)
points { dimensions, count } =
    let
        phis : List Float
        phis =
            List.range 1 dimensions
                |> List.map phi
    in
    List.range 1 count
        |> List.map (\i -> List.map (\phi_ -> r1 0.5 phi_ i) phis)



-- Randomized low-discrepancy sequences


points1DGen : Int -> Generator (List Float)
points1DGen count =
    Random.float 0 1
        |> Random.map
            (\s0 ->
                List.range 1 count
                    |> List.map (r1 s0 phi1)
            )


points2DGen : Int -> Generator (List ( Float, Float ))
points2DGen count =
    Random.float 0 1
        |> Random.map
            (\s0 ->
                List.range 1 count
                    |> List.map
                        (\i ->
                            ( r1 s0 phi1 i
                            , r1 s0 phi2 i
                            )
                        )
            )


points3DGen : Int -> Generator (List ( Float, Float, Float ))
points3DGen count =
    Random.float 0 1
        |> Random.map
            (\s0 ->
                List.range 1 count
                    |> List.map
                        (\i ->
                            ( r1 s0 phi1 i
                            , r1 s0 phi2 i
                            , r1 s0 phi3 i
                            )
                        )
            )


{-| A randomized generalized way to generate N points in D dimensions.

The returned data is in shape:

    [ point0, point1, point2, ... ]

Where each point is structured like

    [ x, y, ... ]

depending on the number of dimensions. Here's an example from the
non-randomized function:

    points { dimensions = 1, count = 100 }
    --> [ [x0], [x1], [x2], [x3], [x4], ... ]

    points { dimensions = 2, count = 100 }
    --> [ [x0, y0], [x1, y1], [x2, y2], [x3, y3], [x4, y4], ... ]

etc.

-}
pointsGen : { dimensions : Int, count : Int } -> Generator (List (List Float))
pointsGen { dimensions, count } =
    let
        phis : List Float
        phis =
            List.range 1 dimensions
                |> List.map phi
    in
    Random.float 0 1
        |> Random.map
            (\s0 ->
                List.range 1 count
                    |> List.map (\i -> List.map (\phi_ -> r1 s0 phi_ i) phis)
            )



-- Generalized golden numbers


phi1 =
    1.618033988749895


phi2 =
    1.324717957244746


phi3 =
    1.2207440846057596


phi : Int -> Float
phi d_ =
    let
        d : Float
        d =
            toFloat d_

        exp : Float
        exp =
            1 / (d + 1)

        step : Float -> Float
        step x =
            (1 + x) ^ exp
    in
    doNTimes 10 step 2



-- Helpers


r1 : Float -> Float -> Int -> Float
r1 s0 a n =
    fractionalPart (s0 + toFloat n * a)


fractionalPart : Float -> Float
fractionalPart n =
    n - toFloat (floor n)


doNTimes : Int -> (a -> a) -> a -> a
doNTimes n fn value =
    if n <= 0 then
        value

    else
        doNTimes (n - 1) fn (fn value)
