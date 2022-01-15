module Quasirandom exposing
    ( points1D, points2D, points3D, points
    , points1DGen, points2DGen, points3DGen, pointsGen
    , next1D, next2D, next3D, next, nextForDimension
    , nth1D, nth2D, nth3D, nth, nthForDimension
    )

{-| These quasirandom (low-discrepancy) sequences are based on the blogpost
"The Unreasonable Effectiveness of Quasirandom Sequences":

<http://extremelearning.com.au/unreasonable-effectiveness-of-quasirandom-sequences/>


# Low-discrepancy sequences

@docs points1D, points2D, points3D, points


# Randomized low-discrepancy sequences

@docs points1DGen, points2DGen, points3DGen, pointsGen


# Stepping functions

@docs next1D, next2D, next3D, next, nextForDimension


# "Nth point" functions

@docs nth1D, nth2D, nth3D, nth, nthForDimension

-}

import List.Extra as List
import Random exposing (Generator)



-- Low-discrepancy sequences


{-| Generate a low-discrepancy sequence of N points in the `[0,1)` interval.
-}
points1D : Int -> List Float
points1D count =
    seq 0.5 phi1 count


{-| Generate a low-discrepancy sequence of N points in the `[0,1)^2` space.
-}
points2D : Int -> List ( Float, Float )
points2D count =
    List.map2 Tuple.pair
        (seq 0.5 phi1 count)
        (seq 0.5 phi2 count)


{-| Generate a low-discrepancy sequence of N points in the `[0,1)^3` space.
-}
points3D : Int -> List ( Float, Float, Float )
points3D count =
    List.map3 (\x y z -> ( x, y, z ))
        (seq 0.5 phi1 count)
        (seq 0.5 phi2 count)
        (seq 0.5 phi3 count)


{-| A generalized way to generate N points in D dimensions (each dimension
confined to the `[0,1)` interval).

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
    List.range 1 dimensions
        |> List.map (\dimension -> seq 0.5 (phi dimension) count)
        |> List.transpose



-- Randomized low-discrepancy sequences


{-| Generate a randomized low-discrepancy sequence of N points in the `[0,1)`
interval.
-}
points1DGen : Int -> Generator (List Float)
points1DGen count =
    Random.float 0 1
        |> Random.map (\s0 -> seq s0 phi1 count)


{-| Generate a randomized low-discrepancy sequence of N points in the `[0,1)^2`
space.
-}
points2DGen : Int -> Generator (List ( Float, Float ))
points2DGen count =
    Random.float 0 1
        |> Random.map
            (\s0 ->
                List.map2 Tuple.pair
                    (seq s0 phi1 count)
                    (seq s0 phi2 count)
            )


{-| Generate a randomized low-discrepancy sequence of N points in the `[0,1)^3`
space.
-}
points3DGen : Int -> Generator (List ( Float, Float, Float ))
points3DGen count =
    Random.float 0 1
        |> Random.map
            (\s0 ->
                List.map3 (\x y z -> ( x, y, z ))
                    (seq s0 phi1 count)
                    (seq s0 phi2 count)
                    (seq s0 phi3 count)
            )


{-| A randomized generalized way to generate N points in D dimensions (each
dimension confined to the `[0,1)` interval).

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
    Random.float 0 1
        |> Random.map
            (\s0 ->
                List.range 1 dimensions
                    |> List.map (\dimension -> seq s0 (phi dimension) count)
                    |> List.transpose
            )



-- Stepping functions


{-| Given a point in the `[0,1)` interval, generate another one in the
low-discrepancy sequence.

Given enough of these, they will cover the interval uniformly.

Note that if you need to generate these numbers on multiple axes, you should
instead use `nextForDimension` and give each axis a different `dimension` value.

-}
next1D : Float -> Float
next1D x =
    fractionalPart (x + phi1)


{-| Given a point in the `[0,1)^2` space, generate another one in the
low-discrepancy sequence.

Given enough of these, they will cover the space uniformly.

-}
next2D : ( Float, Float ) -> ( Float, Float )
next2D ( x, y ) =
    ( fractionalPart (x + phi1)
    , fractionalPart (y + phi2)
    )


{-| Given a point in the `[0,1)^3` space, generate another one in the
low-discrepancy sequence.

Given enough of these, they will cover the space uniformly.

-}
next3D : ( Float, Float, Float ) -> ( Float, Float, Float )
next3D ( x, y, z ) =
    ( fractionalPart (x + phi1)
    , fractionalPart (y + phi2)
    , fractionalPart (z + phi3)
    )


{-| Given a N-dimensional point, generate another one in the low-discrepancy
sequence.

Given enough of these, they will cover the space uniformly.

-}
next : List Float -> List Float
next point =
    List.indexedMap nextForDimension point


{-| Given a point component for a given dimension, generate another one in the
low-discrepancy sequence.

Given enough of these, they will cover the space uniformly.

Note that if `d1 /= d2` then `nextForDimension d1 x /= nextForDimension d2 x`.
This is a good thing!

-}
nextForDimension : Int -> Float -> Float
nextForDimension d component =
    fractionalPart (component + phi d)



-- "Nth point" functions


{-| Generate n-th point in the 1D low-discrepancy sequence.

Note that for speed with large `n`s, this uses a multiplicative algorithm
instead of an additive one that's used by `points*` and `next*`, resulting in a
slight loss of precision, and so the sequences generated using `nth*` will
eventually diverge from the sequences generated using the rest of the functions
in this module.

-}
nth1D : Int -> Float
nth1D n =
    r1 0.5 phi1 n


{-| Generate n-th point in the 2D low-discrepancy sequence.

Note that for speed with large `n`s, this uses a multiplicative algorithm
instead of an additive one that's used by `points*` and `next*`, resulting in a
slight loss of precision, and so the sequences generated using `nth*` will
eventually diverge from the sequences generated using the rest of the functions
in this module.

-}
nth2D : Int -> ( Float, Float )
nth2D n =
    ( r1 0.5 phi1 n
    , r1 0.5 phi2 n
    )


{-| Generate n-th point in the 3D low-discrepancy sequence.

Note that for speed with large `n`s, this uses a multiplicative algorithm
instead of an additive one that's used by `points*` and `next*`, resulting in a
slight loss of precision, and so the sequences generated using `nth*` will
eventually diverge from the sequences generated using the rest of the functions
in this module.

-}
nth3D : Int -> ( Float, Float, Float )
nth3D n =
    ( r1 0.5 phi1 n
    , r1 0.5 phi2 n
    , r1 0.5 phi3 n
    )


{-| Generate n-th point in the N-dimensional low-discrepancy sequence.

Note that for speed with large `n`s, this uses a multiplicative algorithm
instead of an additive one that's used by `points*` and `next*`, resulting in a
slight loss of precision, and so the sequences generated using `nth*` will
eventually diverge from the sequences generated using the rest of the functions
in this module.

-}
nth : { dimensions : Int, n : Int } -> List Float
nth { dimensions, n } =
    List.range 1 dimensions
        |> List.map (\d -> r1 0.5 (phi d) n)


{-| Generate n-th point's component for the n-th dimension in the
low-discrepancy sequence.

Note that for speed with large `n`s, this uses a multiplicative algorithm
instead of an additive one that's used by `points*` and `next*`, resulting in a
slight loss of precision, and so the sequences generated using `nth*` will
eventually diverge from the sequences generated using the rest of the functions
in this module.

-}
nthForDimension : { dimension : Int, n : Int } -> Float
nthForDimension { dimension, n } =
    r1 0.5 (phi dimension) n



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
r1 s0 phi_ n =
    fractionalPart (s0 + toFloat n * phi_)


fractionalPart : Float -> Float
fractionalPart n =
    n - toFloat (floor n)


doNTimes : Int -> (a -> a) -> a -> a
doNTimes n fn value =
    if n <= 0 then
        value

    else
        doNTimes (n - 1) fn (fn value)


seq : Float -> Float -> Int -> List Float
seq s0 phi_ n =
    let
        go : Int -> ( Float, List Float ) -> List Float
        go n_ ( last, acc ) =
            if n_ <= 0 then
                List.reverse acc

            else
                let
                    next_ : Float
                    next_ =
                        fractionalPart (last + phi_)
                in
                go (n_ - 1) ( next_, next_ :: acc )
    in
    go n ( s0, [] )
