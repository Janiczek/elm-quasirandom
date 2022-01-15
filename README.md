# `Janiczek/elm-quasirandom`

Functions for generating [quasirandom (low-discrepancy) numbers](https://en.wikipedia.org/wiki/Low-discrepancy_sequence), roughly speaking a random sequence that fills the N-dimensional space with as little gaps between points as possible (given enough points).

Inspired by [@gampleman's post](https://gist.github.com/gampleman/b46f9b60c25e00a31d2416a4cb113672) on improving elm-test and the [paper on quasi-random testing](https://www.researchgate.net/publication/3152943_Quasi-Random_Testing).

Implementation largely based on the blogpost ["The Unreasonable Effectiveness of Quasirandom Sequences"](http://extremelearning.com.au/unreasonable-effectiveness-of-quasirandom-sequences/).

```elm
points1D : Int -> List Float
points2D : Int -> List ( Float, Float )
points3D : Int -> List ( Float, Float, Float )
points : { dimensions : Int, count : Int } -> List (List Float)

points1DGen : Int -> Generator (List Float)
points2DGen : Int -> Generator (List ( Float, Float ))
points3DGen : Int -> Generator (List ( Float, Float, Float ))
pointsGen : { dimensions : Int, count : Int } -> Generator (List (List Float))

next1D : Float -> Float
next2D : ( Float, Float ) -> ( Float, Float )
next3D : ( Float, Float, Float ) -> ( Float, Float, Float )
next : List Float -> List Float
nextForDimension : Int -> Float -> Float

nth1D : Int -> Float
nth2D : Int -> ( Float, Float )
nth3D : Int -> ( Float, Float, Float )
nth : { dimensions : Int, n : Int } -> List Float
nthForDimension : { dimension : Int, n : Int } -> Float
```

[![1D sequences](https://github.com/Janiczek/elm-quasirandom/raw/master/1d.png)](https://github.com/Janiczek/elm-quasirandom/raw/master/1d.png)

[![2D sequences](https://github.com/Janiczek/elm-quasirandom/raw/master/2d.png)](https://github.com/Janiczek/elm-quasirandom/raw/master/2d.png)

