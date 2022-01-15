module PointsBenchmark exposing (main)

import Benchmark exposing (Benchmark, benchmark)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Html exposing (Html)


suite : Benchmark
suite =
    Benchmark.compare "points"
        "additive"
        (\() -> additive 10000)
        "multiplicative"
        (\() -> multiplicative 10000)


main : BenchmarkProgram
main =
    program suite


additive : Int -> List Float
additive count =
    seq 0.5 phi1 count


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


phi1 =
    1.618033988749895


r1 : Float -> Float -> Int -> Float
r1 s0 phi_ n =
    fractionalPart (s0 + toFloat n * phi_)


multiplicative : Int -> List Float
multiplicative count =
    List.range 1 count
        |> List.map (r1 0.5 phi1)


fractionalPart : Float -> Float
fractionalPart n =
    n - toFloat (floor n)
