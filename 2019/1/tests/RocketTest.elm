module RocketTest exposing (suite)

import Expect
import Rocket
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "The Tyranny of the Rocket Equation"
        [ test "Mass 12 needs fuel 2" <|
            \_ ->
                Rocket.requiredModuleFuel (Rocket.moduleOfMass 12)
                    |> Expect.equal 2
        , test "Mass 14 needs fuel 2" <|
            \_ ->
                Rocket.requiredModuleFuel (Rocket.moduleOfMass 14)
                    |> Expect.equal 2
        , test "Mass 1969 needs fuel 966" <|
            \_ ->
                Rocket.requiredModuleFuel (Rocket.moduleOfMass 1969)
                    |> Expect.equal 966
        , test "Mass 100756 needs fuel 50346" <|
            \_ ->
                Rocket.requiredModuleFuel (Rocket.moduleOfMass 100756)
                    |> Expect.equal 50346
        ]
