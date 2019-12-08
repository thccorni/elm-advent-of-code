module IntcodeComputer exposing (run, solve)

import Array exposing (Array)


type alias IntcodeComputer =
    { state : State
    , memory : Array Int
    }


type State
    = End
    | Running Address


type alias Address =
    Int


execInstructionAtAddress : IntcodeComputer -> Result String IntcodeComputer
execInstructionAtAddress comp =
    case comp.state of
        End ->
            Err "Nothing to run"

        Running pos ->
            let
                nextPos =
                    pos + 4

                program =
                    Array.slice pos nextPos comp.memory
            in
            case Array.toList program of
                99 :: _ ->
                    Ok (IntcodeComputer End comp.memory)

                1 :: ina :: inb :: out :: _ ->
                    calc (+) "add" ( ina, inb, out ) ( pos, comp.memory )
                        |> Result.map (IntcodeComputer (Running nextPos))

                2 :: ina :: inb :: out :: _ ->
                    calc (*) "mul" ( ina, inb, out ) ( pos, comp.memory )
                        |> Result.map (IntcodeComputer (Running nextPos))

                _ ->
                    Err "Unknown operation"


calc : (Int -> Int -> Int) -> String -> ( Int, Int, Int ) -> ( Int, Array Int ) -> Result String (Array Int)
calc op name ( ina, inb, out ) ( pointer, stack ) =
    Maybe.map2 op (Array.get ina stack) (Array.get inb stack)
        |> Maybe.map (\res -> Array.set out res stack)
        |> (Result.fromMaybe <|
                "Could not run operation: '"
                    ++ name
                    ++ "' at position "
                    ++ String.fromInt pointer
           )


interpret : IntcodeComputer -> Result String IntcodeComputer
interpret comp =
    execInstructionAtAddress comp
        |> Result.andThen
            (\c ->
                case c.state of
                    End ->
                        Ok c

                    _ ->
                        interpret c
            )


run : List Int -> Result String (List Int)
run memory =
    Array.fromList memory
        |> IntcodeComputer (Running 0)
        |> interpret
        |> Result.map (Array.toList << .memory)


input : Int -> Int -> List Int
input p1 p2 =
    [ 1, p1, p2, 3, 1, 1, 2, 3, 1, 3, 4, 3, 1, 5, 0, 3, 2, 10, 1, 19, 1, 19, 9, 23, 1, 23, 6, 27, 1, 9, 27, 31, 1, 31, 10, 35, 2, 13, 35, 39, 1, 39, 10, 43, 1, 43, 9, 47, 1, 47, 13, 51, 1, 51, 13, 55, 2, 55, 6, 59, 1, 59, 5, 63, 2, 10, 63, 67, 1, 67, 9, 71, 1, 71, 13, 75, 1, 6, 75, 79, 1, 10, 79, 83, 2, 9, 83, 87, 1, 87, 5, 91, 2, 91, 9, 95, 1, 6, 95, 99, 1, 99, 5, 103, 2, 103, 10, 107, 1, 107, 6, 111, 2, 9, 111, 115, 2, 9, 115, 119, 2, 13, 119, 123, 1, 123, 9, 127, 1, 5, 127, 131, 1, 131, 2, 135, 1, 135, 6, 0, 99, 2, 0, 14, 0 ]


solve =
    solveHelper 0 0


solveHelper : Int -> Int -> Maybe ( Int, Int )
solveHelper p1 p2 =
    case ( p1, p2 ) of
        ( 0, 100 ) ->
            Nothing

        ( 100, _ ) ->
            solveHelper 0 (p2 + 1)

        ( _, _ ) ->
            let
                result =
                    input p1 p2
                        |> run
                        |> Result.withDefault []
                        |> List.head
                        |> Maybe.withDefault -1
            in
            if result == 19690720 then
                Just ( p1, p2 )

            else
                solveHelper (p1 + 1) p2
