module Path exposing
    ( Direction(..)
    , Line
    , Path
    , Point
    , combine
    , distanceTo
    , endof
    , intersection
    , move
    , path
    , startof
    )

-- Path


type alias Path =
    List Line


{-| Creates a path from a list of direction strings
-}
path : List String -> Path
path raw =
    List.map fromString raw
        |> List.foldr (Maybe.map2 (::)) (Just [])
        |> Maybe.map walk
        |> Maybe.withDefault []


{-| Creates a path from a list of directions
-}
walk : List Direction -> Path
walk =
    Tuple.second
        << List.foldl
            (\dir ( lastPoint, lines ) ->
                let
                    nextLine =
                        move lastPoint dir

                    nextPoint =
                        endof nextLine
                in
                ( nextPoint, lines ++ [ nextLine ] )
            )
            ( ( 0, 0 ), [] )



-- Line


{-| Represents a horizontal or vertical line by its starting point and length
-}
type Line
    = HLine Point Int
    | VLine Point Int


length : Line -> Int
length line =
    abs <|
        case line of
            HLine _ l ->
                l

            VLine _ l ->
                l


{-| Creates a `Line` by moving a starting point in the given direction
-}
move : Point -> Direction -> Line
move point dir =
    case dir of
        Up y ->
            VLine point y

        Right x ->
            HLine point x

        Down y ->
            VLine point -y

        Left x ->
            HLine point -x


startof : Line -> Point
startof line =
    case line of
        HLine p _ ->
            p

        VLine p _ ->
            p


endof : Line -> Point
endof line =
    case line of
        HLine ( x, y ) dx ->
            ( x + dx, y )

        VLine ( x, y ) dy ->
            ( x, y + dy )



-- Point


type alias Point =
    ( Int, Int )



-- Direction


type Direction
    = Up Int
    | Right Int
    | Down Int
    | Left Int


fromString : String -> Maybe Direction
fromString s =
    let
        dir =
            String.left 1 s

        amount =
            String.dropLeft 1 s |> String.toInt
    in
    Maybe.andThen (\toDir -> Maybe.map toDir amount) <|
        case dir of
            "U" ->
                Just Up

            "R" ->
                Just Right

            "D" ->
                Just Down

            "L" ->
                Just Left

            _ ->
                Nothing



-- Helpers


{-| Calculates `Just` the sum of the lengths of all lines up to the given line
(inclusive). Returns `Nothing` if the given line is not a path segment.
-}
distanceTo : Line -> Path -> Maybe Int
distanceTo line p =
    let
        ( foundLines, foundLine ) =
            List.foldl
                (\l ( lines, found ) ->
                    if found == True then
                        ( lines, found )

                    else
                        ( l :: lines, l == line )
                )
                ( [], False )
                p
    in
    if foundLine == False then
        Nothing

    else
        List.map length foundLines
            |> List.sum
            |> Just


{-| Calculates `Just` the `Point` of intersection of two perpendicular line
segments. Returns `Nothing` if the lines don't intersect.
-}
intersection : ( Line, Line ) -> Maybe Point
intersection ( linea, lineb ) =
    let
        intersectionPoint =
            \xh yh dx xv yv dy ->
                if
                    {- Check vertical line pointing up
                                  (dy > 0)
                               •••••• ▲ ••••••
                               •••••• | ••••••
                               •••••• | ••••••
                       (_, yh) •• ----x---- ••
                               •••••• | ••••••
                               •••••• | ••••••
                               •••••• | ••••••
                                   (_, yv)
                    -}
                    ((yv <= yh && yv + dy >= yh)
                        {- Check vertical line pointing down
                                       (_, yv)
                                   •••••• | ••••••
                                   •••••• | ••••••
                                   •••••• | ••••••
                           (_, yh) •• ----x---- ••
                                   •••••• | ••••••
                                   •••••• | ••••••
                                   •••••• ▼ ••••••
                                      (dy < 0)
                        -}
                        || (yv >= yh && yv + dy <= yh)
                    )
                        {- Check horizontal line pointing right
                                       (xv, _)
                                   •••••••••••••••
                                   •••••• | ••••••
                                   •••••• | ••••••
                           (xh, _) -------x------▶ (dx > 0)
                                   •••••• | ••••••
                                   •••••• | ••••••
                                   •••••••••••••••
                        -}
                        && ((xh <= xv && xh + dx >= xv)
                                {- Check horizontal line pointing left
                                                (xv, _)
                                            •••••••••••••••
                                            •••••• | ••••••
                                            •••••• | ••••••
                                   (dx < 0) ◀------x------- (xh, _)
                                            •••••• | ••••••
                                            •••••• | ••••••
                                            •••••••••••••••
                                -}
                                || (xh >= xv && xh + dx <= xv)
                           )
                then
                    Just ( xv, yh )

                else
                    Nothing
    in
    case ( linea, lineb ) of
        ( HLine ( xh, yh ) dx, VLine ( xv, yv ) dy ) ->
            intersectionPoint xh yh dx xv yv dy

        ( VLine ( xv, yv ) dy, HLine ( xh, yh ) dx ) ->
            intersectionPoint xh yh dx xv yv dy

        _ ->
            Nothing


{-| Returns the cross product of the path segments
-}
combine : Path -> Path -> List ( Line, Line )
combine patha pathb =
    List.map
        (\linea ->
            List.map
                (\lineb ->
                    ( linea, lineb )
                )
                pathb
        )
        patha
        |> List.concat
