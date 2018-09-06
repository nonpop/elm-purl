module Test.Purl exposing (suite)

import Expect exposing (Expectation)
import Purl
import Test exposing (..)
import Url


suite : Test
suite =
    describe "Url"
        [ test "Root url is empty" <|
            \_ ->
                Purl.root
                    |> Purl.toString ()
                    |> Expect.equal "/"
        , test "Can append a static part" <|
            \_ ->
                (Purl.root |> Purl.append (Purl.s "part"))
                    |> Purl.toString ()
                    |> Expect.equal "/part"
        , test "Can append two static parts" <|
            \_ ->
                (Purl.root |> Purl.append (Purl.s "part1") |> Purl.append (Purl.s "part2"))
                    |> Purl.toString ()
                    |> Expect.equal "/part1/part2"
        , test "Can append two Nothing static parts" <|
            \_ ->
                (Purl.root |> Purl.append (Purl.maybeS Nothing) |> Purl.append (Purl.maybeS Nothing))
                    |> Purl.toString ()
                    |> Expect.equal "/"
        , test "Can append a Just and a Nothing static part" <|
            \_ ->
                (Purl.root |> Purl.append (Purl.maybeS (Just "part1")) |> Purl.append (Purl.maybeS Nothing))
                    |> Purl.toString ()
                    |> Expect.equal "/part1"
        , test "Can append two Just static parts" <|
            \_ ->
                (Purl.root |> Purl.append (Purl.maybeS (Just "part1")) |> Purl.append (Purl.maybeS (Just "part2")))
                    |> Purl.toString ()
                    |> Expect.equal "/part1/part2"
        , test "Can append variable integer" <|
            \_ ->
                (Purl.root |> Purl.append (Purl.int .id))
                    |> Purl.toString { id = 42 }
                    |> Expect.equal "/42"
        , test "Can append static part and variable integer" <|
            \_ ->
                (Purl.root |> Purl.append (Purl.s "part") |> Purl.append (Purl.int .id))
                    |> Purl.toString { id = 42 }
                    |> Expect.equal "/part/42"
        , test "Can append variable integer and static part" <|
            \_ ->
                (Purl.root |> Purl.append (Purl.int .id) |> Purl.append (Purl.s "part"))
                    |> Purl.toString { id = 42 }
                    |> Expect.equal "/42/part"
        , test "Can append two variable integers" <|
            \_ ->
                (Purl.root |> Purl.append (Purl.int .id) |> Purl.append (Purl.int .otherId))
                    |> Purl.toString { id = 42, otherId = 24 }
                    |> Expect.equal "/42/24"
        , test "Can append two variable integers the other way" <|
            \_ ->
                (Purl.root |> Purl.append (Purl.int .otherId) |> Purl.append (Purl.int .id))
                    |> Purl.toString { id = 42, otherId = 24 }
                    |> Expect.equal "/24/42"
        , test "Can append two Nothing integers" <|
            \_ ->
                (Purl.root |> Purl.append (Purl.maybeInt .id) |> Purl.append (Purl.maybeInt .otherId))
                    |> Purl.toString { id = Nothing, otherId = Nothing }
                    |> Expect.equal "/"
        , test "Can append a Just and a Nothing integer" <|
            \_ ->
                (Purl.root |> Purl.append (Purl.maybeInt .id) |> Purl.append (Purl.maybeInt .otherId))
                    |> Purl.toString { id = Just 42, otherId = Nothing }
                    |> Expect.equal "/42"
        , test "Can append two Just integers" <|
            \_ ->
                (Purl.root |> Purl.append (Purl.maybeInt .id) |> Purl.append (Purl.maybeInt .otherId))
                    |> Purl.toString { id = Just 42, otherId = Just 24 }
                    |> Expect.equal "/42/24"
        , test "Can append a string" <|
            \_ ->
                (Purl.root |> Purl.append (Purl.string .name))
                    |> Purl.toString { name = "everything" }
                    |> Expect.equal "/everything"
        , test "Can append a Just string" <|
            \_ ->
                (Purl.root |> Purl.append (Purl.maybeString .name))
                    |> Purl.toString { name = Just "everything" }
                    |> Expect.equal "/everything"
        , test "Can append a bool" <|
            \_ ->
                (Purl.root |> Purl.append (Purl.bool .show))
                    |> Purl.toString { show = True }
                    |> Expect.equal "/true"
        , test "Can append a Just bool" <|
            \_ ->
                (Purl.root |> Purl.append (Purl.maybeBool .show))
                    |> Purl.toString { show = Just True }
                    |> Expect.equal "/true"
        , test "Can use custom segment" <|
            \_ ->
                let
                    idsSegment =
                        Purl.custom (.ids >> List.map String.fromInt >> String.join ";")
                in
                (Purl.root |> Purl.append idsSegment)
                    |> Purl.toString { ids = [ 1, 2, 3 ] }
                    |> Expect.equal ("/" ++ Url.percentEncode "1;2;3")
        , test "Can use custom Maybe segment" <|
            \_ ->
                let
                    idsSegment =
                        Purl.maybeCustom (.ids >> List.filterMap (Maybe.map String.fromInt) >> String.join ";" >> Just)
                in
                (Purl.root |> Purl.append idsSegment)
                    |> Purl.toString { ids = [ Just 1, Nothing, Just 3 ] }
                    |> Expect.equal ("/" ++ Url.percentEncode "1;3")
        , test "Can append parameter" <|
            \_ ->
                (Purl.root |> Purl.append (Purl.s "part") |> Purl.appendParam "id" (Purl.int .id))
                    |> Purl.toString { id = 42 }
                    |> Expect.equal "/part?id=42"
        , test "Can append two parameters" <|
            \_ ->
                (Purl.root |> Purl.append (Purl.s "part") |> Purl.appendParam "id" (Purl.int .id) |> Purl.appendParam "show" (Purl.bool .show))
                    |> Purl.toString { id = 42, show = True }
                    |> Expect.equal "/part?id=42&show=true"
        , test "Can append a Nothing and a Just parameter" <|
            \_ ->
                (Purl.root |> Purl.append (Purl.maybeS (Just "part")) |> Purl.appendParam "id" (Purl.maybeInt .id) |> Purl.appendParam "show" (Purl.maybeBool .show))
                    |> Purl.toString { id = Just 42, show = Nothing }
                    |> Expect.equal "/part?id=42"
        , test "Can have static parts, variables, and parameters" <|
            \_ ->
                (Purl.root |> Purl.append (Purl.s "part") |> Purl.append (Purl.int .id) |> Purl.appendParam "show" (Purl.bool .show))
                    |> Purl.toString { id = 42, show = True }
                    |> Expect.equal "/part/42?show=true"
        ]
