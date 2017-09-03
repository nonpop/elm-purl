module UrlTest exposing (..)

import Url exposing ((</>), (<?>), (@))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import String.Extra
import Test exposing (..)


suite : Test
suite =
    describe "Url"
        [ test "Root url is empty" <|
            \_ ->
                Url.root
                    |> Url.toString ()
                    |> Expect.equal "/"
        , test "Can append a static part" <|
            \_ ->
                (Url.root |> Url.append (Url.s "part"))
                    |> Url.toString ()
                    |> Expect.equal "/part"
        , test "Can append two static parts" <|
            \_ ->
                (Url.root |> Url.append (Url.s "part1") |> Url.append (Url.s "part2"))
                    |> Url.toString ()
                    |> Expect.equal "/part1/part2"
        , test "Can append variable integer" <|
            \_ ->
                (Url.root |> Url.append (Url.int .id))
                    |> Url.toString { id = 42 }
                    |> Expect.equal "/42"
        , test "Can append static part and variable integer" <|
            \_ ->
                (Url.root |> Url.append (Url.s "part") |> Url.append (Url.int .id))
                    |> Url.toString { id = 42 }
                    |> Expect.equal "/part/42"
        , test "Can append variable integer and static part" <|
            \_ ->
                (Url.root |> Url.append (Url.int .id) |> Url.append (Url.s "part"))
                    |> Url.toString { id = 42 }
                    |> Expect.equal "/42/part"
        , test "Can append two variable integers" <|
            \_ ->
                (Url.root |> Url.append (Url.int .id) |> Url.append (Url.int .otherId))
                    |> Url.toString { id = 42, otherId = 24 }
                    |> Expect.equal "/42/24"
        , test "Can append two variable integers the other way" <|
            \_ ->
                (Url.root |> Url.append (Url.int .otherId) |> Url.append (Url.int .id))
                    |> Url.toString { id = 42, otherId = 24 }
                    |> Expect.equal "/24/42"
        , test "Can append a string" <|
            \_ ->
                (Url.root |> Url.append (Url.string .name))
                    |> Url.toString { name = "everything" }
                    |> Expect.equal "/everything"
        , test "Can append a bool" <|
            \_ ->
                (Url.root |> Url.append (Url.bool .show))
                    |> Url.toString { show = True }
                    |> Expect.equal "/true"
        , test "Infix append works" <|
            \_ ->
                (Url.root </> Url.s "part" </> Url.int .id)
                    |> Url.toString { id = 42 }
                    |> Expect.equal "/part/42"
        , test "Infix toString works" <|
            \_ ->
                (Url.root </> Url.s "part" </> Url.int .id @ { id = 42 })
                    |> Expect.equal "/part/42"
        , test "Can use custom segment" <|
            \_ ->
                let
                    idsSegment =
                        Url.custom (.ids >> List.map String.Extra.fromInt >> String.join ";")
                in
                    (Url.root |> Url.append idsSegment)
                        |> Url.toString { ids = [ 1, 2, 3 ] }
                        |> Expect.equal "/1;2;3"
        , test "Can append parameter" <|
            \_ ->
                (Url.root |> Url.append (Url.s "part") |> Url.appendParam "id" (Url.int .id))
                    |> Url.toString { id = 42 }
                    |> Expect.equal "/part?id=42"
        , test "Can append two parameters" <|
            \_ ->
                (Url.root |> Url.append (Url.s "part") |> Url.appendParam "id" (Url.int .id) |> Url.appendParam "show" (Url.bool .show))
                    |> Url.toString { id = 42, show = True }
                    |> Expect.equal "/part?id=42&show=true"
        , test "Can have static parts, variables, and parameters" <|
            \_ ->
                (Url.root |> Url.append (Url.s "part") |> Url.append (Url.int .id) |> Url.appendParam "show" (Url.bool .show))
                    |> Url.toString { id = 42, show = True }
                    |> Expect.equal "/part/42?show=true"
        , test "Infix appendParam works" <|
            \_ ->
                (Url.root </> Url.s "part" <?> ( "show", Url.bool .show ))
                    |> Url.toString { show = False }
                    |> Expect.equal "/part?show=false"
        ]
