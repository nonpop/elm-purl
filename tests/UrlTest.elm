module UrlTest exposing (..)

import Url exposing ((</>), (@))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
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
                (Url.root |> Url.append (Url.s "part") |> Url.append (Url.int .id))
                    |> Url.toString { id = 42 }
                    |> Expect.equal "/part/42"
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
        , test "Infix append works" <|
            \_ ->
                (Url.root </> Url.s "part" </> Url.int .id)
                    |> Url.toString { id = 42 }
                    |> Expect.equal "/part/42"
        , test "Infix toString works" <|
            \_ ->
                (Url.root </> Url.s "part" </> Url.int .id @ { id = 42 })
                    |> Expect.equal "/part/42"
        ]
