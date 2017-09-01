module Url
    exposing
        ( Url
        , Segment
        , toString
        , root
        , append
        , s
        , int
        , string
        , custom
        , (</>)
        , (@)
        )

{-| A tiny library for building parameterized URLs. It is intended to be used
with records to give the parameters names and therefore reducing errors.

    userUrl : Url { id : Int }
    userUrl = root </> s "users" </> int .id

    userUrl @ { id = 42 } == "/users/42"


# Types

@docs Url, Segment


# Builders

@docs root, append, s, int, string, custom, (</>)


# Presenting

@docs toString, (@)

-}

import String.Extra as String


{-| A URL parameterized over the type `a`, which is typically a record containing
a field for each parameterized `Segment`.
-}
type Url a
    = Url (List (Segment a))


{-| A parameteried segment of a URL.
-}
type Segment a
    = Segment (a -> String)


{-| Give a string representation of the URL, given a value for the parameter.
-}
toString : a -> Url a -> String
toString p (Url segments) =
    "/"
        ++ (segments
                |> List.map (\(Segment segment) -> segment p)
                |> String.join "/"
           )


{-| The root URL.

    root |> toString () == "/"

-}
root : Url a
root =
    Url []


{-| Append a segment to the URL.
-}
append : Segment a -> Url a -> Url a
append segment (Url segments) =
    Url (segments ++ [ segment ])


{-| An unparameterized (static) segment.

    root |> append (s "users") |> toString () == "/users"

-}
s : String -> Segment a
s str =
    Segment (\_ -> str)


{-| A parameterized (variable) integer segment.

    root
        |> append (s "users")
        |> append (int .id)
        |> toString { id = 42 }
        == "/users/42"

-}
int : (a -> Int) -> Segment a
int extract =
    Segment (\p -> String.fromInt (extract p))


{-| A parameterized string segment.

    root
        |> append (s "say")
        |> append (string .word)
        |> toString { word = "Hello" }
        == "/say/Hello"

-}
string : (a -> String) -> Segment a
string extract =
    Segment extract


{-| Build a custom segment.

    root
        |> append (custom (.ids >> List.map fromInt >> String.join ";"))
        |> toString { ids = [1, 2, 3] }
        == "/1;2;3"

-}
custom : (a -> String) -> Segment a
custom extract =
    Segment extract


{-| Infix version of `append`.
-}
(</>) : Url a -> Segment a -> Url a
(</>) =
    flip append


{-| Infix version of `toString` ("evaluate at").
-}
(@) : Url a -> a -> String
(@) =
    flip toString
