module Url
    exposing
        ( Url
        , Part
        , toString
        , root
        , append
        , s
        , int
        , string
        , bool
        , custom
        , appendParam
        , (</>)
        , (<?>)
        , (@)
        )

{-| A tiny library for building parameterized URLs. It is intended to be used
with records to give the parameters names and therefore reducing errors.

    userUrl : Url { id : Int, show : Bool }
    userUrl = root </> s "users" </> int .id <?> ("show", bool .show)

    userUrl @ { id = 42, show = True } == "/users/42?show=true"


# Types

@docs Url, Part


# Builders

@docs root, append, appendParam, s, int, string, bool, custom, (</>), (<?>)


# Presenting

@docs toString, (@)

-}

import String.Extra as String


{-| A URL parameterized over the type `a`, which is typically a record containing
a field for each parameterized `Part`.
-}
type Url a
    = Url ( List (Part a), List ( String, Part a ) )


{-| A parameterized part (segment or query value) of a URL.
-}
type Part a
    = Part (a -> String)


{-| Give a string representation of the URL, given a value for the parameter.
-}
toString : a -> Url a -> String
toString p (Url ( segments, queries )) =
    let
        path =
            segments
                |> List.map (\(Part segment) -> segment p)
                |> String.join "/"

        query =
            queries
                |> List.map (\( name, Part query ) -> name ++ "=" ++ query p)
                |> String.join "&"
    in
        if String.isEmpty query then
            "/" ++ path
        else
            "/" ++ path ++ "?" ++ query


{-| The root URL.

    root |> toString () == "/"

-}
root : Url a
root =
    Url ( [], [] )


{-| Append a segment to the URL.
-}
append : Part a -> Url a -> Url a
append segment (Url ( segments, queries )) =
    Url ( segments ++ [ segment ], queries )


{-| An unparameterized (static) part.

    root |> append (s "users") |> toString () == "/users"

-}
s : String -> Part a
s str =
    Part (\_ -> str)


{-| A parameterized (variable) integer part.

    root
        |> append (s "users")
        |> append (int .id)
        |> toString { id = 42 }
        == "/users/42"

-}
int : (a -> Int) -> Part a
int extract =
    Part (\p -> String.fromInt (extract p))


{-| A parameterized string part.

    root
        |> append (s "say")
        |> append (string .word)
        |> toString { word = "Hello" }
        == "/say/Hello"

-}
string : (a -> String) -> Part a
string extract =
    Part extract


{-| A parameterized boolean part.

    root
        |> append (bool .show)
        |> toString { show = True }
        == "/true"

-}
bool : (a -> Bool) -> Part a
bool extract =
    let
        fromBool b =
            if b then
                "true"
            else
                "false"
    in
        Part (\p -> fromBool (extract p))


{-| Build a custom part.

    root
        |> append (custom (.ids >> List.map fromInt >> String.join ";"))
        |> toString { ids = [1, 2, 3] }
        == "/1;2;3"

-}
custom : (a -> String) -> Part a
custom extract =
    Part extract


{-| Append a query parameter to the URL.
-}
appendParam : String -> Part a -> Url a -> Url a
appendParam name param (Url ( segments, queries )) =
    Url ( segments, queries ++ [ ( name, param ) ] )


{-| Infix version of `append`.
-}
(</>) : Url a -> Part a -> Url a
(</>) =
    flip append


{-| Infix version of `appendParam`
-}
(<?>) : Url a -> ( String, Part a ) -> Url a
(<?>) url ( name, param ) =
    appendParam name param url


{-| Infix version of `toString` ("evaluate at").
-}
(@) : Url a -> a -> String
(@) =
    flip toString
