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
        , hash
        , custom
        , appendParam
        , (</>)
        , (<#>)
        , (<?>)
        , (@)
        )

{-| A tiny library for building parameterized URLs. It is intended to be used
with records to give the parameters names and therefore reducing errors.

    userUrl : Url { id : Int, show : Bool }
    userUrl = root </> s "users" </> int .id <?> ("show", bool .show)

    userUrl @ { id = 42, show = True } --> "/users/42?show=true"

    userUrlWithoutInfix : Url { id : Int, show : Bool }
    userUrlWithoutInfix = root
        |> append (s "users")
        |> append (int .id)
        |> appendParam "show" (bool .show)

    userUrlWithoutInfix
        |> Url.toString { id = 42, show = True } --> "/users/42?show=true"


# Types

@docs Url, Part


# Builders

@docs root, append, s, int, string, bool, hash, custom, appendParam, (</>), (<#>), (<?>)


# Presenting

@docs toString, (@)

-}

import Http
import String.Extra as String


{-| A URL parameterized over the type `a`, which is typically a record containing
a field for each parameterized `Part`.
-}
type Url a
    = Url ( List (Part a), List ( String, Part a ) )


{-| A parameterized part (segment or query value) of a URL.
-}
type Part a
    = Part { toString : a -> String, skipUriEncode : Bool }


{-| Give a string representation of the URL, given a value for the parameter.
-}
toString : a -> Url a -> String
toString p (Url ( segments, queries )) =
    let
        path =
            segments
                |> List.map
                    (\(Part { toString, skipUriEncode }) ->
                        if skipUriEncode then
                            toString p
                        else
                            Http.encodeUri (toString p)
                    )
                |> String.join "/"

        query =
            queries
                |> List.map
                    (\( name, Part { toString, skipUriEncode } ) ->
                        Http.encodeUri name
                            ++ "="
                            ++ if skipUriEncode then
                                toString p
                               else
                                Http.encodeUri (toString p)
                    )
                |> String.join "&"
    in
        if String.isEmpty query then
            "/" ++ path
        else
            "/" ++ path ++ "?" ++ query


{-| The root URL.

    root |> Url.toString () --> "/"

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

    root |> append (s "users") |> Url.toString () --> "/users"

-}
s : String -> Part a
s str =
    Part { toString = \_ -> str, skipUriEncode = False }


{-| A parameterized (variable) integer part.

    root
        |> append (s "users")
        |> append (int .id)
        |> Url.toString { id = 42 }
        --> "/users/42"

-}
int : (a -> Int) -> Part a
int extract =
    Part { toString = \p -> String.fromInt (extract p), skipUriEncode = False }


{-| A parameterized string part.

    root
        |> append (s "say")
        |> append (string .word)
        |> Url.toString { word = "Hello" }
        --> "/say/Hello"

-}
string : (a -> String) -> Part a
string extract =
    Part { toString = extract, skipUriEncode = False }


{-| A parameterized boolean part.

    root
        |> append (bool .show)
        |> Url.toString { show = True }
        --> "/true"

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
        Part { toString = \p -> fromBool (extract p), skipUriEncode = False }


{-| A hash-only part.

    root
        |> append (s "base")
        |> append hash
        |> append (s "page")
        |> Url.toString {}
        --> "/base/#/page"

-}
hash : Part a
hash =
    Part { toString = \_ -> "#", skipUriEncode = True }


{-| Build a custom part.

    import String.Extra exposing (fromInt)

    root
        |> append (custom (.ids >> List.map fromInt >> String.join ";"))
        |> Url.toString { ids = [1, 2, 3] }
        --> "/1%3B2%3B3"

-}
custom : (a -> String) -> Part a
custom extract =
    Part { toString = extract, skipUriEncode = False }


{-| Append a query parameter to the URL.

    root
        |> append (s "part")
        |> appendParam "show" (bool .show)
        |> Url.toString { show = True }
        --> "/part?show=true"

-}
appendParam : String -> Part a -> Url a -> Url a
appendParam name param (Url ( segments, queries )) =
    Url ( segments, queries ++ [ ( name, param ) ] )


{-| Infix version of `append`.
-}
(</>) : Url a -> Part a -> Url a
(</>) =
    flip append


{-| Infix operator to inject a hash.

    root </> s "base" <#> s "page" </> int .page
        |> Url.toString { page = 42 }
    --> "/base/#/page/42"

-}
(<#>) : Url a -> Part a -> Url a
(<#>) base next =
    base </> hash </> next


{-| Infix version of `appendParam`
-}
(<?>) : Url a -> ( String, Part a ) -> Url a
(<?>) url ( name, param ) =
    appendParam name param url


{-| Infix version of `Url.toString` ("evaluate at").
-}
(@) : Url a -> a -> String
(@) =
    flip toString
