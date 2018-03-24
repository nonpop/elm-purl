module Url
    exposing
        ( Url
        , Part
        , toString
        , root
        , append
        , s
        , maybeS
        , int
        , maybeInt
        , string
        , maybeString
        , bool
        , maybeBool
        , hash
        , custom
        , maybeCustom
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

@docs root, append, s, maybeS, int, maybeInt, string, maybeString, bool, maybeBool, hash, custom, maybeCustom, appendParam, (</>), (<#>), (<?>)


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
    = Part { toString : a -> Maybe String, skipUriEncode : Bool }


{-| URL-encode the given String if the Bool is True; otherwise, do nothing to
it.

    encodeUriPart True "a b" == "a%20b"
    encodeUriPart False "a b" == "a b"

-}
encodeUriPart : Bool -> String -> String
encodeUriPart encode =
    if encode then
        identity
    else
        Http.encodeUri


{-| Extract the given Part from the given record.

    extractPart { a = "b c" } (Part { toString = .a >> Just, skipUriEncode = False})
        == Just "b%20c"
    extractPart { a = "b c" } (Part { toString = .a >> Just, skipUriEncode = True})
        == Just "b c"
    extractPart
        { a = Just "b c" }
        (Part { toString = .a, skipUriEncode = False})
        == Just "b%20c"
    extractPart
        { a = Nothing }
        (Part { toString = .a, skipUriEncode = False})
        == Nothing

-}
extractPart : a -> Part a -> Maybe String
extractPart p (Part { toString, skipUriEncode }) =
      Maybe.map (encodeUriPart skipUriEncode) (toString p)


{-| Extract from a record and format a Part for a path.
-}
pathPartToString : a -> Part a -> Maybe String
pathPartToString =
    extractPart


{-| Extract from a record and format a Part for a query.

    queryPartToString
        ("a b", { c = "d e" } )
        (Part { toString = .a >> Just, skipUriEncode = False})
        == Just "a%20b=c%20d"
    queryPartToString
        ("a b", { c = "d e" } )
        (Part { toString = .a >> Just, skipUriEncode = True})
        == Just "a%20b=c d"
    queryPartToString
        ("a b", { c = Just "d e" } )
        (Part { toString = .a, skipUriEncode = False})
        == Just "a%20b=c%20d"
    queryPartToString
        ("a b", { c = Nothing } )
        (Part { toString = .a, skipUriEncode = False})
        == Nothing

-}
queryPartToString : a -> ( String, Part a ) -> Maybe String
queryPartToString p ( name, part ) =
    Maybe.map ((++) (Http.encodeUri name ++ "=")) (extractPart p part)


{-| Reads all of the Parts from the record and concatenates them with the
supplied separator.
-}
readAndCombineParts : (a -> b -> Maybe String) -> a -> String -> List b -> String
readAndCombineParts mapper p separator =
    String.join separator << List.filterMap (mapper p)


{-| Give a string representation of the URL, given a value for the parameter.
-}
toString : a -> Url a -> String
toString p (Url ( segments, queries )) =
    let
        path =
            "/" ++ (readAndCombineParts pathPartToString p "/" segments)

        query =
            readAndCombineParts queryPartToString p "&" queries
    in
        if String.isEmpty query then
            path
        else
            path ++ "?" ++ query


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


{-| When some of the Part-constructing functions accepting a function with
co-domain of Maybe b is supplied (such as maybeCustom and maybeInt), then the
return value is an equivalent function sans Maybe.

    fromMaybe maybeCustom == custom
    fromMaybe maybeInt == int
-}
fromMaybe : ((a -> Maybe b) -> Part a) -> (a -> b) -> Part a
fromMaybe f =
    f << ((<<) Just)


{-| Build a custom part with a Maybe value; it is omitted when the value is
Nothing.

    import String.Extra exposing (fromInt)

    root
        |> append (custom (.ids >> List.map fromInt >> String.join ";" >> Just))
        |> Url.toString { ids = [1, 2, 3] }
        == "/1%3B2%3B3"

-}
maybeCustom : (a -> Maybe String) -> Part a
maybeCustom extract =
    Part { toString = extract, skipUriEncode = False }


{-| Build a custom part.

    import String.Extra exposing (fromInt)

    root
        |> append (custom (.ids >> List.map fromInt >> String.join ";"))
        |> Url.toString { ids = [1, 2, 3] }
        --> "/1%3B2%3B3"

-}
custom : (a -> String) -> Part a
custom =
    fromMaybe maybeCustom


{-| Build a custom part without URL-encoding and with a Maybe value; it is
omitted when the value is Nothing.

    root
        |> append (custom (.ids >> String.join ";" >> Just))
        |> Url.toString { ids = ["1", "2 3", "4"] }
        == "/1;2 3;4"

-}
maybeCustomRaw : (a -> Maybe String) -> Part a
maybeCustomRaw extract =
    Part { toString = extract, skipUriEncode = True }


{-| Build a custom part without URL-encoding.

    root
        |> append (custom (.ids >> String.join ";"))
        |> Url.toString { ids = ["1", "2 3", "4"] }
        == "/1;2 3;4"

-}
customRaw : (a -> String) -> Part a
customRaw =
    fromMaybe maybeCustomRaw


{-| An unparameterized (static) part with a Maybe value; it is omitted when the
value is Nothing.

    root
        |> append (maybeS (Just "users"))
        |> append (maybeS Nothing)
        |> append (maybeS (Just "1"))
        |> Url.toString () == "/users/1"

-}
maybeS : Maybe String -> Part a
maybeS =
    maybeCustom << always


{-| An unparameterized (static) part.

    root |> append (s "users") |> Url.toString () --> "/users"

-}
s : String -> Part a
s =
    custom << always


{-| A parameterized (variable) integer part with a Maybe value; it is omitted
when the value is Nothing.

    url =
        root
            |> append (s "users")
            |> append (maybeInt .id)
            |> append (s "images")

    Url.toString { id = Just 42 } url == "/users/42/images"
    Url.toString { id = Nothing } url == "/users/images"

-}
maybeInt : (a -> Maybe Int) -> Part a
maybeInt extract =
    maybeCustom (Maybe.map String.fromInt << extract)


{-| A parameterized (variable) integer part.

    root
        |> append (s "users")
        |> append (int .id)
        |> Url.toString { id = 42 }
        --> "/users/42"

-}
int : (a -> Int) -> Part a
int =
    fromMaybe maybeInt


{-| A parameterized string part with a Maybe value; it is omitted when the value
is Nothing.

    url =
        root
            |> append (s "say")
            |> append (maybeString .word)
            |> append (s "world")

    Url.toString { word = Just "Hello" } url == "/say/Hello/world"
    Url.toString { word = Nothing } url == "/say/world"

-}
maybeString : (a -> Maybe String) -> Part a
maybeString =
    maybeCustom


{-| A parameterized string part.

    root
        |> append (s "say")
        |> append (string .word)
        |> Url.toString { word = "Hello" }
        --> "/say/Hello"

-}
string : (a -> String) -> Part a
string =
    custom


{-| A parameterized boolean part with a Maybe value; it is omitted when the
value is Nothing.

    url =
        root
            |> append (maybeBool .show)

    Url.toString { show = Just True } url == "/true"
    Url.toString { show = Nothing } url == "/"

-}
maybeBool : (a -> Maybe Bool) -> Part a
maybeBool extract =
    let
        fromBool b =
            if b then
                "true"
            else
                "false"
    in
        maybeCustom (Maybe.map fromBool << extract)


{-| A parameterized boolean part.

    root
        |> append (bool .show)
        |> Url.toString { show = True }
        --> "/true"

-}
bool : (a -> Bool) -> Part a
bool =
    fromMaybe maybeBool


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
    customRaw (always "#")


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
