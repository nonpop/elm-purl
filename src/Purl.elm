module Purl exposing
    ( Url
    , root, s, maybeS, hash
    , int, maybeInt, string, maybeString, bool, maybeBool, custom, maybeCustom
    , intParam, maybeIntParam, boolParam, maybeBoolParam, customParam, maybeCustomParam
    , toString
    )

{-| A tiny library for building parameterized URLs. It is intended to be used
with records to give the parameters names and therefore reducing errors.

    userUrl : Url { id : Int, show : Bool }
    userUrl = root
        |> s "users"
        |> int .id
        |> boolParam "show" .show

    userUrl
        |> toString { id = 42, show = True } --> "/users/42?show=true"


# Types

@docs Url


# Builders

@docs root, s, maybeS, hash
@docs int, maybeInt, string, maybeString, bool, maybeBool, custom, maybeCustom
@docs intParam, maybeIntParam, stringParam, maybeStringParam, boolParam, maybeBoolParam, customParam, maybeCustomParam


# Presenting

@docs toString

-}

import Url


{-| A URL parameterized over the type `a`, which is typically a record containing
a field for each parameterized segment.
-}
type Url a
    = Url
        { path : List (Part a)
        , query : List ( String, Part a )
        }


{-| A parameterized part (segment or query value) of a URL.
-}
type Part a
    = Part
        { stringify : a -> Maybe String
        , skipUriEncode : Bool
        }


{-| URL-encode the given String unless skipEncode is True, in which case do nothing.

    encodeUriPart False "a b" == "a%20b"

    encodeUriPart True "a b" == "a b"

-}
encodeUriPart : Bool -> String -> String
encodeUriPart skipEncode url =
    if skipEncode then
        url

    else
        Url.percentEncode url


partToString : a -> Part a -> Maybe String
partToString p (Part { stringify, skipUriEncode }) =
    stringify p |> Maybe.map (encodeUriPart skipUriEncode)


queryPartToString : a -> ( String, Part a ) -> Maybe String
queryPartToString p ( name, part ) =
    partToString p part |> Maybe.map (\str -> name ++ "=" ++ str)


{-| Give a string representation of the URL, given a value for the parameter.
-}
toString : a -> Url a -> String
toString p (Url { path, query }) =
    let
        pathStr =
            String.join "/" (List.filterMap (partToString p) path)

        queryStr =
            String.join "&" (List.filterMap (queryPartToString p) query)
    in
    "/"
        ++ pathStr
        ++ (if String.isEmpty queryStr then
                ""

            else
                "?" ++ queryStr
           )


{-| The root URL.

    root |> toString () --> "/"

-}
root : Url a
root =
    Url { path = [], query = [] }


{-| Append a custom segment with a Maybe value; it is omitted when the value is
Nothing.

    root
        |> custom (.ids >> List.map String.fromInt >> String.join ";" >> Just)
        |> toString { ids = [ 1, 2, 3 ] }
        == "/1%3B2%3B3"

-}
maybeCustom : (a -> Maybe String) -> Url a -> Url a
maybeCustom stringify (Url { path, query }) =
    Url
        { path = path ++ [ Part { stringify = stringify, skipUriEncode = False } ]
        , query = query
        }


{-| Append a custom parameter with a Maybe value; it is omitted when the value is
Nothing.

    root
        |> maybeCustom "ids" (.ids >> List.map String.fromInt >> String.join ";" >> Just)
        |> toString { ids = [ 1, 2, 3 ] }
        == "/?ids=1%3B2%3B3"

-}
maybeCustomParam : String -> (a -> Maybe String) -> Url a -> Url a
maybeCustomParam name stringify (Url { path, query }) =
    Url
        { path = path
        , query = query ++ [ ( name, Part { stringify = stringify, skipUriEncode = False } ) ]
        }


{-| Append a custom segment.

    root
        |> custom (.ids >> List.map String.fromInt >> String.join ";")
        |> toString { ids = [1, 2, 3] }
        --> "/1%3B2%3B3"

-}
custom : (a -> String) -> Url a -> Url a
custom stringify =
    maybeCustom (stringify >> Just)


{-| Append a custom parameter.

    root
        |> customParam "ids" (.ids >> List.map String.fromInt >> String.join ";")
        |> toString { ids = [1, 2, 3] }
        --> "/?ids=1%3B2%3B3"

-}
customParam : String -> (a -> String) -> Url a -> Url a
customParam name stringify =
    maybeCustomParam name (stringify >> Just)


{-| Append a custom segment without URL-encoding and with a Maybe value; it is
omitted when the value is Nothing.

    root
        |> custom (.ids >> String.join ";" >> Just)
        |> toString { ids = [ "1", "2 3", "4" ] }
        == "/1;2 3;4"

-}
maybeCustomRaw : (a -> Maybe String) -> Url a -> Url a
maybeCustomRaw extract (Url { path, query }) =
    Url
        { path = path ++ [ Part { stringify = extract, skipUriEncode = True } ]
        , query = query
        }


{-| Append a custom segment without URL-encoding.

    root
        |> custom (.ids >> String.join ";")
        |> toString { ids = [ "1", "2 3", "4" ] }
        == "/1;2 3;4"

-}
customRaw : (a -> String) -> Url a -> Url a
customRaw extract =
    maybeCustomRaw (extract >> Just)


{-| Append an unparameterized (static) segment with a Maybe value; it is omitted when the
value is Nothing.

    root
        |> maybeS (Just "users")
        |> maybeS Nothing
        |> maybeS (Just "1")
        |> toString ()
        == "/users/1"

-}
maybeS : Maybe String -> Url a -> Url a
maybeS segment =
    maybeCustom (always segment)


{-| Append an unparameterized (static) segment.

    root |> s "users" |> toString () --> "/users"

-}
s : String -> Url a -> Url a
s segment =
    custom (always segment)


{-| Append a parameterized (variable) integer segment with a Maybe value; it is omitted
when the value is Nothing.

    url =
        root
            |> s "users"
            |> maybeInt .id
            |> s "images"

    toString { id = Just 42 } url == "/users/42/images"
    toString { id = Nothing } url == "/users/images"

-}
maybeInt : (a -> Maybe Int) -> Url a -> Url a
maybeInt extract =
    maybeCustom (extract >> Maybe.map String.fromInt)


{-| Append a parameterized (variable) integer segment.

    root
        |> s "users"
        |> int .id
        |> toString { id = 42 }
        --> "/users/42"

-}
int : (a -> Int) -> Url a -> Url a
int extract =
    custom (extract >> String.fromInt)


{-| Append a parameterized (variable) integer parameter with a Maybe value; it is omitted
when the value is Nothing.

    url =
        root
            |> s "users"
            |> s "images"
            |> maybeIntParam "id" .id

    toString { id = Just 42 } url == "/users/images?id=42"
    toString { id = Nothing } url == "/users/images"

-}
maybeIntParam : String -> (a -> Maybe Int) -> Url a -> Url a
maybeIntParam name extract =
    maybeCustomParam name (extract >> Maybe.map String.fromInt)


{-| Append a parameterized (variable) integer segment.

    root
        |> s "users"
        |> intParam "id" .id
        |> toString { id = 42 }
        --> "/users?id=42"

-}
intParam : String -> (a -> Int) -> Url a -> Url a
intParam name extract =
    customParam name (extract >> String.fromInt)


{-| Append a parameterized string segment with a Maybe value; it is omitted when the value
is Nothing.

    url =
        root
            |> s "say"
            |> maybeString .word
            |> s "world"

    toString { word = Just "Hello" } url == "/say/Hello/world"
    toString { word = Nothing } url == "/say/world"

-}
maybeString : (a -> Maybe String) -> Url a -> Url a
maybeString =
    maybeCustom


{-| Append a parameterized string segment.

    root
        |> s "say"
        |> string .word
        |> toString { word = "Hello" }
        --> "/say/Hello"

-}
string : (a -> String) -> Url a -> Url a
string =
    custom


boolToString : Bool -> String
boolToString b =
    if b then
        "true"

    else
        "false"


{-| Append a parameterized boolean segment with a Maybe value; it is omitted when the
value is Nothing.

    url =
        root
            |> maybeBool .show

    toString { show = Just True } url == "/true"
    toString { show = Nothing } url == "/"

-}
maybeBool : (a -> Maybe Bool) -> Url a -> Url a
maybeBool extract =
    maybeCustom (extract >> Maybe.map boolToString)


{-| Append a parameterized boolean segment.

    root
        |> bool .show
        |> toString { show = True }
        --> "/true"

-}
bool : (a -> Bool) -> Url a -> Url a
bool extract =
    custom (extract >> boolToString)


{-| Append a parameterized boolean parameter with a Maybe value; it is omitted when the
value is Nothing.

    url =
        root
            |> maybeBool "show" .show

    toString { show = Just True } url == "/?show=true"
    toString { show = Nothing } url == "/"

-}
maybeBoolParam : String -> (a -> Maybe Bool) -> Url a -> Url a
maybeBoolParam name extract =
    maybeCustomParam name (extract >> Maybe.map boolToString)


{-| Append a parameterized boolean parameter.

    root
        |> boolParam "show" .show
        |> toString { show = True }
        --> "/?show=true"

-}
boolParam : String -> (a -> Bool) -> Url a -> Url a
boolParam name extract =
    customParam name (extract >> boolToString)


{-| Append a hash-only segment.

    root
        |> s "base"
        |> hash
        |> s "page"
        |> toString {}
        --> "/base/#/page"

-}
hash : Url a -> Url a
hash =
    customRaw (always "#")
