module Purl exposing
    ( Url
    , root, s, maybeS, hash
    , int, string, bool, custom
    , maybeInt, maybeString, maybeBool, maybeCustom
    , intQuery, stringQuery, boolQuery, customQuery
    , maybeIntQuery, maybeStringQuery, maybeBoolQuery, maybeCustomQuery
    , toString
    , customRoot
    )

{-| A tiny library for building parameterized URLs. It is intended to be used
with records to give the parameters names and therefore reducing errors.

    userUrl : Url { id : Int, show : Bool }
    userUrl =
        root |> hash |> s "users" |> int .id |> boolQuery "show" .show

    userUrl |> toString { id = 42, show = True } --> "/#/users/42?show=true"


# Types

@docs Url


# Builders

@docs root, s, maybeS, hash, rootString, rootFromParts
@docs int, string, bool, custom
@docs maybeInt, maybeString, maybeBool, maybeCustom
@docs intQuery, stringQuery, boolQuery, customQuery
@docs maybeIntQuery, maybeStringQuery, maybeBoolQuery, maybeCustomQuery


# Presenting

@docs toString

-}

import Url


{-| A URL parameterized over the type `a`, which is typically a record containing
a field for each variable segment and query parameter.
-}
type Url a
    = Url
        { prefix : String
        , path : List (Part a)
        , query : List ( String, Part a )
        , hasHash : Bool
        }


{-| A parameterized part (segment or query parameter) of a URL.
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
toString p (Url { prefix, path, query }) =
    let
        pathStr =
            String.join "/" (List.filterMap (partToString p) path)

        queryStr =
            String.join "&" (List.filterMap (queryPartToString p) query)
    in
    prefix
        ++ pathStr
        ++ (if String.isEmpty queryStr then
                ""

            else
                "?" ++ queryStr
           )


{-| A custom root URL.

    customRoot "http://example.com:8080/" |> toString () --> "http://example.com:8080/"

If the string has no trailing slash, one is added:

    customRoot "http://example.com:8080" |> toString () --> "http://example.com:8080/"

-}
customRoot : String -> Url a
customRoot prefix =
    let
        withTrailingSlash =
            if not (String.endsWith "/" prefix) then
                prefix ++ "/"

            else
                prefix
    in
    Url { prefix = withTrailingSlash, path = [], query = [], hasHash = False }


{-| The root URL.

    root |> toString () --> "/"

-}
root : Url a
root =
    customRoot ""


{-| Append a custom segment with a Maybe value; it is omitted when the value is
Nothing.

    url : Url { ids : Maybe (List Int) }
    url =
        root
            |> maybeCustom (.ids >> Maybe.map (List.map String.fromInt >> String.join ";"))

    url |> toString { ids = Just [ 1, 2, 3 ] } --> "/1%3B2%3B3"
    url |> toString { ids = Nothing } --> "/"str

-}
maybeCustom : (a -> Maybe String) -> Url a -> Url a
maybeCustom stringify (Url url) =
    Url { url | path = url.path ++ [ Part { stringify = stringify, skipUriEncode = False } ] }


{-| Append a custom query value with a Maybe value; it is omitted when the value is
Nothing.

    url : Url { ids : Maybe (List Int) }
    url =
        root
            |> maybeCustomQuery "ids" (.ids >> Maybe.map (List.map String.fromInt >> String.join ";"))

    url |> toString { ids = Just [ 1, 2, 3 ] } --> "/?ids=1%3B2%3B3"
    url |> toString { ids = Nothing } --> "/"

-}
maybeCustomQuery : String -> (a -> Maybe String) -> Url a -> Url a
maybeCustomQuery name stringify (Url url) =
    Url { url | query = url.query ++ [ ( name, Part { stringify = stringify, skipUriEncode = False } ) ] }


{-| Append a custom segment.

    root
        |> custom (.ids >> List.map String.fromInt >> String.join ";")
        |> toString { ids = [1, 2, 3] }
        --> "/1%3B2%3B3"

-}
custom : (a -> String) -> Url a -> Url a
custom stringify =
    maybeCustom (stringify >> Just)


{-| Append a custom query value.

    root
        |> customQuery "ids" (.ids >> List.map String.fromInt >> String.join ";")
        |> toString { ids = [1, 2, 3] }
        --> "/?ids=1%3B2%3B3"

-}
customQuery : String -> (a -> String) -> Url a -> Url a
customQuery name stringify =
    maybeCustomQuery name (stringify >> Just)


{-| Append a custom segment without URL-encoding and with a Maybe value; it is
omitted when the value is Nothing.

    root
        |> custom (.ids >> String.join ";" >> Just)
        |> toString { ids = [ "1", "2 3", "4" ] }
        == "/1;2 3;4"

-}
maybeCustomRaw : (a -> Maybe String) -> Url a -> Url a
maybeCustomRaw extract (Url url) =
    Url { url | path = url.path ++ [ Part { stringify = extract, skipUriEncode = True } ] }


{-| Append a custom segment without URL-encoding.

    root
        |> customRaw (.ids >> String.join ";")
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
        --> "/users/1"

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


{-| Append an integer segment with a Maybe value; it is omitted
when the value is Nothing.

    url : Url { id : Maybe Int }
    url =
        root
            |> s "users"
            |> maybeInt .id
            |> s "images"

    toString { id = Just 42 } url --> "/users/42/images"
    toString { id = Nothing } url --> "/users/images"

-}
maybeInt : (a -> Maybe Int) -> Url a -> Url a
maybeInt extract =
    maybeCustom (extract >> Maybe.map String.fromInt)


{-| Append an integer segment.

    root
        |> s "users"
        |> int .id
        |> toString { id = 42 }
        --> "/users/42"

-}
int : (a -> Int) -> Url a -> Url a
int extract =
    custom (extract >> String.fromInt)


{-| Append an integer parameter with a Maybe value; it is omitted
when the value is Nothing.

    url : Url { id : Maybe Int }
    url =
        root
            |> s "users"
            |> s "images"
            |> maybeIntQuery "id" .id

    toString { id = Just 42 } url --> "/users/images?id=42"
    toString { id = Nothing } url --> "/users/images"

-}
maybeIntQuery : String -> (a -> Maybe Int) -> Url a -> Url a
maybeIntQuery name extract =
    maybeCustomQuery name (extract >> Maybe.map String.fromInt)


{-| Append an integer segment.

    root
        |> s "users"
        |> intQuery "id" .id
        |> toString { id = 42 }
        --> "/users?id=42"

-}
intQuery : String -> (a -> Int) -> Url a -> Url a
intQuery name extract =
    customQuery name (extract >> String.fromInt)


{-| Append a string segment with a Maybe value; it is omitted when the value
is Nothing.

    url : Url { word : Maybe String }
    url =
        root
            |> s "say"
            |> maybeString .word
            |> s "world"

    toString { word = Just "Hello" } url --> "/say/Hello/world"
    toString { word = Nothing } url --> "/say/world"

-}
maybeString : (a -> Maybe String) -> Url a -> Url a
maybeString =
    maybeCustom


{-| Append a string segment.

    root
        |> s "say"
        |> string .word
        |> toString { word = "Hello" }
        --> "/say/Hello"

-}
string : (a -> String) -> Url a -> Url a
string =
    custom


{-| Append a string query value with a Maybe value; it is omitted when the value
is Nothing.

    url : Url { word : Maybe String }
    url =
        root
            |> s "say"
            |> maybeStringQuery "word" .word

    toString { word = Just "Hello" } url --> "/say?word=Hello"
    toString { word = Nothing } url --> "/say"

-}
maybeStringQuery : String -> (a -> Maybe String) -> Url a -> Url a
maybeStringQuery =
    maybeCustomQuery


{-| Append a string query value.

    root
        |> s "say"
        |> stringQuery "word" .word
        |> toString { word = "Hello" }
        --> "/say?word=Hello"

-}
stringQuery : String -> (a -> String) -> Url a -> Url a
stringQuery =
    customQuery


boolToString : Bool -> String
boolToString b =
    if b then
        "true"

    else
        "false"


{-| Append a boolean segment with a Maybe value; it is omitted when the
value is Nothing.

    url : Url { show : Maybe Bool }
    url =
        root
            |> maybeBool .show

    toString { show = Just True } url --> "/true"
    toString { show = Nothing } url --> "/"

-}
maybeBool : (a -> Maybe Bool) -> Url a -> Url a
maybeBool extract =
    maybeCustom (extract >> Maybe.map boolToString)


{-| Append a boolean segment.

    root
        |> bool .show
        |> toString { show = True }
        --> "/true"

-}
bool : (a -> Bool) -> Url a -> Url a
bool extract =
    custom (extract >> boolToString)


{-| Append a boolean query value with a Maybe value; it is omitted when the
value is Nothing.

    url : Url { show : Maybe Bool }
    url =
        root
            |> maybeBoolQuery "show" .show

    toString { show = Just True } url --> "/?show=true"
    toString { show = Nothing } url --> "/"

-}
maybeBoolQuery : String -> (a -> Maybe Bool) -> Url a -> Url a
maybeBoolQuery name extract =
    maybeCustomQuery name (extract >> Maybe.map boolToString)


{-| Append a boolean query value.

    root
        |> boolQuery "show" .show
        |> toString { show = True }
        --> "/?show=true"

-}
boolQuery : String -> (a -> Bool) -> Url a -> Url a
boolQuery name extract =
    customQuery name (extract >> boolToString)


{-| Append a hash segment.

    root
        |> s "base"
        |> hash
        |> s "page"
        |> toString {}
        --> "/base/#/page"

NOTE: Only the first hash is unencoded:

    root |> hash |> hash |> toString {} --> "/#/%23"

-}
hash : Url a -> Url a
hash (Url url) =
    if url.hasHash then
        custom (always "#") (Url url)

    else
        customRaw (always "#") (Url { url | hasHash = True })
