module Url
    exposing
        ( Url
        , toString
        , root
        , append
        , s
        , int
        , string
        , (</>)
        , (@)
        )

import String.Extra as String


type Part a
    = Part (a -> String)


type Url a
    = Url (List (Part a))


toString : a -> Url a -> String
toString p (Url parts) =
    "/"
        ++ (parts
                |> List.map (\(Part part) -> part p)
                |> String.join "/"
           )


root : Url a
root =
    Url []


append : Part a -> Url a -> Url a
append part (Url parts) =
    Url (parts ++ [ part ])


s : String -> Part a
s str =
    Part (\_ -> str)


int : (a -> Int) -> Part a
int extract =
    Part (\p -> String.fromInt (extract p))


string : (a -> String) -> Part a
string extract =
    Part extract


(</>) : Url a -> Part a -> Url a
(</>) =
    flip append


(@) : Url a -> a -> String
(@) =
    flip toString
