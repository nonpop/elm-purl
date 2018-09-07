# elm-purl

A tiny library for building parameterized URLs. It is intended to be used
with records to give the parameters names and therefore reducing errors.

```elm
userUrl : Url { id : Int, show : Bool }
userUrl =
    root |> hash |> s "users" |> int .id |> boolQuery "show" .show

userUrl |> toString { id = 42, show = True } --> "/#/users/42?show=true"
```

[Full API documentation.](http://package.elm-lang.org/packages/nonpop/elm-purl/latest/Purl)
