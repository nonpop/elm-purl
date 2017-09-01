# elm-purl

A tiny library for building parameterized URLs. It is intended to be used
with records to give the parameters names and therefore reducing errors.

```elm
import Url exposing (Url, (</>), (@), root, s, int)

userUrl : Url { id : Int }
userUrl = root </> s "users" </> int .id

userUrl @ { id = 42 } == "/users/42"
```

[http://package.elm-lang.org/packages/nonpop/elm-purl/latest/Url](Full API documentation.)
