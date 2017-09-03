# elm-purl

A tiny library for building parameterized URLs. It is intended to be used
with records to give the parameters names and therefore reducing errors.

```elm
import Url exposing (Url, (</>), (<?>), (@), root, s, int, bool)

userUrl : Url { id : Int }
userUrl = root </> s "users" </> int .id <?> ("show", bool .show)

userUrl @ { id = 42, show = True } == "/users/42?show=true"
```

[Full API documentation.](http://package.elm-lang.org/packages/nonpop/elm-purl/latest/Url)
