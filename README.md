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

## Known issues

In Elm precedences for infix operators are applied globally, so if you want to 
use two packages which define the same operators but with different precedences
(eg. this package and [evancz/url-parser](http://package.elm-lang.org/packages/evancz/url-parser/2.0.1)),
you can only use the infix operators of one package freely. Trying to use both
packages' operators will lead to compilation problems. [This issue](https://github.com/nonpop/elm-purl/issues/2)
contains links to more resources.

A workaround is to use the `append` etc. functions instead of their infix versions. Here is the above
example in this style:

```elm
import Url exposing (Url, append, appendParam, root, s, int, bool)

userUrl : Url { id : Int }
userUrl = root 
  |> append (s "users") 
  |> append (int .id)
  |> appendParam "show" (bool .show)

userUrl 
  |> Url.toString { id = 42, show = True } == "/users/42?show=true"
```

Another workaround is to use explicit parentheses:
```elm
userUrl = ((root </> s "users") </> int .id) <?> ("show", bool .show)
```
