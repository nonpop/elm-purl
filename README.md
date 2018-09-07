# elm-purl

A tiny library for building parameterized URLs. It is intended to be used
with records to give the parameters names and therefore reducing errors.

```elm
userUrl : Url { id : Int, show : Bool }
userUrl =
    root |> hash |> s "users" |> int .id |> boolQuery "show" .show

userUrl |> toString { id = 42, show = True }
    --> "/#/users/42?show=true"
```

[Full API documentation.](http://package.elm-lang.org/packages/nonpop/elm-purl/latest/Purl)

## Migrating from 2.2.0 to 3.0.0

Version 3.0.0 removed the infix operators (`</>`, `<?>`, `<#>` and `@`) because
they are not allowed in Elm 0.19. Here's how the API has changed:
- First, the module has been renamed from `Url` to `Purl` so it won't clash with
  Elm 0.19's `Url` module.

- The `</>` operator is essentially replaced with `|>`
    ```elm
    root </> s "users" </> int .id
    -- becomes
    root |> s "users" |> int .id
    ```

- The `<?>` operator is `|>` plus a `Query`-postfix
    ```elm
    root </> s "users" <?> ("show", bool .show)
    -- becomes
    root |> s "users" |> boolQuery "show" .show
    ```

- The `<#>` and `@` operators were just removed; use the `hash` and `toString` functions
    ```elm
    root <#> s "users" </> int .id @ { id = 42 }
    -- becomes
    root |> hash |> s "users" |> int .id
        |> toString { id = 42 }
    ```