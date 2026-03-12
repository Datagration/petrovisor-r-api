# Get Authentication Context

Get the singleton AuthContext instance

## Usage

``` r
get_auth_context()
```

## Value

AuthContext singleton instance

## Details

This function provides convenient access to the singleton AuthContext
instance. It's the recommended way to access authentication context
throughout the application.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get authentication context
auth <- get_auth_context()

# Check if authenticated
if (auth$is_authenticated()) {
  user <- auth$get_user()
  workspace <- auth$get_workspace()
}
} # }
```
