# Require Authentication

Helper function that throws an error if not authenticated

## Usage

``` r
require_authentication(message = "This operation requires authentication")
```

## Arguments

- message:

  Optional custom error message

## Details

This is a convenience function that can be called at the start of
functions that require authentication to ensure the user is properly
authenticated before proceeding.

## Examples

``` r
if (FALSE) { # \dontrun{
# At the start of a function that requires auth
some_protected_function <- function() {
  require_authentication()

  # Proceed with authenticated operations...
}
} # }
```
