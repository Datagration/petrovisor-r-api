# AuthenticationService

Provides methods for authenticating with the PetroVisor API.

## Details

This class includes methods to obtain access tokens using various
authentication mechanisms such as API keys, user credentials, and
refresh tokens.

## See also

Related classes:

- [ServiceProvider](https://datagration.github.io/petrovisor-r-api/reference/ServiceProvider.md)
  for using tokens with the service provider

- [AuthContext](https://datagration.github.io/petrovisor-r-api/reference/AuthContext.md)
  for managing authentication context

Authentication helpers:

- [`get_auth_context()`](https://datagration.github.io/petrovisor-r-api/reference/get_auth_context.md)
  for accessing the global authentication context

- [`with_auth_context()`](https://datagration.github.io/petrovisor-r-api/reference/with_auth_context.md)
  for executing code with specific authentication

- [`require_authentication()`](https://datagration.github.io/petrovisor-r-api/reference/require_authentication.md)
  for ensuring authentication is present

Vignettes:

- [`vignette("authentication")`](https://datagration.github.io/petrovisor-r-api/articles/authentication.md)
  for comprehensive authentication guide

- [`vignette("getting-started")`](https://datagration.github.io/petrovisor-r-api/articles/getting-started.md)
  for basic setup and usage

## Examples

``` r
if (FALSE) { # \dontrun{
# Create an instance of the AuthenticationService
auth_service <- AuthenticationService$new()

# Get access token using API key
token <- auth_service$get_access_token(
  key = "your_api_key",
  discovery_url = "https://example.com"
)

# Get access token using username and password
token <- auth_service$get_access_token(
  username = "user",
  password = "pass",
  discovery_url = "https://example.com"
)

# Get access token using refresh token
token <- auth_service$get_access_token(
  refresh_token = "your_refresh_token",
  discovery_url = "https://example.com"
)
} # }
```
