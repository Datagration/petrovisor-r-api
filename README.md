
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Myrconn.PetroVisor.Client

<!-- badges: start -->

[![R-CMD-check](https://github.com/Datagration/petrovisor-r-api/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Datagration/petrovisor-r-api/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This package provides a wrapper for the PetroVisor API, enabling users
to interact with PetroVisor services directly from R. It includes
functionalities for authentication, data retrieval, logging, and more.

## Installation

To install the package using `remotes`, run the following command:

``` r
# Install from GitHub
remotes::install_github("Datagration/petrovisor-r-api")
```

## Example

### Authentication

Authenticate with the PetroVisor API using an API key,
username/password, or refresh token.

``` r
library(Myrconn.PetroVisor.Client)

# Create an instance of the AuthenticationService
auth_service <- AuthenticationService$new()

# Get access token using API key
token <- auth_service$get_access_token(
  key = "your_api_key",
  discovery_url = "https://identity.us1.petrovisor.com"
)

# Get access token using username and password
token <- auth_service$get_access_token(
  username = "user",
  password = "pass",
  discovery_url = "https://identity.us1.petrovisor.com"
)
```

### Service Provider

Use the `ServiceProvider` class to interact with various PetroVisor
services.

``` r
# Create a new instance of the ServiceProvider using a token
sp <- ServiceProvider$new(
  url = "https://identity.us1.petrovisor.com",
  client_token = "your_token_here"
)

# Create a new instance of the ServiceProvider using username and password
sp <- ServiceProvider$new(
  url = "https://identity.us1.petrovisor.com",
  workspace = "your_workspace_name",
  user = "your_user_name",
  password = "your_password"
)

# Load all available log entries
log_entries <- sp$logs$load()

# Load the names of all available entities
entity_names <- sp$items$load_names("Entity")
```

## License

This package is licensed under the MIT License. See the `LICENSE` file
for details.
