# Authentication Guide

``` r
library(Myrconn.PetroVisor.Client)
```

## Overview

Authentication is the first step in using the
`Myrconn.PetroVisor.Client` package. This guide covers all
authentication methods, managing authentication contexts, and best
practices for secure API access.

## Authentication Service

The `AuthenticationService` class handles all authentication operations.
It provides methods to obtain access tokens using various authentication
mechanisms.

### Creating an Authentication Service Instance

``` r
auth_service <- AuthenticationService$new()
```

## Authentication Methods

The package supports three primary authentication methods:

### 1. API Key Authentication

API keys are the recommended method for programmatic access. They are
base64-encoded credentials that can be generated from the PetroVisor web
interface.

``` r
# Authenticate using an API key
token_response <- auth_service$get_access_token(
  key = "your_api_key_here",
  discovery_url = "https://identity.us1.petrovisor.com"
)

# The response contains:
# - access_token: The token to use for API requests
# - token_type: Usually "Bearer"
# - expires_in: Token lifetime in seconds
# - refresh_token: Token for refreshing access

access_token <- token_response$access_token
refresh_token <- token_response$refresh_token
```

#### How API Keys Work

API keys are base64-encoded strings containing username and password
information. The `AuthenticationService` automatically:

1.  Decodes the API key
2.  Extracts the credentials
3.  Obtains an access token from the identity server
4.  Returns the token response

### 2. Username and Password Authentication

If you don’t have an API key, you can authenticate directly with
credentials:

``` r
token_response <- auth_service$get_access_token(
  username = "your_username",
  password = "your_password",
  discovery_url = "https://identity.us1.petrovisor.com"
)

access_token <- token_response$access_token
```

**Security Note**: Avoid hardcoding credentials in scripts. Use
environment variables or secure configuration files instead.

``` r
# Store credentials in environment variables
username <- Sys.getenv("PETROVISOR_USER")
password <- Sys.getenv("PETROVISOR_PASSWORD")
discovery_url <- Sys.getenv("PETROVISOR_URL")

token_response <- auth_service$get_access_token(
  username = username,
  password = password,
  discovery_url = discovery_url
)
```

### 3. Refresh Token Authentication

Access tokens expire after a certain period (typically 1 hour). Use
refresh tokens to obtain new access tokens without re-entering
credentials:

``` r
# Use a refresh token to get a new access token
token_response <- auth_service$get_access_token(
  refresh_token = refresh_token,
  discovery_url = "https://identity.us1.petrovisor.com"
)

# Update your access token
access_token <- token_response$access_token

# You'll also get a new refresh token
refresh_token <- token_response$refresh_token
```

## Service Provider Authentication

The `ServiceProvider` class can handle authentication automatically,
eliminating the need to manually manage tokens in most cases.

**Note**: See
[`?ServiceProvider`](https://datagration.github.io/petrovisor-r-api/reference/ServiceProvider.md)
for complete documentation of initialization parameters.

### Direct Token Authentication

Pass a pre-obtained token to the service provider:

``` r
sp <- ServiceProvider$new(
  url = "https://identity.us1.petrovisor.com",
  workspace = "MyWorkspace",
  client_token = access_token
)
```

### Automatic Authentication

Let the service provider handle token retrieval:

``` r
sp <- ServiceProvider$new(
  url = "https://identity.us1.petrovisor.com",
  workspace = "MyWorkspace",
  user = "your_username",
  password = "your_password"
)

# The service provider automatically:
# 1. Contacts the identity server
# 2. Obtains an access token
# 3. Configures all services with the token
```

## Authentication Context

The package includes a global authentication context that stores
authentication details and can be used across different parts of your
code.

### Using the Authentication Context

The authentication context is automatically set when you create a
`ServiceProvider`:

``` r
sp <- ServiceProvider$new(
  url = "https://identity.us1.petrovisor.com",
  workspace = "MyWorkspace",
  user = "your_username",
  password = "your_password"
)

# Get the authentication context
auth_context <- get_auth_context()

# The context contains:
# - token: The current access token
# - token_type: Token type (usually "Bearer")
# - workspace_data_url: Full URL for workspace data operations
# - user: Current username
# - workspace: Current workspace name
```

### Manual Context Management

You can also manually manage the authentication context:

``` r
# Get the global authentication context
auth_context <- get_auth_context()

# Set authentication details
auth_context$set_auth(
  token = access_token,
  token_type = "Bearer",
  workspace_data_url = "https://api.us1.petrovisor.com/PetroVisor/API/MyWorkspace/",
  user = "your_username",
  workspace = "MyWorkspace"
)

# Clear authentication (useful for switching users/workspaces)
auth_context$clear()
```

### Scoped Authentication with `with_auth_context()`

Execute code with a specific authentication context without affecting
the global context:

``` r
# Define authentication for a specific operation
with_auth_context(
  token = access_token,
  token_type = "Bearer",
  workspace_data_url = workspace_url,
  {
    # Code here uses the specified authentication
    sp <- ServiceProvider$new(
      url = "https://identity.us1.petrovisor.com",
      workspace = "MyWorkspace",
      client_token = access_token
    )

    data <- sp$data$load(...)
  }
)

# Authentication context is restored after the block
```

### Requiring Authentication

Use
[`require_authentication()`](https://datagration.github.io/petrovisor-r-api/reference/require_authentication.md)
to ensure code only runs when properly authenticated:

``` r
# This function will throw an error if not authenticated
require_authentication()

# Use in custom functions
my_data_function <- function() {
  require_authentication()

  auth_context <- get_auth_context()
  # ... rest of your code
}
```

## Discovery URLs

PetroVisor uses OpenID Connect for authentication. The discovery URL is
the base URL of your PetroVisor identity server, which provides
authentication endpoints.

### Common Discovery URLs

``` r
# US Region 1
discovery_url <- "https://identity.us1.petrovisor.com"

# EU Region 1
discovery_url <- "https://identity.eu1.petrovisor.com"

# On-premise installations
discovery_url <- "https://your-company-petrovisor.com"
```

### Discovery Document

The authentication service uses the discovery document to find
authentication endpoints:

``` r
# Get the discovery document (advanced usage)
discovery_doc <- auth_service$get_discovery_document(
  "https://identity.us1.petrovisor.com"
)

# Contains endpoints like:
# - token_endpoint: Where to get tokens
# - petrovisor_webapi_endpoint: API base URL
```

## Token Management Best Practices

### 1. Store Tokens Securely

Never hardcode tokens in your scripts:

``` r
# BAD - Don't do this
token <- "eyJhbGciOiJSUzI1NiIsImtpZCI6..."

# GOOD - Use environment variables
token <- Sys.getenv("PETROVISOR_TOKEN")

# GOOD - Read from secure file
token <- readLines("~/.petrovisor/token.txt")[1]
```

### 2. Handle Token Expiration

Access tokens expire. Implement refresh logic:

``` r
# Store both access and refresh tokens
tokens <- list(
  access = token_response$access_token,
  refresh = token_response$refresh_token,
  expires_at = Sys.time() + token_response$expires_in
)

# Check expiration before API calls
check_token <- function(tokens, discovery_url) {
  if (Sys.time() >= tokens$expires_at) {
    # Token expired, refresh it
    auth_service <- AuthenticationService$new()
    new_response <- auth_service$get_access_token(
      refresh_token = tokens$refresh,
      discovery_url = discovery_url
    )

    tokens$access <- new_response$access_token
    tokens$refresh <- new_response$refresh_token
    tokens$expires_at <- Sys.time() + new_response$expires_in
  }

  return(tokens)
}
```

### 3. Use API Keys for Automation

For scheduled scripts and automation, API keys are more reliable than
username/password:

``` r
# In your automated script
api_key <- Sys.getenv("PETROVISOR_API_KEY")
discovery_url <- Sys.getenv("PETROVISOR_URL")

auth_service <- AuthenticationService$new()
token_response <- auth_service$get_access_token(
  key = api_key,
  discovery_url = discovery_url
)

sp <- ServiceProvider$new(
  url = discovery_url,
  workspace = Sys.getenv("PETROVISOR_WORKSPACE"),
  client_token = token_response$access_token
)
```

### 4. Separate Credentials by Environment

Use different credentials for development, testing, and production:

``` r
# Set up environment-specific configuration
config <- list(
  dev = list(
    url = "https://identity.dev.petrovisor.com",
    workspace = "DevWorkspace"
  ),
  prod = list(
    url = "https://identity.us1.petrovisor.com",
    workspace = "ProductionWorkspace"
  )
)

# Select environment
env <- Sys.getenv("R_ENV", "dev")
cfg <- config[[env]]

sp <- ServiceProvider$new(
  url = cfg$url,
  workspace = cfg$workspace,
  user = Sys.getenv("PETROVISOR_USER"),
  password = Sys.getenv("PETROVISOR_PASSWORD")
)
```

## Multi-Workspace Access

To work with multiple workspaces, create separate service providers:

``` r
# Connect to first workspace
sp1 <- ServiceProvider$new(
  url = "https://identity.us1.petrovisor.com",
  workspace = "Workspace1",
  user = username,
  password = password
)

# Connect to second workspace
sp2 <- ServiceProvider$new(
  url = "https://identity.us1.petrovisor.com",
  workspace = "Workspace2",
  user = username,
  password = password
)

# Load data from both workspaces
data1 <- sp1$data$load(...)
data2 <- sp2$data$load(...)

# Combine or compare data
combined <- merge(data1, data2, ...)
```

## Troubleshooting

### Common Authentication Errors

#### “Failed to retrieve token”

**Cause**: Invalid credentials or network issues

**Solution**: - Verify your username/password or API key - Check that
the discovery URL is correct - Ensure you have network access to the
identity server

#### “Token endpoint not found”

**Cause**: Invalid discovery URL

**Solution**: - Verify the discovery URL is correct - Ensure the URL
includes the protocol (<https://>) - Check if the identity server is
accessible

#### “Unauthorized” errors during API calls

**Cause**: Token expired or invalid

**Solution**: - Refresh your token using the refresh token -
Re-authenticate to get a new token - Check that you’re using the correct
workspace

### Testing Authentication

Test your authentication setup:

``` r
# Test authentication
tryCatch({
  auth_service <- AuthenticationService$new()
  token_response <- auth_service$get_access_token(
    username = Sys.getenv("PETROVISOR_USER"),
    password = Sys.getenv("PETROVISOR_PASSWORD"),
    discovery_url = "https://identity.us1.petrovisor.com"
  )

  cat("Authentication successful!\n")
  cat("Token expires in:", token_response$expires_in, "seconds\n")

}, error = function(e) {
  cat("Authentication failed:", e$message, "\n")
})
```

## Next Steps

Now that you understand authentication, explore these guides:

- **[Getting
  Started](https://datagration.github.io/petrovisor-r-api/articles/getting-started.md)**:
  Overview of the package
- **[Working with
  Data](https://datagration.github.io/petrovisor-r-api/articles/working-with-data.md)**:
  Loading and saving data
- **[Repository
  Service](https://datagration.github.io/petrovisor-r-api/articles/repository-service.md)**:
  Managing PetroVisor items

## See Also

- [`?AuthenticationService`](https://datagration.github.io/petrovisor-r-api/reference/AuthenticationService.md):
  Authentication service documentation
- [`?ServiceProvider`](https://datagration.github.io/petrovisor-r-api/reference/ServiceProvider.md):
  Service provider documentation
- [`?get_auth_context`](https://datagration.github.io/petrovisor-r-api/reference/get_auth_context.md):
  Authentication context functions
- [`?with_auth_context`](https://datagration.github.io/petrovisor-r-api/reference/with_auth_context.md):
  Scoped authentication
- [`?require_authentication`](https://datagration.github.io/petrovisor-r-api/reference/require_authentication.md):
  Authentication requirement checker
