# PetroVisor R API

This package provides a wrapper for the PetroVisor API, enabling users to interact with PetroVisor services directly from R. It includes functionalities for authentication, data retrieval, logging, and more.

## Features

- Authenticate using API keys, user credentials, or refresh tokens.
- Access and manage PetroVisor workspaces.
- Interact with logging, repository, data, and tag entry services.

## Installation

To install the package using `remotes`, run the following command:

```r
# Install from GitHub
remotes::install_github("Datagration/petrovisor-r-api")
```

## Usage

### Authentication

Authenticate with the PetroVisor API using an API key, username/password, or refresh token.

```r
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

Use the `ServiceProvider` class to interact with various PetroVisor services.

```r
# Create a new instance of the ServiceProvider using a token
sp <- ServiceProvider$new(
  url = "https://identity.us1.petrovisor.com",
  client_token = "your_token_here"
)

#Create a new instance of the ServiceProvider using username and password
sp <- ServiceProvider$new(
  url = "https://identity.us1.petrovisor.com",
  workspace = "your_workspace_name",
  user = "your_user_name",
  password = "your_password"
)

# Access logging service
log_entries <- sp$loggingService$GetLogEntries()

# Access repository service
entity_names <- sp$repositoryService$GetNamesOfItems("Entity")
```

## Test Use Case

Here is a simple test use case to verify the functionality of the package:

```r
library(Myrconn.PetroVisor.Client)

# Initialize the ServiceProvider
sp <- ServiceProvider$new(
  url = "https://identity.us1.petrovisor.com",
  workspace = "TestWorkspace",
  user = "test_user",
  password = "test_password"
)

# Retrieve log entries
log_entries <- sp$loggingService$GetLogEntries()
print(log_entries)
```

## License

This package is licensed under the MIT License. See the `LICENSE` file for details.
