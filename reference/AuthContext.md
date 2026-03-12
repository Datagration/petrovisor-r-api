# AuthContext

Singleton class for managing authentication context across the
application.

## Details

This class provides a centralized authentication context that can be
accessed from anywhere in the application. It stores authentication
credentials and provides methods to safely access them. The class
implements the singleton pattern to ensure a single source of truth for
authentication state.

## See also

- [ServiceProvider](https://datagration.github.io/petrovisor-r-api/reference/ServiceProvider.md)
  for creating authenticated sessions

- [AuthenticationService](https://datagration.github.io/petrovisor-r-api/reference/AuthenticationService.md)
  for authentication methods

- [`get_auth_context()`](https://datagration.github.io/petrovisor-r-api/reference/get_auth_context.md)
  for accessing the singleton instance

- [`with_auth_context()`](https://datagration.github.io/petrovisor-r-api/reference/with_auth_context.md)
  for executing code with authentication

- [`require_authentication()`](https://datagration.github.io/petrovisor-r-api/reference/require_authentication.md)
  for checking authentication status

- [`vignette("authentication")`](https://datagration.github.io/petrovisor-r-api/articles/authentication.md)
  for authentication examples

- [`vignette("getting-started")`](https://datagration.github.io/petrovisor-r-api/articles/getting-started.md)
  for setup guide

## Methods

### Public methods

- [`AuthContext$new()`](#method-AuthContext-new)

- [`AuthContext$set_auth()`](#method-AuthContext-set_auth)

- [`AuthContext$get_auth()`](#method-AuthContext-get_auth)

- [`AuthContext$get_auth_header()`](#method-AuthContext-get_auth_header)

- [`AuthContext$is_authenticated()`](#method-AuthContext-is_authenticated)

- [`AuthContext$clear_auth()`](#method-AuthContext-clear_auth)

- [`AuthContext$refresh_token()`](#method-AuthContext-refresh_token)

- [`AuthContext$get_user()`](#method-AuthContext-get_user)

- [`AuthContext$get_workspace()`](#method-AuthContext-get_workspace)

- [`AuthContext$get_workspace_url()`](#method-AuthContext-get_workspace_url)

- [`AuthContext$is_user()`](#method-AuthContext-is_user)

- [`AuthContext$is_workspace()`](#method-AuthContext-is_workspace)

- [`AuthContext$clone()`](#method-AuthContext-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize the AuthContext. This should not be called directly. Use
AuthContext\$get_instance() or get_auth_context() instead.

#### Usage

    AuthContext$new()

------------------------------------------------------------------------

### Method `set_auth()`

Set authentication credentials

#### Usage

    AuthContext$set_auth(
      token,
      token_type,
      workspace_data_url,
      user = NULL,
      workspace = NULL
    )

#### Arguments

- `token`:

  The access token

- `token_type`:

  The token type (e.g., "Bearer")

- `workspace_data_url`:

  The workspace-specific API URL

- `user`:

  Optional user information

- `workspace`:

  Optional workspace name

------------------------------------------------------------------------

### Method `get_auth()`

Get authentication information

#### Usage

    AuthContext$get_auth()

#### Returns

List containing auth details

------------------------------------------------------------------------

### Method `get_auth_header()`

Get formatted authorization header

#### Usage

    AuthContext$get_auth_header()

#### Returns

Character string for Authorization header

------------------------------------------------------------------------

### Method `is_authenticated()`

Check if authenticated

#### Usage

    AuthContext$is_authenticated()

#### Returns

Logical indicating if auth is available

------------------------------------------------------------------------

### Method `clear_auth()`

Clear authentication (logout)

#### Usage

    AuthContext$clear_auth()

------------------------------------------------------------------------

### Method `refresh_token()`

Update token (for token refresh scenarios)

#### Usage

    AuthContext$refresh_token(new_token, new_token_type = NULL)

#### Arguments

- `new_token`:

  The new access token

- `new_token_type`:

  Optional new token type

------------------------------------------------------------------------

### Method `get_user()`

Get current user information

#### Usage

    AuthContext$get_user()

#### Returns

Character string with username or NULL if not set

------------------------------------------------------------------------

### Method `get_workspace()`

Get current workspace information

#### Usage

    AuthContext$get_workspace()

#### Returns

Character string with workspace name or NULL if not set

------------------------------------------------------------------------

### Method `get_workspace_url()`

Get workspace data URL

#### Usage

    AuthContext$get_workspace_url()

#### Returns

Character string with workspace data URL

------------------------------------------------------------------------

### Method `is_user()`

Check if authentication context has specific user

#### Usage

    AuthContext$is_user(username)

#### Arguments

- `username`:

  Username to check

#### Returns

Logical indicating if current user matches

------------------------------------------------------------------------

### Method `is_workspace()`

Check if authentication context has specific workspace

#### Usage

    AuthContext$is_workspace(workspace_name)

#### Arguments

- `workspace_name`:

  Workspace name to check

#### Returns

Logical indicating if current workspace matches

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    AuthContext$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get the singleton instance
auth <- get_auth_context()

# Set authentication (typically done by ServiceProvider)
auth$set_auth(
  token = "your_access_token",
  token_type = "Bearer",
  workspace_data_url = "https://api.example.com/workspace/test/",
  user = "username",
  workspace = "workspace_name"
)

# Check if authenticated
if (auth$is_authenticated()) {
  # Get auth information
  auth_info <- auth$get_auth()

  # Get formatted authorization header
  header <- auth$get_auth_header()
}

# Clear authentication when done
auth$clear_auth()
} # }
```
