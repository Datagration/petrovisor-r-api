# ServiceProvider

Provides access to all services provided through the web API.

## Value

Converted values. Either single number or collection of values.

## Details

This class allows interaction with the PetroVisor API by providing
access to various services such as logging, repository, data, and tag
entries.

## See also

Service classes for interacting with PetroVisor:

- [AuthenticationService](https://datagration.github.io/petrovisor-r-api/reference/AuthenticationService.md)
  for authentication methods

- [DataServices](https://datagration.github.io/petrovisor-r-api/reference/DataServices.md)
  for loading and saving data

- [RepositoryService](https://datagration.github.io/petrovisor-r-api/reference/RepositoryService.md)
  for managing items (entities, signals, units, etc.)

- [LoggingService](https://datagration.github.io/petrovisor-r-api/reference/LoggingService.md)
  for logging operations

- [FileService](https://datagration.github.io/petrovisor-r-api/reference/FileService.md)
  for file operations

- [MlTrainingService](https://datagration.github.io/petrovisor-r-api/reference/MlTrainingService.md)
  for machine learning

- [TagEntriesService](https://datagration.github.io/petrovisor-r-api/reference/TagEntriesService.md)
  for tag entry operations

Authentication helpers:

- [`get_auth_context()`](https://datagration.github.io/petrovisor-r-api/reference/get_auth_context.md)
  for accessing the authentication context

- [`with_auth_context()`](https://datagration.github.io/petrovisor-r-api/reference/with_auth_context.md)
  for executing code with specific auth context

- [`require_authentication()`](https://datagration.github.io/petrovisor-r-api/reference/require_authentication.md)
  for ensuring authentication

Vignettes:

- [`vignette("getting-started")`](https://datagration.github.io/petrovisor-r-api/articles/getting-started.md)
  for an introduction to the package

- [`vignette("authentication")`](https://datagration.github.io/petrovisor-r-api/articles/authentication.md)
  for authentication details

- [`vignette("working-with-data")`](https://datagration.github.io/petrovisor-r-api/articles/working-with-data.md)
  for data operations

## Super class

[`Myrconn.PetroVisor.Client::ApiRequests`](https://datagration.github.io/petrovisor-r-api/reference/ApiRequests.md)
-\> `ServiceProvider`

## Public fields

- `url`:

  Full URL to the PetroVisor API (either with or without token).

- `data_url`:

  The base URL for data services.

- `workspace`:

  The currently used workspace.

- `user`:

  The current user.

- `client_token`:

  Access token for the API (optional, used if URL is provided).

- `workspace_data_url`:

  The URL for the workspace-specific data services.

- `logs`:

  Instance of class `LoggingService` wrapping all logging-related
  functionalities.

- `items`:

  Instance of class `RepositoryService` wrapping all functionality
  related to PetroVisor items (entities, signals, units, etc.).

- `data`:

  Instance of class `DataServices` wrapping all functionality related to
  data.

- `tag_entries`:

  Instance of class `TagEntriesService` wrapping all functionality
  related to tag entries.

- `files`:

  Instance of class `FileService` wrapping all functionality related to
  files.

- `ml`:

  Instance of class `MlTrainingService` wrapping all functionality
  related to ML model training and prediction.

## Methods

### Public methods

- [`ServiceProvider$new()`](#method-ServiceProvider-new)

- [`ServiceProvider$parse_signal()`](#method-ServiceProvider-parse_signal)

- [`ServiceProvider$set_workspace_data_url()`](#method-ServiceProvider-set_workspace_data_url)

- [`ServiceProvider$getDataUrl()`](#method-ServiceProvider-getDataUrl)

- [`ServiceProvider$convert_unit()`](#method-ServiceProvider-convert_unit)

- [`ServiceProvider$send_mail()`](#method-ServiceProvider-send_mail)

- [`ServiceProvider$clone()`](#method-ServiceProvider-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new ServiceProvider instance.

#### Usage

    ServiceProvider$new(
      url = NULL,
      workspace = NULL,
      user = NULL,
      password = NULL,
      client_token = NULL
    )

#### Arguments

- `url`:

  Full URL to the PetroVisor API (if token is provided).

- `workspace`:

  Name of the workspace to connect to.

- `user`:

  Username for authentication.

- `password`:

  Password for the given user.

- `client_token`:

  Access token for authentication (optional).

------------------------------------------------------------------------

### Method `parse_signal()`

Parse the mapped signal received from PetroVisor to a list containing
the name and unit of the mapped signal.

#### Usage

    ServiceProvider$parse_signal(mapped_signal)

#### Arguments

- `mapped_signal`:

  The mapped signal (string) as received from PetroVisor.

------------------------------------------------------------------------

### Method `set_workspace_data_url()`

Set the workspace-specific data URL.

#### Usage

    ServiceProvider$set_workspace_data_url(data_url, workspace)

#### Arguments

- `data_url`:

  The base URL for data services.

- `workspace`:

  The name of the workspace.

------------------------------------------------------------------------

### Method `getDataUrl()`

Retrieve the base data URL from the authentication service.

#### Usage

    ServiceProvider$getDataUrl()

------------------------------------------------------------------------

### Method `convert_unit()`

Convert values from a source unit to a target unit

#### Usage

    ServiceProvider$convert_unit(x, source_unit, target_unit)

#### Arguments

- `x`:

  The value to convert. Either a single value, or a collection of
  values.

- `source_unit`:

  The name of the source unit.

- `target_unit`:

  The name of the target unit.

------------------------------------------------------------------------

### Method `send_mail()`

Send an email using the PetroVisor email configuration.

#### Usage

    ServiceProvider$send_mail(address, subject, body)

#### Arguments

- `address`:

  The recipient email address (single string).

- `subject`:

  The subject of the email.

- `body`:

  The body content of the email.

#### Examples

    \dontrun{
      # Send a simple email
      sp$send_mail(
        address = "example@domain.com",
        subject = "Test Subject",
        body = "This is a test email from PetroVisor R client."
      )
    }

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ServiceProvider$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a new instance of the service provider using token
sp <- ServiceProvider$new(
  url = "https://identity.us1.petrovisor.com/PetroVisor/API/WorkspaceA/",
  client_token = "your_token_here"
)

# Create a new instance of the service provider using username and password
sp <- ServiceProvider$new(
  url = "https://identity.us1.petrovisor.com",
  workspace = "WorkspaceA",
  user = "your_username",
  password = "your_password"
)
} # }

## ------------------------------------------------
## Method `ServiceProvider$send_mail`
## ------------------------------------------------

if (FALSE) { # \dontrun{
  # Send a simple email
  sp$send_mail(
    address = "example@domain.com",
    subject = "Test Subject",
    body = "This is a test email from PetroVisor R client."
  )
} # }
```
