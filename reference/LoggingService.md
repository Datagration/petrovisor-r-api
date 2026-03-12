# LoggingService

Provides access to logging functionality provided through the web API.

## Details

A new instance of this class will be created by the ServiceProvider
automatically.

## See also

- [ServiceProvider](https://datagration.github.io/petrovisor-r-api/reference/ServiceProvider.md)
  for accessing the logging service via `sp$logs`

- [LogEntry](https://datagration.github.io/petrovisor-r-api/reference/LogEntry.md)
  for log entry structure

- [`vignette("getting-started")`](https://datagration.github.io/petrovisor-r-api/articles/getting-started.md)
  for basic usage

## Super class

[`Myrconn.PetroVisor.Client::ApiRequests`](https://datagration.github.io/petrovisor-r-api/reference/ApiRequests.md)
-\> `LoggingService`

## Methods

### Public methods

- [`LoggingService$new()`](#method-LoggingService-new)

- [`LoggingService$load_categories()`](#method-LoggingService-load_categories)

- [`LoggingService$load()`](#method-LoggingService-load)

- [`LoggingService$save()`](#method-LoggingService-save)

- [`LoggingService$clone()`](#method-LoggingService-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new LoggingService instance. This is done by the
[ServiceProvider](https://datagration.github.io/petrovisor-r-api/reference/ServiceProvider.md)
automatically.

#### Usage

    LoggingService$new()

#### Arguments

- `url`:

  the URL for the API calls.

- `token_type`:

  the type of the issued token.

- `token`:

  the issued token.

------------------------------------------------------------------------

### Method `load_categories()`

Get all available categories from the existing log entries.

#### Usage

    LoggingService$load_categories()

#### Returns

A character vector containing all available categories.

------------------------------------------------------------------------

### Method [`load()`](https://rdrr.io/r/base/load.html)

Retrieve all log entries matching the given filter from the database.

#### Usage

    LoggingService$load(
      last_entries = NULL,
      start = NULL,
      end = NULL,
      categories = NULL,
      user_name = NULL,
      severities = NULL,
      message_text_filter = NULL,
      workflow = NULL,
      schedule = NULL
    )

#### Arguments

- `last_entries`:

  Return only the latest n tag entries.

- `start`:

  Return log entries after the specified date (inclusive).

- `end`:

  Return log entries before the specified date (inclusive).

- `categories`:

  Return log entries of the specified categaories.

- `user_name`:

  Return log entries of the specified user.

- `severities`:

  Return log entries of the given severities.

- `message_text_filter`:

  Return log entries whose massage contains the specified text.

- `workflow`:

  Return log entries for the specified workflow.

- `schedule`:

  Return log entries for the specified schedule.

#### Returns

A dataframe containing the requested log entries. The number of returned
columns depends on the available information. Columns that contain no
information are not shown in the output.

------------------------------------------------------------------------

### Method [`save()`](https://rdrr.io/r/base/save.html)

Add one or several log entries to the database at once.

#### Usage

    LoggingService$save(log_entries)

#### Arguments

- `log_entries`:

  a list of LogEntry-objects.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LoggingService$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# create a new instance of the service provider
sp <- ServiceProvider$new("Host", 8095, "WorkspaceA", "UserX", "Password")

# get available categories
availableCategories <- sp$logs$load_categories()

# get log entries
allLogEntries <- sp$logs$load()
warnings <- sp$logs$load(severities = "Warning")
signIns <- sp$logs$load(categories = "SignIn")

# add log entry
entry <- LogEntry$new(message = "Test",
                      category = "Tag",
                      severity = "Information")
sp$logs$save(list(entry))

# add several log entries at once
entry1 <- LogEntry$new(message = "Test1",
                       category = "Tag",
                       severity = "Information")
entry2 <- LogEntry$new(message = "Test2",
                       category = "Tag",
                       severity = "Information")
sp$logs$save(list(entry1, entry2))
} # }
```
