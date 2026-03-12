# LogEntry

Class to define and create new log entries.

## See also

- [LoggingService](https://datagration.github.io/petrovisor-r-api/reference/LoggingService.md)
  for saving and loading log entries

- [ServiceProvider](https://datagration.github.io/petrovisor-r-api/reference/ServiceProvider.md)
  for accessing the logging service

- [`vignette("getting-started")`](https://datagration.github.io/petrovisor-r-api/articles/getting-started.md)
  for logging examples

## Public fields

- `timestamp`:

  The timestamp of the log entry. If none is specified, the current date
  and time will be inserted automatically.

- `message`:

  The message of the log entry.

- `category`:

  The category of the log entry. May be any string.

- `user_name`:

  If not specified, the current user will be inserted automatically.

- `severity`:

  The severity of the log entry. One of: `Information`, `Warning`,
  `Error`, `Debug`, `Alarm`.

- `workspace`:

  The workspace of the log entry. If none is specified, the current
  workspace will be inserted automatically.

- `item_name`:

  The name of the item the log entry is related to.

- `item_type`:

  The name of the item type the log entry is related to.

- `item_change`:

  The type of change the log entry is related to. One of: `Created`,
  `Modified`, `Deleted`, `Renamed`, `Other`.

- `schedule_name`:

  The name of the schedule affected by the operation.

- `workflow_name`:

  The name of the workflow affected by the operation.

- `start_time`:

  The start time of an operation.

- `end_time`:

  The end time of an operation.

- `elapsed_time`:

  The time an operation needed to execute.

- `message_details`:

  Optional details of the message.

- `directory`:

  User directory (tenant).

## Methods

### Public methods

- [`LogEntry$new()`](#method-LogEntry-new)

- [`LogEntry$to_list()`](#method-LogEntry-to_list)

- [`LogEntry$clone()`](#method-LogEntry-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new LogEntry instance.

#### Usage

    LogEntry$new(
      timestamp = NULL,
      message = NULL,
      category = NULL,
      user_name = NULL,
      severity = c("Information", "Warning", "Error", "Debug", "Alarm"),
      workspace = NULL,
      item_name = NULL,
      item_type = c("Unknown", "Signal", "Unit", "EntityType", "BlobFile", "Entity",
        "EntitySet", "Hierarchy", "Calculation", "Workspace", "Tag", "Scope",
        "EventSubscription", "Context", "EventCalculation", "CleansingCalculation",
        "PSharpScript", "TicketTimeInterval", "Label", "UserGroup", "WorkflowSchedule",
        "Message", "FilterDefinition", "TagEntry", "Ticket", "DataConnection", "DataSource",
        "DataIntegrationSet", "Dashboard", "Scenario", "Snapshot", "User", "ChartDefinition",
        "PivotTable", "DCA", "MLModel", "CustomWorkflowActivity",
         "RWorkflowActivity",
        "CleansingScript", "ProcessTemplate", "Workflow", "WorkspaceValue", "ReferenceTable",
        "WorkspacePackage", "PythonWorkflowActivity", "DataGrid", "WebWorkflowActivity",
        "PowerBIItem", "DataIntegrationSession"),
      item_change = c("Other", "Created", "Modified", "Deleted", "Renamed"),
      schedule_name = NULL,
      workflow_name = NULL,
      start_time = NULL,
      end_time = NULL,
      elapsed_time = NULL,
      message_details = NULL,
      directory = NULL
    )

#### Arguments

- `timestamp`:

  The timestamp of the log entry. If none is specified, the current date
  and time will be inserted automatically.

- `message`:

  The message of the log entry.

- `category`:

  The category of the log entry. May be any string.

- `user_name`:

  If not specified, the current user will be inserted automatically.

- `severity`:

  The severity of the log entry. One of: `Information`, `Warning`,
  `Error`, `Debug`, `Alarm`.

- `workspace`:

  The workspace of the log entry. If none is specified, the current
  workspace will be inserted automatically.

- `item_name`:

  The name of the item the log entry is related to.

- `item_type`:

  The name of the item type the log entry is related to.

- `item_change`:

  The type of change the log entry is related to. One of: `Created`,
  `Modified`, `Deleted`, `Renamed`, `Other`.

- `schedule_name`:

  The name of the schedule affected by the operation.

- `workflow_name`:

  The name of the workflow affected by the operation.

- `start_time`:

  The start time of an operation.

- `end_time`:

  The end time of an operation.

- `elapsed_time`:

  The time an operation needed to execute.

- `message_details`:

  Optional details of the message.

- `directory`:

  User directory (tenant).

------------------------------------------------------------------------

### Method `to_list()`

Convert the object to a list. This function is mainly used by the
LoggingService to convert the objects to lists and then call the web
API.

#### Usage

    LogEntry$to_list()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LogEntry$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
LogEntry$new(category = "MyCategory",
             message = "This is the message",
             message_details = "and here are some details",
             severity = "Debug")
} # }
```
