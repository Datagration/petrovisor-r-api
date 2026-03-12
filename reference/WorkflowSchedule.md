# WorkflowSchedule

Class representing a PetroVisor workflow schedule object.

## Public fields

- `name`:

  The name of the item.

- `modified`:

  The date the workflow schedule was modified (string).

- `scheduleName`:

  The name of the workflow schedule.

- `workflowName`:

  The name of the workflow associated with the schedule.

- `workspaceName`:

  The workspace in which the schedule is saved.

- `active`:

  This flag specifies whether the schedule is active.

- `processingEntitySetName`:

  The name of the processing entity set.

- `processingScopeName`:

  The name of the processing scope.

- `processingContextNames`:

  A list of strings holding the names of the processing contexts.

- `absoluteDate`:

  The date at which the schedule will be executed (string).

- `executedWorkflowName`:

  The name of the executed workflow.

- `executedWorkflowScheduleName`:

  The name of the executed workflow schedule.

- `scheduleStart`:

  The start date of the schedule (string).

- `scheduleEnd`:

  The end of the schedule. Must be an object of the class `ScheduleEnd`.

- `scheduleRecurrence`:

  Settings defining the schedule's recurrence. Must be an object of type
  `ScheduleRecurrence`.

- `isLocked`:

  This flag specifies whether the schedule is locked. Defaults to
  `FALSE`.

- `user`:

  The user the schedule belongs to.

- `isFavorite`:

  This flag specifies whether the schedule is marked as favorite item,
  and thus shown in the favorites tab on the home module in PetroVisor.
  Defaults to `FALSE`.

- `labels`:

  A list of strings holding the labels of the schedule.

## Methods

### Public methods

- [`WorkflowSchedule$new()`](#method-WorkflowSchedule-new)

- [`WorkflowSchedule$toList()`](#method-WorkflowSchedule-toList)

- [`WorkflowSchedule$clone()`](#method-WorkflowSchedule-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new Workflow schedule instance.

#### Usage

    WorkflowSchedule$new(
      name = NULL,
      modified = NULL,
      scheduleName = NULL,
      workflowName = NULL,
      workspaceName = NULL,
      active = NULL,
      processingEntitySetName = NULL,
      processingScopeName = NULL,
      processingContextNames = NULL,
      absoluteDate = NULL,
      executedWorkflowName = NULL,
      executedWorkflowScheduleName = NULL,
      scheduleStart = NULL,
      scheduleEnd = NULL,
      scheduleRecurrence = NULL,
      isLocked = FALSE,
      user = NULL,
      isFavorite = FALSE,
      labels = NULL
    )

#### Arguments

- `name`:

  The name of the item.

- `modified`:

  The date the workflow schedule was modified (string).

- `scheduleName`:

  The name of the workflow schedule.

- `workflowName`:

  The name of the workflow associated with the schedule.

- `workspaceName`:

  The workspace in which the schedule is saved.

- `active`:

  This flag specifies whether the schedule is active.

- `processingEntitySetName`:

  The name of the processing entity set.

- `processingScopeName`:

  The name of the processing scope.

- `processingContextNames`:

  A list of strings holding the names of the processing contexts.

- `absoluteDate`:

  The date at which the schedule will be executed (string).

- `executedWorkflowName`:

  The name of the executed workflow.

- `executedWorkflowScheduleName`:

  The name of the executed workflow schedule.

- `scheduleStart`:

  The start date of the schedule (string).

- `scheduleEnd`:

  The end of the schedule. Must be an object of the class `ScheduleEnd`.

- `scheduleRecurrence`:

  Settings defining the schedule's recurrence. Must be an object of type
  `ScheduleRecurrence`.

- `isLocked`:

  This flag specifies whether the schedule is locked. Defaults to
  `FALSE`.

- `user`:

  The user the schedule belongs to.

- `isFavorite`:

  This flag specifies whether the schedule is marked as favorite item,
  and thus shown in the favorites tab on the home module in PetroVisor.
  Defaults to `FALSE`.

- `labels`:

  A list of strings holding the labels of the schedule.

------------------------------------------------------------------------

### Method `toList()`

#### Usage

    WorkflowSchedule$toList()

#### Details

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    WorkflowSchedule$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
WorkflowSchedule$new()
} # }
```
