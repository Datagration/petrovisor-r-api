# Workflow

Class representing a PetroVisor workflow object.

## See also

- [RepositoryService](https://datagration.github.io/petrovisor-r-api/reference/RepositoryService.md)
  for loading and saving workflows

- [WorkflowActivity](https://datagration.github.io/petrovisor-r-api/reference/WorkflowActivity.md)
  for workflow activity definitions

- [RWorkflowActivity](https://datagration.github.io/petrovisor-r-api/reference/RWorkflowActivity.md)
  for R-based workflow activities

- [CustomWorkflowActivity](https://datagration.github.io/petrovisor-r-api/reference/CustomWorkflowActivity.md)
  for custom activities

- [WorkflowSchedule](https://datagration.github.io/petrovisor-r-api/reference/WorkflowSchedule.md)
  for scheduling workflows

- [`vignette("repository-service")`](https://datagration.github.io/petrovisor-r-api/articles/repository-service.md)
  for workflow examples

## Public fields

- `name`:

  The name of the workflow.

- `activities`:

  A list of activities. The items in the list must be objects of the
  class `WorkflowActivity`.

- `isLocked`:

  This flag specifies whether the workflow is locked. Defaults to
  `FALSE`.

- `user`:

  The user the workflow belongs to.

- `isFavorite`:

  This flag specifies whether the workflow is marked as favorite item,
  and thus shown in the favorites tab on the home module in PetroVisor.
  Defaults to `FALSE`.

- `labels`:

  A list of strings holding the labels of the workflow.

## Methods

### Public methods

- [`Workflow$new()`](#method-Workflow-new)

- [`Workflow$toList()`](#method-Workflow-toList)

- [`Workflow$clone()`](#method-Workflow-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new Workflow instance.

#### Usage

    Workflow$new(
      name = NULL,
      activities = NULL,
      isLocked = FALSE,
      user = NULL,
      isFavorite = FALSE,
      labels = NULL
    )

#### Arguments

- `name`:

  The name of the workflow.

- `activities`:

  A list of activities. The items in the list must be objects of the
  class `WorkflowActivity`.

- `isLocked`:

  This flag specifies whether the workflow is locked. Defaults to
  `FALSE`.

- `user`:

  The user the workflow belongs to.

- `isFavorite`:

  This flag specifies whether the workflow is marked as favorite item,
  and thus shown in the favorites tab on the home module in PetroVisor.
  Defaults to `FALSE`.

- `labels`:

  A list of strings holding the labels of the workflow.

------------------------------------------------------------------------

### Method `toList()`

#### Usage

    Workflow$toList()

#### Details

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Workflow$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
Workflow$new()
} # }
```
