# WorkflowActivity

Class representing a workflow activity object.

## Public fields

- `activityType`:

  String giving the activity type.

- `activityName`:

  The activity's name.

- `settings`:

  The activity's settings as string.

- `mappedInputArguments`:

  A list of mapped input arguments. The items in the list must be
  objects of the class `ActivityMappedArgument`.

- `mappedOutputArguments`:

  a list of mapped output arguments. The items in the list must be
  objects of the class `ActivityMappedArgument`.

- `inputArgumentsInfo`:

  A list of argument information. The items in the list must be objects
  of the class `ActivityArgument`.

## Methods

### Public methods

- [`WorkflowActivity$new()`](#method-WorkflowActivity-new)

- [`WorkflowActivity$toList()`](#method-WorkflowActivity-toList)

- [`WorkflowActivity$clone()`](#method-WorkflowActivity-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new workflow activity instance.

#### Usage

    WorkflowActivity$new(
      activityType = NULL,
      activityName = NULL,
      settings = NULL,
      mappedInputArguments = NULL,
      mappedOutputArguments = NULL,
      inputArgumentsInfo = NULL
    )

#### Arguments

- `activityType`:

  String giving the activity type.

- `activityName`:

  The activity's name.

- `settings`:

  The activity's settings as string.

- `mappedInputArguments`:

  A list of mapped input arguments. The items in the list must be
  objects of the class `ActivityMappedArgument`.

- `mappedOutputArguments`:

  a list of mapped output arguments. The items in the list must be
  objects of the class `ActivityMappedArgument`.

- `inputArgumentsInfo`:

  A list of argument information. The items in the list must be objects
  of the class `ActivityArgument`.

------------------------------------------------------------------------

### Method `toList()`

#### Usage

    WorkflowActivity$toList()

#### Details

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    WorkflowActivity$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
WorkflowActivity$new()
} # }
```
