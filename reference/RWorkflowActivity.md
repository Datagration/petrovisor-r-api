# RWorkflowActivity

Class representing an R workflow activity object.

## Public fields

- `name`:

  The name of the R workflow activity.

- `providerConfiguration`:

  The configuration of the provider. Must be an object of the class
  `RProviderConfiguration`.

- `scriptName`:

  The name of the activity's R script.

- `functionName`:

  The name of the activity's R function. The function has to present in
  the referenced R script.

- `input`:

  A list of input arguments for the activity. The items in the list must
  be objects of the class `ActivityArgument`.

- `output`:

  A list of output arguments for the activity. The items in the list
  must be objects of the class `ActivityArgument`.

## Methods

### Public methods

- [`RWorkflowActivity$new()`](#method-RWorkflowActivity-new)

- [`RWorkflowActivity$toList()`](#method-RWorkflowActivity-toList)

- [`RWorkflowActivity$clone()`](#method-RWorkflowActivity-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new R workflow activity instance.

#### Usage

    RWorkflowActivity$new(
      name = NULL,
      providerConfiguration = NULL,
      scriptName = NULL,
      functionName = NULL,
      input = NULL,
      output = NULL
    )

#### Arguments

- `name`:

  The name of the R workflow activity.

- `providerConfiguration`:

  The configuration of the provider. Must be an object of the class
  `RProviderConfiguration`.

- `scriptName`:

  The name of the activity's R script.

- `functionName`:

  The name of the activity's R function. The function has to present in
  the referenced R script.

- `input`:

  A list of input arguments for the activity. The items in the list must
  be objects of the class `ActivityArgument`.

- `output`:

  A list of output arguments for the activity. The items in the list
  must be objects of the class `ActivityArgument`.

------------------------------------------------------------------------

### Method `toList()`

#### Usage

    RWorkflowActivity$toList()

#### Details

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    RWorkflowActivity$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
RWorkflowActivity$new()
} # }
```
