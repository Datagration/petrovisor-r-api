# CustomWorkflowActivity

Class representing a custom workflow activity object.

## Public fields

- `name`:

  The name of the custom workflow activity.

- `className`:

  The custom workflow activity's class name.

- `assemblyContent`:

  The content of the custom workflow activity.

## Methods

### Public methods

- [`CustomWorkflowActivity$new()`](#method-CustomWorkflowActivity-new)

- [`CustomWorkflowActivity$toList()`](#method-CustomWorkflowActivity-toList)

- [`CustomWorkflowActivity$clone()`](#method-CustomWorkflowActivity-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new custom workflow activity instance.

#### Usage

    CustomWorkflowActivity$new(
      name = NULL,
      className = NULL,
      assemblyContent = NULL
    )

#### Arguments

- `name`:

  The name of the custom workflow activity.

- `className`:

  The custom workflow activity's class name.

- `assemblyContent`:

  The content of the custom workflow activity.

------------------------------------------------------------------------

### Method `toList()`

#### Usage

    CustomWorkflowActivity$toList()

#### Details

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    CustomWorkflowActivity$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
CustomWorkflowActivity$new()
} # }
```
