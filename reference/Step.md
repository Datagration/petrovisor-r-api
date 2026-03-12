# Step

Class representing a process template step.

## Public fields

- `name`:

  The name of the step.

- `userGroupName`:

  The name of the user group assigned to the step.

## Methods

### Public methods

- [`Step$new()`](#method-Step-new)

- [`Step$toList()`](#method-Step-toList)

- [`Step$clone()`](#method-Step-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new Step instance.

#### Usage

    Step$new(name = NULL, userGroupName = NULL)

#### Arguments

- `name`:

  The name of the step.

- `userGroupName`:

  The name of the user group assigned to the step.

------------------------------------------------------------------------

### Method `toList()`

Convert the object to a list. This function is mainly used by the
Services to convert the objects to lists and then call the web API.

#### Usage

    Step$toList()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Step$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
Step$new(name = "DoThis", userGroupName = "Engineers")
} # }
```
