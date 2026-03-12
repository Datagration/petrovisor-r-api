# EventCalculation

Class representing a PetroVisor event calculation object.

## Public fields

- `name`:

  The name of the event calculation.

- `events`:

  A list of events of the event calculation. Each item has to be an
  instance of the class `Event`.

- `inputTableNames`:

  A list containing the names of all input tables for the event
  calculation.

- `isLocked`:

  This flag specifies whether the event calculation is locked. Defaults
  to `FALSE`.

- `user`:

  The user the event calculation belongs to.

- `isFavorite`:

  This flag specifies whether the event calculation is marked as
  favorite item, and thus shown in the favorites tab on the home module
  in PetroVisor. Defaults to `FALSE`.

- `labels`:

  A list of strings holding the labels of the event calculation.

- `formula`:

  The event calculation's formula as string.

## Methods

### Public methods

- [`EventCalculation$new()`](#method-EventCalculation-new)

- [`EventCalculation$toList()`](#method-EventCalculation-toList)

- [`EventCalculation$clone()`](#method-EventCalculation-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new EventCalculation instance.

#### Usage

    EventCalculation$new(
      name = NULL,
      events = NULL,
      inputTableNames = NULL,
      isLocked = FALSE,
      user = NULL,
      isFavorite = FALSE,
      labels = NULL,
      formula = NULL
    )

#### Arguments

- `name`:

  The name of the event calculation.

- `events`:

  A list of events of the event calculation. Each item has to be an
  instance of the class `Event`.

- `inputTableNames`:

  A list containing the names of all input tables for the event
  calculation.

- `isLocked`:

  This flag specifies whether the event calculation is locked. Defaults
  to `FALSE`.

- `user`:

  The user the event calculation belongs to.

- `isFavorite`:

  This flag specifies whether the event calculation is marked as
  favorite item, and thus shown in the favorites tab on the home module
  in PetroVisor. Defaults to `FALSE`.

- `labels`:

  A list of strings holding the labels of the event calculation.

- `formula`:

  The event calculation's formula as string.

------------------------------------------------------------------------

### Method `toList()`

#### Usage

    EventCalculation$toList()

#### Details

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    EventCalculation$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
EventCalculation$new()
} # }
```
