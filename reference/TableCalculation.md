# TableCalculation

Class representing a PetroVisor table calculation object.

## Public fields

- `name`:

  The name of the table calculation.

- `columns`:

  A list of columns of the table calculation. Each item has to be an
  instance of the class `Column`.

- `inputTableNames`:

  A list containing the names of all input tables for the table
  calculation.

- `isLocked`:

  This flag specifies whether the table calculation is locked. Defaults
  to `FALSE`.

- `user`:

  The user the table calculation belongs to.

- `isFavorite`:

  This flag specifies whether the table calculation is marked as
  favorite item, and thus shown in the favorites tab on the home module
  in PetroVisor. Defaults to `FALSE`.

- `labels`:

  A list of strings holding the labels of the table calculation.

- `formula`:

  The table calculation's formula as string.

## Methods

### Public methods

- [`TableCalculation$new()`](#method-TableCalculation-new)

- [`TableCalculation$toList()`](#method-TableCalculation-toList)

- [`TableCalculation$clone()`](#method-TableCalculation-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new EventCalculation instance.

#### Usage

    TableCalculation$new(
      name = NULL,
      columns = NULL,
      inputTableNames = NULL,
      isLocked = FALSE,
      user = NULL,
      isFavorite = FALSE,
      labels = NULL,
      formula = NULL
    )

#### Arguments

- `name`:

  The name of the table calculation.

- `columns`:

  A list of columns of the table calculation. Each item has to be an
  instance of the class `Column`.

- `inputTableNames`:

  A list containing the names of all input tables for the table
  calculation.

- `isLocked`:

  This flag specifies whether the table calculation is locked. Defaults
  to `FALSE`.

- `user`:

  The user the table calculation belongs to.

- `isFavorite`:

  This flag specifies whether the table calculation is marked as
  favorite item, and thus shown in the favorites tab on the home module
  in PetroVisor. Defaults to `FALSE`.

- `labels`:

  A list of strings holding the labels of the table calculation.

- `formula`:

  The table calculation's formula as string.

------------------------------------------------------------------------

### Method `toList()`

#### Usage

    TableCalculation$toList()

#### Details

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TableCalculation$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
TableCalculation$new()
} # }
```
