# CleansingCalculation

Class representing a PetroVisor cleansing calculation object.

## Public fields

- `name`:

  The name of the cleansing calculation.

- `filters`:

  A list of filters of the cleansing calculation. Each item has to be an
  instance of the class `CleansingFilter`.

- `isLocked`:

  This flag specifies whether the cleansing calculation is locked.
  Defaults to `FALSE`.

- `user`:

  The user the cleansing calculation belongs to.

- `isFavorite`:

  This flag specifies whether the cleansing calculation is marked as
  favorite item, and thus shown in the favorites tab on the home module
  in PetroVisor. Defaults to `FALSE`.

- `labels`:

  A list of strings holding the labels of the cleansing calculation.

- `formula`:

  The cleansing calculation's formula as string.

## Methods

### Public methods

- [`CleansingCalculation$new()`](#method-CleansingCalculation-new)

- [`CleansingCalculation$toList()`](#method-CleansingCalculation-toList)

- [`CleansingCalculation$clone()`](#method-CleansingCalculation-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new CleansingCalculation instance.

#### Usage

    CleansingCalculation$new(
      name = NULL,
      filters = NULL,
      isLocked = FALSE,
      user = NULL,
      isFavorite = FALSE,
      labels = NULL,
      formula = NULL
    )

#### Arguments

- `name`:

  The name of the cleansing calculation.

- `filters`:

  A list of filters of the cleansing calculation. Each item has to be an
  instance of the class `CleansingFilter`.

- `isLocked`:

  This flag specifies whether the cleansing calculation is locked.
  Defaults to `FALSE`.

- `user`:

  The user the cleansing calculation belongs to.

- `isFavorite`:

  This flag specifies whether the cleansing calculation is marked as
  favorite item, and thus shown in the favorites tab on the home module
  in PetroVisor. Defaults to `FALSE`.

- `labels`:

  A list of strings holding the labels of the cleansing calculation.

- `formula`:

  The cleansing calculation's formula as string.

------------------------------------------------------------------------

### Method `toList()`

#### Usage

    CleansingCalculation$toList()

#### Details

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    CleansingCalculation$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
CleansingCalculation$new()
} # }
```
