# CleansingFilter

Class representing a cleansing filter object.

## Public fields

- `name`:

  The name of the cleansing filter.

- `formula`:

  The cleansing filter's formula.

- `unitName`:

  The cleansing filter's unit name.

## Methods

### Public methods

- [`CleansingFilter$new()`](#method-CleansingFilter-new)

- [`CleansingFilter$toList()`](#method-CleansingFilter-toList)

- [`CleansingFilter$clone()`](#method-CleansingFilter-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new CleansingFilter instance.

#### Usage

    CleansingFilter$new(name = NULL, formula = NULL, unitName = NULL)

#### Arguments

- `name`:

  The name of the cleansing filter.

- `formula`:

  The cleansing filter's formula.

- `unitName`:

  The cleansing filter's unit name.

------------------------------------------------------------------------

### Method `toList()`

#### Usage

    CleansingFilter$toList()

#### Details

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    CleansingFilter$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
CleansingFilter$new()
} # }
```
