# Column

Class representing a column object.

## Public fields

- `name`:

  The name of the column.

- `unit`:

  The unit of the column. Has to be an object of the class `Unit`.

- `formula`:

  The columns's formula.

- `isStatic`:

  This flag specifies whether the column is static or time-dependent.

- `savingSignalName`:

  The name of the signal in the columns saving clause.

- `saveToParentEntity`:

  This flag specifies whether data shall be saved to the parent of the
  entity.

## Methods

### Public methods

- [`Column$new()`](#method-Column-new)

- [`Column$toList()`](#method-Column-toList)

- [`Column$clone()`](#method-Column-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new Column instance.

#### Usage

    Column$new(
      name = NULL,
      unit = NULL,
      formula = NULL,
      isStatic = NULL,
      savingSignalName = NULL,
      saveToParentEntity = NULL
    )

#### Arguments

- `name`:

  The name of the column.

- `unit`:

  The unit of the column. Has to be an object of the class `Unit`.

- `formula`:

  The columns's formula.

- `isStatic`:

  This flag specifies whether the column is static or time-dependent.

- `savingSignalName`:

  The name of the signal in the columns saving clause.

- `saveToParentEntity`:

  This flag specifies whether data shall be saved to the parent of the
  entity.

------------------------------------------------------------------------

### Method `toList()`

#### Usage

    Column$toList()

#### Details

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Column$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
Column$new()
} # }
```
