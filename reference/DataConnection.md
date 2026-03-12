# DataConnection

Class representing a PetroVisor data connection object.

## Public fields

- `name`:

  The name of the data connection.

- `connectionType`:

  The type of the data connection.

- `settings`:

  The settings of the data connection.

- `isLocked`:

  This flag specifies whether the data connection is locked. Defaults to
  `FALSE`.

- `user`:

  The user the data connection belongs to.

- `isFavorite`:

  This flag specifies whether the data connection is marked as favorite
  item, and thus shown in the favorites tab on the home module in
  PetroVisor. Defaults to `FALSE`.

- `labels`:

  A list of strings holding the labels of the data connection.

## Methods

### Public methods

- [`DataConnection$new()`](#method-DataConnection-new)

- [`DataConnection$toList()`](#method-DataConnection-toList)

- [`DataConnection$clone()`](#method-DataConnection-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new DataConnection instance.

#### Usage

    DataConnection$new(
      name = NULL,
      connectionType = NULL,
      settings = NULL,
      isLocked = FALSE,
      user = NULL,
      isFavorite = FALSE,
      labels = NULL
    )

#### Arguments

- `name`:

  The name of the data connection.

- `connectionType`:

  The type of the data connection.

- `settings`:

  The settings of the data connection.

- `isLocked`:

  This flag specifies whether the data connection is locked. Defaults to
  `FALSE`.

- `user`:

  The user the data connection belongs to.

- `isFavorite`:

  This flag specifies whether the data connection is marked as favorite
  item, and thus shown in the favorites tab on the home module in
  PetroVisor. Defaults to `FALSE`.

- `labels`:

  A list of strings holding the labels of the data connection.

------------------------------------------------------------------------

### Method `toList()`

#### Usage

    DataConnection$toList()

#### Details

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DataConnection$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
DataConnection$new()
} # }
```
