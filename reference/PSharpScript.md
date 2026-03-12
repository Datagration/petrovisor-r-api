# PSharpScript

Class representing a PetroVisor P# script object.

## Public fields

- `name`:

  The name of the script.

- `content`:

  The script's content as string (P# syntax).

- `isLocked`:

  This flag specifies whether the script is locked. Defaults to `FALSE`.

- `user`:

  The user the script belongs to.

- `isFavorite`:

  This flag specifies whether the script is marked as favorite item, and
  thus shown in the favorites tab on the home module in PetroVisor.
  Defaults to `FALSE`.

- `labels`:

  A list of strings holding the labels of the script.

## Methods

### Public methods

- [`PSharpScript$new()`](#method-PSharpScript-new)

- [`PSharpScript$toList()`](#method-PSharpScript-toList)

- [`PSharpScript$clone()`](#method-PSharpScript-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new P# script instance.

#### Usage

    PSharpScript$new(
      name = NULL,
      content = NULL,
      isLocked = FALSE,
      user = NULL,
      isFavorite = FALSE,
      labels = NULL
    )

#### Arguments

- `name`:

  The name of the P# script.

- `content`:

  The script's content as string (P# syntax).

- `isLocked`:

  This flag specifies whether the script is locked. Defaults to `FALSE`.

- `user`:

  The user the script belongs to.

- `isFavorite`:

  This flag specifies whether the script is marked as favorite item, and
  thus shown in the favorites tab on the home module in PetroVisor.
  Defaults to `FALSE`.

- `labels`:

  A list of strings holding the labels of the script.

------------------------------------------------------------------------

### Method `toList()`

#### Usage

    PSharpScript$toList()

#### Details

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PSharpScript$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
PSharpScript$new(name = "Myscript",
          content = "content dummy",
          isLocked = FALSE,
          isFavorite = FALSE)
} # }
```
