# ReferenceTableColumn

Class representing a PetroVisor ReferenceTableColumn object.

## Public fields

- `name`:

  The name of the column.

- `column_type`:

  The type of the column.

- `unit_name`:

  The name of the unit associated with the column.

## Methods

### Public methods

- [`ReferenceTableColumn$new()`](#method-ReferenceTableColumn-new)

- [`ReferenceTableColumn$toList()`](#method-ReferenceTableColumn-toList)

- [`ReferenceTableColumn$clone()`](#method-ReferenceTableColumn-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new ReferenceTableColumn instance.

#### Usage

    ReferenceTableColumn$new(
      name = NULL,
      column_type = c("Numeric", "String", "DateTime", "Bool"),
      unit_name = NULL
    )

#### Arguments

- `name`:

  The name of the column.

- `column_type`:

  The type of the column.

- `unit_name`:

  The name of the unit associated with the column.

------------------------------------------------------------------------

### Method `toList()`

#### Usage

    ReferenceTableColumn$toList()

#### Details

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ReferenceTableColumn$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
column <- ReferenceTableColumn$new(name = "Column One",
                                   column_type = "Numeric",
                                   unit_name = "m3")
} # }
```
