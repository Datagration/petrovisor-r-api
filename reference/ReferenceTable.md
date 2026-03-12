# ReferenceTable

Class representing a PetroVisor ReferenceTable object.

## Public fields

- `name`:

  The name of the reference table.

- `description`:

  The description of the item.

- `labels`:

  A list of strings holding the labels of the reference table.

- `key`:

  The key column definition. Object of type ReferenceTableColumn.

- `values`:

  List of value column definitions. I.e. a list of objects of type
  ReferenceTableColumn.

## Methods

### Public methods

- [`ReferenceTable$new()`](#method-ReferenceTable-new)

- [`ReferenceTable$toList()`](#method-ReferenceTable-toList)

- [`ReferenceTable$clone()`](#method-ReferenceTable-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new ReferenceTable instance.

#### Usage

    ReferenceTable$new(
      name = NULL,
      description = NULL,
      labels = list(),
      key = NULL,
      values = list()
    )

#### Arguments

- `name`:

  The name of the reference table.

- `description`:

  The description of the item.

- `labels`:

  A list of strings holding the labels of the reference table.

- `key`:

  The key column definition. Object of type ReferenceTableColumn.

- `values`:

  List of value column definitions. I.e. a list of objects of type
  ReferenceTableColumn.

------------------------------------------------------------------------

### Method `toList()`

#### Usage

    ReferenceTable$toList()

#### Details

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ReferenceTable$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
referenceTable <- ReferenceTable$new(
    name = "Test R ReferenceTable",
    description = "Test Description",
    labels = list("A", "B"),
    key = ReferenceTableColumn$new(name = "TestKey",
                                   column_type = "Numeric",
                                   unit_name = "m3"),
    values = list(
        ReferenceTableColumn$new(name = "Value One",
                                 column_type = "Numeric",
                                 unit_name = "m3"),
        ReferenceTableColumn$new(name = "Value Two",
                                 column_type = "Bool",
                                 unit_name = " ")
    ))
} # }
```
