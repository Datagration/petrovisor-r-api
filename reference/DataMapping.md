# DataMapping

Class representing a data mapping as used in data sources.

## Public fields

- `sourceRef`:

  The reference in the source.

- `entityName`:

  The name of the mapped entity.

- `signalName`:

  The name of the mapped signal.

- `unitName`:

  The name of the mapped unit.

## Methods

### Public methods

- [`DataMapping$new()`](#method-DataMapping-new)

- [`DataMapping$toList()`](#method-DataMapping-toList)

- [`DataMapping$clone()`](#method-DataMapping-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new DataMapping instance.

#### Usage

    DataMapping$new(
      sourceRef = NULL,
      entityName = NULL,
      signalName = NULL,
      unitName = NULL
    )

#### Arguments

- `sourceRef`:

  The reference in the source.

- `entityName`:

  The name of the mapped entity.

- `signalName`:

  The name of the mapped signal.

- `unitName`:

  The name of the mapped unit.

------------------------------------------------------------------------

### Method `toList()`

Convert the object to a list. This function is mainly used by the
Services to convert the objects to lists and then call the web API.

#### Usage

    DataMapping$toList()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DataMapping$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
DataMapping$new()
} # }
```
