# DataSetRequest

**\[deprecated\]**

Class representing a PetroVisor dataset request object.

**This class is deprecated and no longer used.** It was originally
designed for use with DataServices but was never integrated into the
actual implementation. DataServices uses direct parameter passing
instead.

This class is retained only for backwards compatibility and may be
removed in a future version.

## See also

- [DataServices](https://datagration.github.io/petrovisor-r-api/reference/DataServices.md)
  for loading data (does not use DataSetRequest)

- [Entity](https://datagration.github.io/petrovisor-r-api/reference/Entity.md)
  for entity definitions

- [Signal](https://datagration.github.io/petrovisor-r-api/reference/Signal.md)
  for signal definitions

- [Unit](https://datagration.github.io/petrovisor-r-api/reference/Unit.md)
  for unit definitions

- [`vignette("working-with-data")`](https://datagration.github.io/petrovisor-r-api/articles/working-with-data.md)
  for data loading examples

## Public fields

- `entityName`:

  The name of the
  [Entity](https://datagration.github.io/petrovisor-r-api/reference/Entity.md)
  for which data is requested.

- `signalName`:

  The name of the requested
  [Signal](https://datagration.github.io/petrovisor-r-api/reference/Signal.md).

- `unitName`:

  The name of the
  [Unit](https://datagration.github.io/petrovisor-r-api/reference/Unit.md)
  the data is requested in.

## Methods

### Public methods

- [`DataSetRequest$new()`](#method-DataSetRequest-new)

- [`DataSetRequest$toList()`](#method-DataSetRequest-toList)

- [`DataSetRequest$clone()`](#method-DataSetRequest-clone)

------------------------------------------------------------------------

### Method `new()`

**\[deprecated\]**

Create a new DataSetRequest instance.

**Deprecated:** This class is not used by DataServices and will be
removed in a future version.

#### Usage

    DataSetRequest$new(entityName = NULL, signalName = NULL, unitName = NULL)

#### Arguments

- `entityName`:

  The name of the
  [Entity](https://datagration.github.io/petrovisor-r-api/reference/Entity.md)
  for which data is requested.

- `signalName`:

  The name of the requested
  [Signal](https://datagration.github.io/petrovisor-r-api/reference/Signal.md).

- `unitName`:

  The name of the
  [Unit](https://datagration.github.io/petrovisor-r-api/reference/Unit.md)
  the data is requested in.

------------------------------------------------------------------------

### Method `toList()`

**\[deprecated\]**

Convert the object to a list. This function is mainly used by
[DataServices](https://datagration.github.io/petrovisor-r-api/reference/DataServices.md)
to convert the objects to lists and then call the web API.

**Deprecated:** This method is not used by DataServices.

#### Usage

    DataSetRequest$toList()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DataSetRequest$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# DEPRECATED: This class is not used by DataServices
DataSetRequest$new(entityName = "Well01",
                   signalName = "surface x-coordinate",
                   unitName = "m")
} # }
```
