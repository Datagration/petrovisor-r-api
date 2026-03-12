# UnitMeasurement

Class representing a PetroVisor unit measurement object.

## See also

- [Unit](https://datagration.github.io/petrovisor-r-api/reference/Unit.md)
  for unit definitions

- [Signal](https://datagration.github.io/petrovisor-r-api/reference/Signal.md)
  for signal definitions using measurements

- [RepositoryService](https://datagration.github.io/petrovisor-r-api/reference/RepositoryService.md)
  for loading unit measurements

## Public fields

- `name`:

  The name of the unit measurement.

- `canonical_unit_name`:

  The name of the unit measurement's canonical unit.

## Methods

### Public methods

- [`UnitMeasurement$new()`](#method-UnitMeasurement-new)

- [`UnitMeasurement$toList()`](#method-UnitMeasurement-toList)

- [`UnitMeasurement$clone()`](#method-UnitMeasurement-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new UnitMeasurement instance.

#### Usage

    UnitMeasurement$new(name = NULL, canonical_unit_name = NULL)

#### Arguments

- `name`:

  The name of the unit measurement.

- `canonical_unit_name`:

  The name of the unit measurement's canonical unit.

------------------------------------------------------------------------

### Method `toList()`

#### Usage

    UnitMeasurement$toList()

#### Details

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    UnitMeasurement$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
UnitMeasurement$new(name = "Length", canonical_unit_name = "m")
} # }
```
