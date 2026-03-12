# Unit

Class representing a PetroVisor unit object.

## See also

- [UnitMeasurement](https://datagration.github.io/petrovisor-r-api/reference/UnitMeasurement.md)
  for measurement definitions

- [Signal](https://datagration.github.io/petrovisor-r-api/reference/Signal.md)
  for signal definitions using units

- [RepositoryService](https://datagration.github.io/petrovisor-r-api/reference/RepositoryService.md)
  for loading and saving units

- [DataServices](https://datagration.github.io/petrovisor-r-api/reference/DataServices.md)
  for data operations with units

## Public fields

- `name`:

  The name of the unit.

- `measurement_name`:

  The unit's measurement name.

- `factor`:

  The unit's factor. Used for conversion between the unit and it's base
  (SI) unit.

- `summand`:

  The unit's summand. Used for conversion between the unit and it's base
  (SI) unit.

## Methods

### Public methods

- [`Unit$new()`](#method-Unit-new)

- [`Unit$toList()`](#method-Unit-toList)

- [`Unit$clone()`](#method-Unit-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new Unit instance.

#### Usage

    Unit$new(name = NULL, measurement_name = NULL, factor = NULL, summand = NULL)

#### Arguments

- `name`:

  The name of the unit.

- `measurement_name`:

  The unit's measurement name.

- `factor`:

  The unit's factor. Used for conversion between the unit and it's base
  (SI) unit.

- `summand`:

  The unit's summand. Used for conversion between the unit and it's base
  (SI) unit.

------------------------------------------------------------------------

### Method `toList()`

#### Usage

    Unit$toList()

#### Details

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Unit$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
Unit$new(name = "hyper m",
         measurement_name = "Length",
         factor = 10000000000000,
         summand = 0)
} # }
```
