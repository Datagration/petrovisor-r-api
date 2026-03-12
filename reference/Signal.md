# Signal

Class representing a PetroVisor signal object.

## See also

- [DataServices](https://datagration.github.io/petrovisor-r-api/reference/DataServices.md)
  for loading and saving signal data

- [RepositoryService](https://datagration.github.io/petrovisor-r-api/reference/RepositoryService.md)
  for managing signal definitions

- [Unit](https://datagration.github.io/petrovisor-r-api/reference/Unit.md)
  for working with measurement units

- [UnitMeasurement](https://datagration.github.io/petrovisor-r-api/reference/UnitMeasurement.md)
  for unit measurements

- [`vignette("working-with-data")`](https://datagration.github.io/petrovisor-r-api/articles/working-with-data.md)
  for data examples

## Public fields

- `name`:

  The name of the signal.

- `short_name`:

  The signal's short name.

- `measurement_name`:

  The signal's measurement name.

- `storage_unit_name`:

  The name of the signal's storage
  [Unit](https://datagration.github.io/petrovisor-r-api/reference/Unit.md).

- `aggregation_type`:

  The signal's aggregation type.

- `container_aggregation_type`:

  The signal's container aggregation type.

- `signal_type`:

  The signal's type.

- `default_color`:

  The signal's default color.

- `default_line_type`:

  The signal's default line type.

- `setting_name`:

  (Optional) The name of the enumeration linked to the signal.

- `labels`:

  A list of strings holding the labels of the signal.

- `description`:

  The description of the signal.

## Methods

### Public methods

- [`Signal$new()`](#method-Signal-new)

- [`Signal$toList()`](#method-Signal-toList)

- [`Signal$clone()`](#method-Signal-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new Signal instance.

#### Usage

    Signal$new(
      name = NULL,
      short_name = NULL,
      measurement_name = NULL,
      storage_unit_name = NULL,
      aggregation_type = c("Sum", "Average", "Mode", "Max", "Min", "Count", "Count Distinct",
        "First", "Last", "None"),
      container_aggregation_type = c("Sum", "Average", "Mode", "Max", "Min", "Count",
        "Count Distinct", "First", "Last", "None"),
      signal_type = c("Static", "TimeDependent", "DepthDependent", "String", "PVT",
        "StringTimeDependent", "StringDepthDependent"),
      default_color = 0,
      default_line_type = c("Solid", "Dash", "DashDot"),
      setting_name = NULL,
      labels = list(),
      description = NULL
    )

#### Arguments

- `name`:

  The name of the signal.

- `short_name`:

  The signal's short name.

- `measurement_name`:

  The signal's measurement name.

- `storage_unit_name`:

  The name of the signal's storage unit.

- `aggregation_type`:

  The signal's aggregation type.

- `container_aggregation_type`:

  The signal's container aggregation type.

- `signal_type`:

  The signal's type.

- `default_color`:

  The signal's default color.

- `default_line_type`:

  The signal's default line type.

- `setting_name`:

  (Optional) The name of the enumeration linked to the signal.

- `labels`:

  A list of strings holding the labels of the signal.

- `description`:

  The description of the signal.

------------------------------------------------------------------------

### Method `toList()`

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

#### Usage

    Signal$toList()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Signal$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
Signal$new(name = "my signal",
           short_name = "my sig",
           measurement_name = "Length",
           storage_unit_name = "m",
           aggregation_type = "Average",
           container_aggregation_type = "Sum",
           signal_type = "TimeDependent",
           default_color = 0,
           default_line_type = "Solid",
           setting_name = NULL,
           labels = list(),
           description = NULL)
} # }
```
