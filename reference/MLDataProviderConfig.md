# MLDataProviderConfig

Configuration for ML data providers.

## Public fields

- `provider_type`:

  The data provider type (PSharp or ReferenceTable).

- `source_name`:

  The name of the data source.

- `storage_name`:

  The name of the data storage.

- `columns`:

  The configuration of the data columns. List of MLDataColumns.

## Methods

### Public methods

- [`MLDataProviderConfig$new()`](#method-MLDataProviderConfig-new)

- [`MLDataProviderConfig$toList()`](#method-MLDataProviderConfig-toList)

- [`MLDataProviderConfig$clone()`](#method-MLDataProviderConfig-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new MLDataProviderConfig instance.

#### Usage

    MLDataProviderConfig$new(
      provider_type = c("PSharp", "ReferenceTable"),
      source_name = NULL,
      storage_name = NULL,
      columns = list()
    )

#### Arguments

- `provider_type`:

  The data provider type (PSharp or ReferenceTable).

- `source_name`:

  The name of the data source.

- `storage_name`:

  The name of the data storage.

- `columns`:

  The configuration of the data columns. List of MLDataColumns.

------------------------------------------------------------------------

### Method `toList()`

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

#### Usage

    MLDataProviderConfig$toList()

#### Returns

A list representation of the MLDataProviderConfig object compatible with
the API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MLDataProviderConfig$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a MLDataProviderConfig
MLDataProviderConfig$new(
  provider_type = "ReferenceTable",
  source_name = "My Source Table",
  storage_name = "My Target Table",
  columns = list()
)
} # }
```
