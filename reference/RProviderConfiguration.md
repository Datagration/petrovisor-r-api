# RProviderConfiguration

Class representing an R provider configuration object.

## Public fields

- `providerType`:

  A string specifying the provider type. Allowed values are: `RNET` and
  `Rserve`.

- `rServerAddress`:

  The URL or host name of the server running R.

- `rServerPort`:

  The port Rserve is available at.

- `rServerScriptsFolder`:

  The path to the folder containing the scripts.

## Methods

### Public methods

- [`RProviderConfiguration$new()`](#method-RProviderConfiguration-new)

- [`RProviderConfiguration$toList()`](#method-RProviderConfiguration-toList)

- [`RProviderConfiguration$clone()`](#method-RProviderConfiguration-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new R provider configuration instance.

#### Usage

    RProviderConfiguration$new(
      providerType = c("RNET", "Rserve"),
      rServerAddress = NULL,
      rServerPort = NULL,
      rServerScriptsFolder = NULL
    )

#### Arguments

- `providerType`:

  A string specifying the provider type. Allowed values are: `RNET` and
  `Rserve`.

- `rServerAddress`:

  The URL or host name of the server running R.

- `rServerPort`:

  The port Rserve is available at.

- `rServerScriptsFolder`:

  The path to the folder containing the scripts.

------------------------------------------------------------------------

### Method `toList()`

#### Usage

    RProviderConfiguration$toList()

#### Details

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    RProviderConfiguration$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
RProviderConfiguration$new()
} # }
```
