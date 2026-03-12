# DataPoint

Class representing a time-dependent data point.

## Public fields

- `date`:

  The timestamp of the data point.

- `value`:

  The value at the specified timestamp.

## Methods

### Public methods

- [`DataPoint$new()`](#method-DataPoint-new)

- [`DataPoint$to_list()`](#method-DataPoint-to_list)

- [`DataPoint$clone()`](#method-DataPoint-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new DataPoint instance.

#### Usage

    DataPoint$new(date = NULL, value = NULL)

#### Arguments

- `date`:

  The timestamp of the data point.

- `value`:

  The value at the specified timestamp.

------------------------------------------------------------------------

### Method `to_list()`

Convert the object to a list. This function is mainly used by the
DataServices to convert the objects to lists and then call the web API.

#### Usage

    DataPoint$to_list()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DataPoint$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
DataPoint$new(date = "2020-01-01T00:00:00.000Z", value = 20)
} # }
```
