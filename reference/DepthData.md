# DepthData

Class representing depth-dependent data.

## Public fields

- `signal_name`:

  The name of the signal.

- `entity_name`:

  The name of the entity.

- `unit_name`:

  The data's unit name.

- `data`:

  The data. A list of depth-value-pairs (named list).

- `scenario`:

  The scenario the data is part of.

## Methods

### Public methods

- [`DepthData$new()`](#method-DepthData-new)

- [`DepthData$to_list()`](#method-DepthData-to_list)

- [`DepthData$clone()`](#method-DepthData-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new DepthData instance.

#### Usage

    DepthData$new(
      signal_name = NULL,
      entity_name = NULL,
      unit_name = NULL,
      data = list(),
      scenario = NULL
    )

#### Arguments

- `signal_name`:

  The name of the signal.

- `entity_name`:

  The name of the entity.

- `unit_name`:

  The data's unit name.

- `data`:

  The data. A list of date-value-pairs (named list)

- `scenario`:

  The scenario the data is part of.

------------------------------------------------------------------------

### Method `to_list()`

Convert the object to a list. This function is mainly used by the
DataServices to convert the objects to lists and then call the web API.

#### Usage

    DepthData$to_list()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DepthData$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
TimeData$new(signal_name = "produced oil per depth increment",
             entity_name = "Well01",
             unit_name = "m3",
             data = list(list(Depth = 100,
                              Value = 20)))
} # }
```
