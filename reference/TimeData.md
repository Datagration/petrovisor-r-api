# TimeData

Class representing time-dependent data.

## Public fields

- `signal_name`:

  The name of the signal.

- `entity_name`:

  The name of the entity.

- `unit_name`:

  The data's unit name.

- `data`:

  The data. A list of date-value-pairs (named list).

- `scenario`:

  The scenario the data is part of.

## Methods

### Public methods

- [`TimeData$new()`](#method-TimeData-new)

- [`TimeData$to_list()`](#method-TimeData-to_list)

- [`TimeData$clone()`](#method-TimeData-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new TimeData instance.

#### Usage

    TimeData$new(
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

    TimeData$to_list()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TimeData$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
TimeData$new(signal_name = "produced oil per time increment",
             entity_name = "Well01",
             unit_name = "m3",
             data = list(list(Date = "2020-01-01T00:00:00.000Z",
                              Value = 20)))
} # }
```
