# PVTData

Class representing pvt data.

## Public fields

- `signal_name`:

  The name of the signal.

- `entity_name`:

  The name of the entity.

- `unit_name`:

  The data's unit name.

- `data`:

  The data. A list of pressure-temperature-value triplets (named list).

- `scenario`:

  The scenario the data is part of.

## Methods

### Public methods

- [`PVTData$new()`](#method-PVTData-new)

- [`PVTData$to_list()`](#method-PVTData-to_list)

- [`PVTData$clone()`](#method-PVTData-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new PVTData instance.

#### Usage

    PVTData$new(
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

  The data. The data. A list of pressure-temperature-value triplets
  (named list).

- `scenario`:

  The scenario the data is part of.

------------------------------------------------------------------------

### Method `to_list()`

Convert the object to a list. This function is mainly used by the
DataServices to convert the objects to lists and then call the web API.

#### Usage

    PVTData$to_list()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PVTData$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
PVTData$new(signal_name = "produced oil per time increment pvt",
            entity_name = "Well01",
            unit_name = "m3",
            data = list(list(Pressure = 300,
                             Temperature = 150,
                             Value = 20)))
} # }
```
