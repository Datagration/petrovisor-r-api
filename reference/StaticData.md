# StaticData

Class representing static data.

## Public fields

- `signal_name`:

  The name of the signal.

- `entity_name`:

  The name of the entity.

- `unit_name`:

  The data's unit name.

- `data`:

  The data. In this case case a single numberic or string value.

- `scenario`:

  The scenario the data is part of.

## Methods

### Public methods

- [`StaticData$new()`](#method-StaticData-new)

- [`StaticData$to_list()`](#method-StaticData-to_list)

- [`StaticData$clone()`](#method-StaticData-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new StaticData instance.

#### Usage

    StaticData$new(
      signal_name = NULL,
      entity_name = NULL,
      unit_name = NULL,
      data = NULL,
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

  The data. In this case case a single number or string value.

- `scenario`:

  The scenario the data is part of.

------------------------------------------------------------------------

### Method `to_list()`

#### Usage

    StaticData$to_list()

#### Details

Convert the object to a list. This function is mainly used by the
DataServices to convert the objects to lists and then call the web API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    StaticData$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
StaticData$new(signal_name = "surface x-coordinate",
               entity_name = "Well01",
               unit_name = "m",
               data = 10)
} # }
```
