# Axis

Class representing a chart axis object. Axes are used when creating a
chart.

## Public fields

- `title`:

  The title of the axis.

- `isOpposed`:

  Flag specifying whether the axis is located on the opposing side of
  the chart (left or top).

- `isLog`:

  Flag specifying whether the axis is scaled logarithmically.

- `axisId`:

  The id of the axis.

- `manualMin`:

  Integer specifying the manually set minimum of the axis.

- `manualMax`:

  Integer specifying the manually set maximum of the axis.

- `isInversed`:

  Flag specifying whether the axis is inversed.

- `ticksPerInterval`:

  Integer specifying the number of ticks per interval.

## Methods

### Public methods

- [`Axis$new()`](#method-Axis-new)

- [`Axis$toList()`](#method-Axis-toList)

- [`Axis$clone()`](#method-Axis-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new Axis instance.

#### Usage

    Axis$new(
      title = NULL,
      isOpposed = NULL,
      isLog = NULL,
      axisId = NULL,
      manualMin = NULL,
      manualMax = NULL,
      isInversed = NULL,
      ticksPerInterval = NULL
    )

#### Arguments

- `title`:

  The title of the axis.

- `isOpposed`:

  Flag specifying whether the axis is located on the opposing side of
  the chart (left or top).

- `isLog`:

  Flag specifying whether the axis is scaled logarithmically.

- `axisId`:

  The id of the axis.

- `manualMin`:

  Integer specifying the manually set minimum of the axis.

- `manualMax`:

  Integer specifying the manually set maximum of the axis.

- `isInversed`:

  Flag specifying whether the axis is inversed.

- `ticksPerInterval`:

  Integer specifying the number of ticks per interval.

------------------------------------------------------------------------

### Method `toList()`

#### Usage

    Axis$toList()

#### Details

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Axis$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
Axis$new(title = "Fruits eaten",
         isOpposed = false,
         isLog = false,
         axisId = "fruits",
         manualMin = NULL,
         manualMax = NULL,
         isInversed = false,
         ticksPerInterval = 10)
} # }
```
