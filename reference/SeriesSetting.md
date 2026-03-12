# SeriesSetting

Class representing a series setting object. These settings are used when
creating a chart.

## Public fields

- `scatterSize`:

  An integer giving the scatter size of the series.

- `lineThickness`:

  An integer specifiying the series' line thickness.

- `color`:

  A string specifying the series' color.

## Methods

### Public methods

- [`SeriesSetting$new()`](#method-SeriesSetting-new)

- [`SeriesSetting$toList()`](#method-SeriesSetting-toList)

- [`SeriesSetting$clone()`](#method-SeriesSetting-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new SeriesSetting instance.

#### Usage

    SeriesSetting$new(scatterSize = NULL, lineThickness = NULL, color = NULL)

#### Arguments

- `scatterSize`:

  An integer giving the scatter size of the series.

- `lineThickness`:

  An integer specifiying the series' line thickness.

- `color`:

  A string specifying the series' color.

------------------------------------------------------------------------

### Method `toList()`

#### Usage

    SeriesSetting$toList()

#### Details

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SeriesSetting$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
SeriesSetting$new(scatterSize = 2,
          lineThickness = 3,
          color = "#123456")
} # }
```
