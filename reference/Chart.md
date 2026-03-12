# Chart

Class representing a PetroVisor chart object.

## Public fields

- `name`:

  The name of the chart.

- `dataType`:

  The datatype of the chart (string).

- `legendLocation`:

  A string specifying the location of the legend.

- `isLegendVisible`:

  A flag specifying whether the legend is visible.

- `isLegendOrientationHorizontal`:

  A flag specifying whether the legend is oriented horizontal.

- `isLegendPlacementInside`:

  A flag specifying whether the legend is placed inside or outside the
  chart.

- `chartType`:

  The type of the chart (string).

- `isStacked`:

  A flag specifying whether the chart is stacked.

- `isStacked100`:

  A flag specifying whether the chart is stacked.

- `isGridLinesOnXAxis`:

  A flag specifying whether gridlines are visible on the x-axis.

- `isGridLinesOnYAxis`:

  A flag specifying whether gridlines are visible on the y-axis.

- `dataAxesMappings`:

  A list giving the axes mappings.

- `seriesSettings`:

  A list of series settings. The individual settings must be instances
  of the class `SeriesSetting`.

- `axes`:

  A list of axes. The individual axes must be instances of the class
  `Axis`.

- `showToolTipTitle`:

  A flag specifying whether the tooltip title is shown.

- `showToolTipX`:

  A flag specifying whether to show the x-value in the tooltip.

- `showToolTipY`:

  A flag specifying whether to show the y-value in the tooltip.

- `isLocked`:

  This flag specifies whether the chart is locked. Defaults to `FALSE`.

- `user`:

  The user the chart belongs to.

- `isFavorite`:

  This flag specifies whether the chart is marked as favorite item, and
  thus shown in the favorites tab on the home module in PetroVisor.
  Defaults to `FALSE`.

- `labels`:

  A list of strings holding the labels of the chart.

## Methods

### Public methods

- [`Chart$new()`](#method-Chart-new)

- [`Chart$toList()`](#method-Chart-toList)

- [`Chart$clone()`](#method-Chart-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new Scope instance.

#### Usage

    Chart$new(
      name = NULL,
      dataType = NULL,
      legendLocation = NULL,
      isLegendVisible = NULL,
      isLegendOrientationHorizontal = NULL,
      isLegendPlacementInside = NULL,
      chartType = NULL,
      isStacked = NULL,
      isStacked100 = NULL,
      isGridLinesOnXAxis = NULL,
      isGridLinesOnYAxis = NULL,
      dataAxesMappings = NULL,
      seriesSettings = NULL,
      axes = NULL,
      showToolTipTitle = NULL,
      showToolTipX = NULL,
      showToolTipY = NULL,
      isLocked = FALSE,
      user = NULL,
      isFavorite = FALSE,
      labels = NULL
    )

#### Arguments

- `name`:

  The name of the chart.

- `dataType`:

  The datatype of the chart (string).

- `legendLocation`:

  A string specifying the location of the legend.

- `isLegendVisible`:

  A flag specifying whether the legend is visible.

- `isLegendOrientationHorizontal`:

  A flag specifying whether the legend is oriented horizontal.

- `isLegendPlacementInside`:

  A flag specifying whether the legend is placed inside or outside the
  chart.

- `chartType`:

  The type of the chart (string).

- `isStacked`:

  A flag specifying whether the chart is stacked.

- `isStacked100`:

  A flag specifying whether the chart is stacked.

- `isGridLinesOnXAxis`:

  A flag specifying whether gridlines are visible on the x-axis.

- `isGridLinesOnYAxis`:

  A flag specifying whether gridlines are visible on the y-axis.

- `dataAxesMappings`:

  A list giving the axes mappings.

- `seriesSettings`:

  A list of series settings. The individual settings must be instances
  of the class `SeriesSetting`.

- `axes`:

  A list of axes. The individual axes must be instances of the class
  `Axis`.

- `showToolTipTitle`:

  A flag specifying whether the tooltip title is shown.

- `showToolTipX`:

  A flag specifying whether to show the x-value in the tooltip.

- `showToolTipY`:

  A flag specifying whether to show the y-value in the tooltip.

- `isLocked`:

  This flag specifies whether the chart is locked. Defaults to `FALSE`.

- `user`:

  The user the chart belongs to.

- `isFavorite`:

  This flag specifies whether the chart is marked as favorite item, and
  thus shown in the favorites tab on the home module in PetroVisor.
  Defaults to `FALSE`.

- `labels`:

  A list of strings holding the labels of the chart.

------------------------------------------------------------------------

### Method `toList()`

#### Usage

    Chart$toList()

#### Details

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Chart$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
Chart$new()
} # }
```
