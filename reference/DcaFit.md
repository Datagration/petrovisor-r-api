# DcaFit

Class representing a PetroVisor DcaFit object.

## Public fields

- `name`:

  The name of the DCA fit.

- `entity`:

  The entity for which the DCA fit is created. Must be an instance of
  the class `Entity`.

- `rateSignal`:

  The signal giving the production rate. Must be an instance of the
  class `Signal`.

- `rateUnit`:

  The unit the rate signal is given in. Must be an instance of the class
  `Unit`.

- `cumulativeSignal`:

  The signal giving the cumulative data. Must be an instance of the
  class `Signal`.

- `cumulativeUnit`:

  The unit the cumulative signal is given in. Must be an instance of the
  class `Unit`.

- `timeScope`:

  The scope of the fit. Must be an instance of the class `Scope`.

- `dataPoints`:

  A list holding the data points for the fit. The items in the list must
  be instances of the class `DataPoint`.

- `excludedDataPoints`:

  A list of excluded data points. The items in the list must be
  instances of the class `DataPoint`.

- `cumulativeDataPoints`:

  A list holding the cumulative data points for the fit. The items in
  the list must be instances of the class `DataPoint`.

- `fitRangeStart`:

  The start date of the fit range (string).

- `fitRangeEnd`:

  The end date of the fit range (string).

- `fitType`:

  The type of the fit.

- `isAutomatic`:

  This flag specifies whether the fit was created automatically.

- `isReviewed`:

  This flag specifies whether the fit was reviewed.

- `isReviewNeeded`:

  This flag specifies whether a review of the fit is needed.

- `intercept`:

  The value of the fit's y-intercept.

- `slope`:

  The value of the fit's slope.

- `hyperbolicConstant`:

  The value of the fit's hyperbolic constant.

- `rootSquared`:

  The RS value of the fit.

- `meanAbsoluteError`:

  The fit's mean absolute error.

- `rootMeanSquareError`:

  The fit's RMS error.

- `estimatedUltimateRecovery`:

  The EUR calculated from the fit.

- `remainingReserves`:

  The remaining reserves based on the fit.

- `economicLimit`:

  The economic limit for the calculation of EUR and remaining reserves.

- `leftBehind`:

  The reserves that are left behind.

- `nowDate`:

  The current date.

- `forecastEndDate`:

  The end date of the forecast.

- `isLocked`:

  This flag specifies whether the DCA fit is locked. Defaults to
  `FALSE`.

- `user`:

  The user the DCA fit belongs to.

- `isFavorite`:

  This flag specifies whether the DCA fit is marked as favorite item,
  and thus shown in the favorites tab on the home module in PetroVisor.
  Defaults to `FALSE`.

- `labels`:

  A list of strings holding the labels of the DCA fit.

## Methods

### Public methods

- [`DcaFit$new()`](#method-DcaFit-new)

- [`DcaFit$toList()`](#method-DcaFit-toList)

- [`DcaFit$clone()`](#method-DcaFit-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new DCAFit instance.

#### Usage

    DcaFit$new(
      name = NULL,
      entity = NULL,
      rateSignal = NULL,
      rateUnit = NULL,
      cumulativeSignal = NULL,
      cumulativeUnit = NULL,
      timeScope = NULL,
      dataPoints = NULL,
      excludedDataPoints = NULL,
      cumulativeDataPoints = NULL,
      fitRangeStart = NULL,
      fitRangeEnd = NULL,
      fitType = NULL,
      isAutomatic = NULL,
      isReviewed = NULL,
      isReviewNeeded = NULL,
      intercept = NULL,
      slope = NULL,
      hyperbolicConstant = NULL,
      rootSquared = NULL,
      meanAbsoluteError = NULL,
      rootMeanSquareError = NULL,
      estimatedUltimateRecovery = NULL,
      remainingReserves = NULL,
      economicLimit = NULL,
      leftBehind = NULL,
      nowDate = NULL,
      forecastEndDate = NULL,
      isLocked = FALSE,
      user = NULL,
      isFavorite = FALSE,
      labels = NULL
    )

#### Arguments

- `name`:

  The name of the DCA fit.

- `entity`:

  The entity for which the DCA fit is created. Must be an instance of
  the class `Entity`.

- `rateSignal`:

  The signal giving the production rate. Must be an instance of the
  class `Signal`.

- `rateUnit`:

  The unit the rate signal is given in. Must be an instance of the class
  `Unit`.

- `cumulativeSignal`:

  The signal giving the cumulative data. Must be an instance of the
  class `Signal`.

- `cumulativeUnit`:

  The unit the cumulative signal is given in. Must be an instance of the
  class `Unit`.

- `timeScope`:

  The scope of the fit. Must be an instance of the class `Scope`.

- `dataPoints`:

  A list holding the data points for the fit. The items in the list must
  be instances of the class `DataPoint`.

- `excludedDataPoints`:

  A list of excluded data points. The items in the list must be
  instances of the class `DataPoint`.

- `cumulativeDataPoints`:

  A list holding the cumulative data points for the fit. The items in
  the list must be instances of the class `DataPoint`.

- `fitRangeStart`:

  The start date of the fit range (string).

- `fitRangeEnd`:

  The end date of the fit range (string).

- `fitType`:

  The type of the fit.

- `isAutomatic`:

  This flag specifies whether the fit was created automatically.

- `isReviewed`:

  This flag specifies whether the fit was reviewed.

- `isReviewNeeded`:

  This flag specifies whether a review of the fit is needed.

- `intercept`:

  The value of the fit's y-intercept.

- `slope`:

  The value of the fit's slope.

- `hyperbolicConstant`:

  The value of the fit's hyperbolic constant.

- `rootSquared`:

  The RS value of the fit.

- `meanAbsoluteError`:

  The fit's mean absolute error.

- `rootMeanSquareError`:

  The fit's RMS error.

- `estimatedUltimateRecovery`:

  The EUR calculated from the fit.

- `remainingReserves`:

  The remaining reserves based on the fit.

- `economicLimit`:

  The economic limit for the calculation of EUR and remaining reserves.

- `leftBehind`:

  The reserves that are left behind.

- `nowDate`:

  The current date.

- `forecastEndDate`:

  The end date of the forecast.

- `isLocked`:

  This flag specifies whether the DCA fit is locked. Defaults to
  `FALSE`.

- `user`:

  The user the DCA fit belongs to.

- `isFavorite`:

  This flag specifies whether the DCA fit is marked as favorite item,
  and thus shown in the favorites tab on the home module in PetroVisor.
  Defaults to `FALSE`.

- `labels`:

  A list of strings holding the labels of the DCA fit.

------------------------------------------------------------------------

### Method `toList()`

#### Usage

    DcaFit$toList()

#### Details

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DcaFit$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
DcaFit$new( )
} # }
```
