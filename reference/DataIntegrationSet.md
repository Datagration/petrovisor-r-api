# DataIntegrationSet

Class representing a data integration set.

## Public fields

- `name`:

  The name of the data integration set.

- `overwriteExistingData`:

  This flag specifies whether existing data shall be overwritten by data
  extracted from the source.

- `trackNewEntities`:

  This flag specifies whether new entity shall be tracked in the log.

- `dataSourceNames`:

  A list of all data sources of the data entegration set (names only).

- `cleansingScriptNames`:

  A list of all cleansing scripts applied to the data (names only).

- `timeStep`:

  The time step for the data integration. One of: `"EveryMinute"`,
  `"EveryFiveMinutes"`, `"EveryFifteenMinutes"`, `"Hourly"`, `"Daily"`,
  `"Monthly"`, `"Quarterly"`, `"Yearly"`.

- `depthStep`:

  The depth step for the data integration. One of:
  `"Every tenth of meter"`, `"Every eighth of meter"`,
  `"Every half of foot"`, `"Every foot"`, `"Every half of meter"`,
  `"Every meter"`.

- `timeStart`:

  The start date for the data aquisition.

- `timeEnd`:

  The end date for the data aquisition.

- `depthStart`:

  The start depth for the data aquisition.

- `depthEnd`:

  The end depth for the data aquisition.

- `isLocked`:

  This flag specifies whether the data integration set is locked.
  Defaults to `FALSE`.

- `user`:

  The user the data integration set belongs to.

- `isFavorite`:

  This flag specifies whether the data integration set is marked as
  favorite item, and thus shown in the favorites tab on the home module
  in PetroVisor. Defaults to `FALSE`.

- `labels`:

  A list of strings holding the labels of the data integration set.

## Methods

### Public methods

- [`DataIntegrationSet$new()`](#method-DataIntegrationSet-new)

- [`DataIntegrationSet$toList()`](#method-DataIntegrationSet-toList)

- [`DataIntegrationSet$clone()`](#method-DataIntegrationSet-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new DataIntegrationSet instance.

#### Usage

    DataIntegrationSet$new(
      name = NULL,
      overwriteExistingData = NULL,
      trackNewEntities = NULL,
      dataSourceNames = NULL,
      cleansingScriptNames = NULL,
      timeStep = c("EveryMinute", "EveryFiveMinutes", "EveryFifteenMinutes", "Hourly",
        "Daily", "Monthly", "Quarterly", "Yearly"),
      depthStep = c("Every tenth of meter", "Every eighth of meter", "Every half of foot",
        "Every foot", "Every half of meter", "Every meter"),
      timeStart = NULL,
      timeEnd = NULL,
      depthStart = NULL,
      depthEnd = NULL,
      isLocked = FALSE,
      user = NULL,
      isFavorite = FALSE,
      labels = NULL
    )

#### Arguments

- `name`:

  The name of the data integration set.

- `overwriteExistingData`:

  This flag specifies whether existing data shall be overwritten by data
  extracted from the source.

- `trackNewEntities`:

  This flag specifies whether new entity shall be tracked in the log.

- `dataSourceNames`:

  A list of all data sources of the data entegration set (names only).

- `cleansingScriptNames`:

  A list of all cleansing scripts applied to the data (names only).

- `timeStep`:

  The time step for the data integration. One of: `"EveryMinute"`,
  `"EveryFiveMinutes"`, `"EveryFifteenMinutes"`, `"Hourly"`, `"Daily"`,
  `"Monthly"`, `"Quarterly"`, `"Yearly"`.

- `depthStep`:

  The depth step for the data integration. One of:
  `"Every tenth of meter"`, `"Every eighth of meter"`,
  `"Every half of foot"`, `"Every foot"`, `"Every half of meter"`,
  `"Every meter"`.

- `timeStart`:

  The start date for the data aquisition.

- `timeEnd`:

  The end date for the data aquisition.

- `depthStart`:

  The start depth for the data aquisition.

- `depthEnd`:

  The end depth for the data aquisition.

- `isLocked`:

  This flag specifies whether the data integration set is locked.
  Defaults to `FALSE`.

- `user`:

  The user the data integration set belongs to.

- `isFavorite`:

  This flag specifies whether the data integration set is marked as
  favorite item, and thus shown in the favorites tab on the home module
  in PetroVisor. Defaults to `FALSE`.

- `labels`:

  A list of strings holding the labels of the data integration set.

------------------------------------------------------------------------

### Method `toList()`

Convert the object to a list. This function is mainly used by the
Services to convert the objects to lists and then call the web API.

#### Usage

    DataIntegrationSet$toList()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DataIntegrationSet$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
DataIntegrationSet$new()
} # }
```
