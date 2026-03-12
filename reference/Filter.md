# Filter

Class representing a PetroVisor filter object.

## Public fields

- `name`:

  The name of the filter.

- `entityNamePattern`:

  The pattern to filter the available entities by.

- `signalNamePattern`:

  The pattern to filter the available signals by.

- `entitySetName`:

  The name of the selected entity set.

- `checkedEntityNames`:

  The names of the checked entities.

- `checkedSignalNames`:

  The names of the checked signals.

- `checkedUnitNames`:

  The names of the checked units.

- `start`:

  The filter's start date.

- `end`:

  The filter's end date.

- `step`:

  The filter's time increment. Allowed values are: `"EveryMinute"`,
  `"EveryFiveMinutes"`, `"EveryFifteenMinutes"`, `"Hourly"`, `"Daily"`,
  `"Monthly"`, `"Quarterly"`, `"Yearly"`

- `isLocked`:

  This flag specifies whether the filter is locked. Defaults to `FALSE`.

- `user`:

  The user the filter belongs to.

- `isFavorite`:

  This flag specifies whether the filter is marked as favorite item, and
  thus shown in the favorites tab on the home module in PetroVisor.
  Defaults to `FALSE`.

- `labels`:

  A list of strings holding the labels of the filter.

## Methods

### Public methods

- [`Filter$new()`](#method-Filter-new)

- [`Filter$toList()`](#method-Filter-toList)

- [`Filter$clone()`](#method-Filter-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new Filter instance.

#### Usage

    Filter$new(
      name = NULL,
      entityNamePattern = NULL,
      signalNamePattern = NULL,
      entitySetName = NULL,
      checkedEntityNames = NULL,
      checkedSignalNames = NULL,
      checkedUnitNames = NULL,
      start = NULL,
      end = NULL,
      step = c("EveryMinute", "EveryFiveMinutes", "EveryFifteenMinutes", "Hourly", "Daily",
        "Monthly", "Quarterly", "Yearly"),
      isLocked = FALSE,
      user = NULL,
      isFavorite = FALSE,
      labels = NULL
    )

#### Arguments

- `name`:

  The name of the filter.

- `entityNamePattern`:

  The pattern to filter the available entities by.

- `signalNamePattern`:

  The pattern to filter the available signals by.

- `entitySetName`:

  The name of the selected entity set.

- `checkedEntityNames`:

  The names of the checked entities.

- `checkedSignalNames`:

  The names of the checked signals.

- `checkedUnitNames`:

  The names of the checked units.

- `start`:

  The filter's start date.

- `end`:

  The filter's end date.

- `step`:

  The filter's time increment. Allowed values are: `"EveryMinute"`,
  `"EveryFiveMinutes"`, `"EveryFifteenMinutes"`, `"Hourly"`, `"Daily"`,
  `"Monthly"`, `"Quarterly"`, `"Yearly"`

- `isLocked`:

  This flag specifies whether the filter is locked. Defaults to `FALSE`.

- `user`:

  The user the filter belongs to.

- `isFavorite`:

  This flag specifies whether the filter is marked as favorite item, and
  thus shown in the favorites tab on the home module in PetroVisor.
  Defaults to `FALSE`.

- `labels`:

  A list of strings holding the labels of the filter.

------------------------------------------------------------------------

### Method `toList()`

#### Usage

    Filter$toList()

#### Details

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Filter$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
Filter$new(name = "MyFilter",
           entitySetName = "All Wells",
           checkedSignalNames = list("oil rate", "gas rate"),
           checkedUnitNames = list("m3/d", "m3/d"),
           start = "2020-01-01T00:00:00.000Z",
           end = "2020-03-01T00:00:00.000Z",
           step = "Daily")
} # }
```
