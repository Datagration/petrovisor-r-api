# Scope

Class representing a PetroVisor scope object.

## See also

- [RepositoryService](https://datagration.github.io/petrovisor-r-api/reference/RepositoryService.md)
  for managing scopes

- [DataServices](https://datagration.github.io/petrovisor-r-api/reference/DataServices.md)
  for using scopes in data queries

- [EntitySet](https://datagration.github.io/petrovisor-r-api/reference/EntitySet.md)
  for entity filtering

- [MLModel](https://datagration.github.io/petrovisor-r-api/reference/MLModel.md)
  for ML training context

- [`vignette("working-with-data")`](https://datagration.github.io/petrovisor-r-api/articles/working-with-data.md)
  for data examples

- [`vignette("machine-learning")`](https://datagration.github.io/petrovisor-r-api/articles/machine-learning.md)
  for ML examples

## Public fields

- `name`:

  The name of the scope.

- `start`:

  The scope's start date.

- `end`:

  The scope's end date.

- `time_increment`:

  The scope's time increment. Allowed values are: `"EverySecond"`,
  `"EveryMinute"`, `"EveryFiveMinutes"`, `"EveryFifteenMinutes"`,
  `"Hourly"`, `"Daily"`, `"Monthly"`, `"Quarterly"`, `"Yearly"`

- `depth_increment`:

  The scope's depth increment. Allowed values are: `"Meter"`,
  `"HalfMeter"`, `"EighthMeter"`, `"TenthMeter"`, `"Foot"`, `"HalfFoot"`

- `start_depth`:

  The scope's start depth.

- `end_depth`:

  The scope's end depth.

- `formula`:

  The scope's definition as string (P# syntax).

- `description`:

  The description of the item.

- `labels`:

  A list of strings holding the labels of the scope.

## Methods

### Public methods

- [`Scope$new()`](#method-Scope-new)

- [`Scope$toList()`](#method-Scope-toList)

- [`Scope$clone()`](#method-Scope-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new Scope instance.

#### Usage

    Scope$new(
      name = NULL,
      start = NULL,
      end = NULL,
      time_increment = c("", "EverySecond", "EveryMinute", "EveryFiveMinutes",
        "EveryFifteenMinutes", "Hourly", "Daily", "Monthly", "Quarterly", "Yearly"),
      depth_increment = c("", "Meter", "HalfMeter", "EighthMeter", "TenthMeter", "Foot",
        "HalfFoot"),
      start_depth = NULL,
      end_depth = NULL,
      formula = NULL,
      description = NULL,
      labels = list()
    )

#### Arguments

- `name`:

  The name of the scope.

- `start`:

  The scope's start date.

- `end`:

  The scope's end date.

- `time_increment`:

  The scope's time increment. Allowed values are: `"EverySecond"`,
  `"EveryMinute"`, `"EveryFiveMinutes"`, `"EveryFifteenMinutes"`,
  `"Hourly"`, `"Daily"`, `"Monthly"`, `"Quarterly"`, `"Yearly"`

- `depth_increment`:

  The scope's depth increment. Allowed values are: `"Meter"`,
  `"HalfMeter"`, `"EighthMeter"`, `"TenthMeter"`, `"Foot"`, `"HalfFoot"`

- `start_depth`:

  The scope's start depth.

- `end_depth`:

  The scope's end depth.

- `formula`:

  The scope's definition as string (P# syntax).

- `description`:

  The description of the item.

- `labels`:

  A list of strings holding the labels of the scope.

------------------------------------------------------------------------

### Method `toList()`

#### Usage

    Scope$toList()

#### Details

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Scope$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
Scope$new(name = "MyScope",
          start = "2020-01-01T00:00:00.000Z",
          end = "2020-03-01T00:00:00.000Z",
          time_increment = "Daily")
} # }
```
