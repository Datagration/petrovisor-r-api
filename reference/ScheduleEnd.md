# ScheduleEnd

Class representing a schedule end object.

## Public fields

- `endType`:

  The type of the schedule end (integer).

- `repetitionsCount`:

  The number of times the schedule was executed.

- `totalRepetitions`:

  The number of times the schedule shall be executed.

- `endDate`:

  The date when the schedule shall end.

## Methods

### Public methods

- [`ScheduleEnd$new()`](#method-ScheduleEnd-new)

- [`ScheduleEnd$toList()`](#method-ScheduleEnd-toList)

- [`ScheduleEnd$clone()`](#method-ScheduleEnd-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new schedule end instance.

#### Usage

    ScheduleEnd$new(
      endType = NULL,
      repetitionsCount = NULL,
      totalRepetitions = NULL,
      endDate = NULL
    )

#### Arguments

- `endType`:

  The type of the schedule end (integer).

- `repetitionsCount`:

  The number of times the schedule was executed.

- `totalRepetitions`:

  The number of times the schedule shall be executed.

- `endDate`:

  The date when the schedule shall end.

------------------------------------------------------------------------

### Method `toList()`

#### Usage

    ScheduleEnd$toList()

#### Details

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ScheduleEnd$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
ScheduleEnd$new()
} # }
```
