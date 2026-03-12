# ScheduleRecurrence

Class representing a schedule recurrence object.

## Public fields

- `recurrenceType`:

  The type of the recurrence (string).

- `intervalsAmount`:

  Run the schedule every n days.

- `skipWeekend`:

  Flag specifying whether to skip execution on weekends.

- `repeatOnSpecificDayMonth`:

  Flag specifying whether to reapeat the schedule on a specific day of a
  month.

- `specificDayNumber`:

  The number of the day the schedule shall be repeated on if
  `repeatOnSpecificDayMonth` is `TRUE`.

- `orderNumberDay`:

  ??

- `typeDayMonthDay`:

  ??

- `runOnMonday`:

  Flag specifying if the schedule is run on Mondays.

- `runOnTuesday`:

  Flag specifying if the schedule is run on Tuesdays.

- `runOnWednesday`:

  Flag specifying if the schedule is run on Wednesdays.

- `runOnThursday`:

  Flag specifying if the schedule is run on Thursdays.

- `runOnFriday`:

  Flag specifying if the schedule is run on Fridays.

- `runOnSaturday`:

  Flag specifying if the schedule is run on Saturdays.

- `runOnSunday`:

  Flag specifying if the schedule is run on Sundays.

## Methods

### Public methods

- [`ScheduleRecurrence$new()`](#method-ScheduleRecurrence-new)

- [`ScheduleRecurrence$toList()`](#method-ScheduleRecurrence-toList)

- [`ScheduleRecurrence$clone()`](#method-ScheduleRecurrence-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new schedule recurrence instance.

#### Usage

    ScheduleRecurrence$new(
      recurrenceType = NULL,
      intervalsAmount = NULL,
      skipWeekend = NULL,
      repeatOnSpecificDayMonth = NULL,
      specificDayNumber = NULL,
      orderNumberDay = NULL,
      typeDayMonthDay = NULL,
      runOnMonday = NULL,
      runOnTuesday = NULL,
      runOnWednesday = NULL,
      runOnThursday = NULL,
      runOnFriday = NULL,
      runOnSaturday = NULL,
      runOnSunday = NULL
    )

#### Arguments

- `recurrenceType`:

  The type of the recurrence (string).

- `intervalsAmount`:

  Run the schedule every n days.

- `skipWeekend`:

  Flag specifying whether to skip execution on weekends.

- `repeatOnSpecificDayMonth`:

  Flag specifying whether to reapeat the schedule on a specific day of a
  month.

- `specificDayNumber`:

  The number of the day the schedule shall be repeated on if
  `repeatOnSpecificDayMonth` is `TRUE`.

- `orderNumberDay`:

  ??

- `typeDayMonthDay`:

  ??

- `runOnMonday`:

  Flag specifying if the schedule is run on Mondays.

- `runOnTuesday`:

  Flag specifying if the schedule is run on Tuesdays.

- `runOnWednesday`:

  Flag specifying if the schedule is run on Wednesdays.

- `runOnThursday`:

  Flag specifying if the schedule is run on Thursdays.

- `runOnFriday`:

  Flag specifying if the schedule is run on Fridays.

- `runOnSaturday`:

  Flag specifying if the schedule is run on Saturdays.

- `runOnSunday`:

  Flag specifying if the schedule is run on Sundays.

------------------------------------------------------------------------

### Method `toList()`

#### Usage

    ScheduleRecurrence$toList()

#### Details

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ScheduleRecurrence$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
ScheduleRecurrence$new()
} # }
```
