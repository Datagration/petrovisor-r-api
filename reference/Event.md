# Event

Class representing a event object.

## Public fields

- `name`:

  The name of the event.

- `formula`:

  The event's formula.

## Methods

### Public methods

- [`Event$new()`](#method-Event-new)

- [`Event$toList()`](#method-Event-toList)

- [`Event$clone()`](#method-Event-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new Event instance.

#### Usage

    Event$new(name = NULL, formula = NULL)

#### Arguments

- `name`:

  The name of the cleansing filter.

- `formula`:

  The cleansing filter's formula.

------------------------------------------------------------------------

### Method `toList()`

#### Usage

    Event$toList()

#### Details

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Event$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
Event$new()
} # }
```
