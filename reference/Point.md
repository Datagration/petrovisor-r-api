# Point

Class representing a point with x- and y-value.

## Public fields

- `x`:

  The x-value of the point.

- `y`:

  The y-value of the point.

## Methods

### Public methods

- [`Point$new()`](#method-Point-new)

- [`Point$toList()`](#method-Point-toList)

- [`Point$clone()`](#method-Point-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new Point instance.

#### Usage

    Point$new(x = NULL, y = NULL)

#### Arguments

- `x`:

  The x-value of the point.

- `y`:

  The y-value of the point.

------------------------------------------------------------------------

### Method `toList()`

Convert the object to a list. This function is mainly used by the
Services to convert the objects to lists and then call the web API.

#### Usage

    Point$toList()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Point$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
Point$new(x = 10, y = 20)
} # }
```
