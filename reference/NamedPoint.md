# NamedPoint

Class representing a point with name, tag, x- and y-value.

## Public fields

- `name`:

  The name of the point.

- `tagName`:

  The name of the point's tag.

- `x`:

  The x-value of the point.

- `y`:

  The y-value of the point.

## Methods

### Public methods

- [`NamedPoint$new()`](#method-NamedPoint-new)

- [`NamedPoint$toList()`](#method-NamedPoint-toList)

- [`NamedPoint$clone()`](#method-NamedPoint-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new NamedPoint instance.

#### Usage

    NamedPoint$new(name = NULL, tagName = NULL, x = NULL, y = NULL)

#### Arguments

- `name`:

  The name of the point.

- `tagName`:

  The name of the point's tag

- `x`:

  The x-value of the point.

- `y`:

  The y-value of the point.

------------------------------------------------------------------------

### Method `toList()`

Convert the object to a list. This function is mainly used by the
Services to convert the objects to lists and then call the web API.

#### Usage

    NamedPoint$toList()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    NamedPoint$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
NamedPoint$new(name = "MyPoint", tagName = "MyTag", x = 10, y = 20)
} # }
```
