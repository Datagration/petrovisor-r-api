# Hierarchy

Class representing a PetroVisor hierarchy object.

## See also

- [RepositoryService](https://datagration.github.io/petrovisor-r-api/reference/RepositoryService.md)
  for loading and saving hierarchies

- [Context](https://datagration.github.io/petrovisor-r-api/reference/Context.md)
  for using hierarchies in data contexts

- [Entity](https://datagration.github.io/petrovisor-r-api/reference/Entity.md)
  for entity definitions

- [`vignette("repository-service")`](https://datagration.github.io/petrovisor-r-api/articles/repository-service.md)
  for hierarchy examples

## Public fields

- `name`:

  The name of the hierarchy.

- `relationship`:

  The child-parent relationships stored in the hierarchy. A single data
  frame with the columns `child` and `parent` if the hierarchy is
  static. For time-dependent hierarchies a list of data frames. The
  names of the list refer to the date of the relationship.

- `is_time_dependent`:

  Whether the hierarchy is time dependent or static.

- `time_stamp`:

  The (first) time stamp of the time dependent hierarchy.

- `description`:

  The description of the item.

- `labels`:

  A list of strings holding the labels of the scope.

## Methods

### Public methods

- [`Hierarchy$new()`](#method-Hierarchy-new)

- [`Hierarchy$toList()`](#method-Hierarchy-toList)

- [`Hierarchy$clone()`](#method-Hierarchy-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new Scope instance.

#### Usage

    Hierarchy$new(
      name = NULL,
      relationship = NULL,
      is_time_dependent = FALSE,
      time_stamp = NULL,
      description = NULL,
      labels = list()
    )

#### Arguments

- `name`:

  The name of the hierarchy.

- `relationship`:

  The child-parent relationships stored in the hierarchy. A single data
  frame with the columns `child` and `parent` if the hierarchy is
  static. For time-dependent hierarchies a list of data frames. The
  names of the list refer to the date of the relationship.

- `is_time_dependent`:

  Whether the hierarchy is time dependent or static.

- `time_stamp`:

  The (first) time stamp of the time dependent hierarchy.

- `description`:

  The description of the item.

- `labels`:

  A list of strings holding the labels of the scope.

------------------------------------------------------------------------

### Method `toList()`

#### Usage

    Hierarchy$toList()

#### Details

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Hierarchy$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
Hierarchy$new(name = "MyHierarchy",
              relationship = list(Well1 = "Parent1",
                                  Well2 = "Parent1",
                                  Parent1 = NA))
} # }
```
