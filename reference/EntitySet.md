# EntitySet

Class representing a PetroVisor entity set object.

## See also

- [Entity](https://datagration.github.io/petrovisor-r-api/reference/Entity.md)
  for entity definitions

- [RepositoryService](https://datagration.github.io/petrovisor-r-api/reference/RepositoryService.md)
  for managing entity sets

- [Scope](https://datagration.github.io/petrovisor-r-api/reference/Scope.md)
  for time/depth filtering

- [MLModel](https://datagration.github.io/petrovisor-r-api/reference/MLModel.md)
  for ML training context

- [`vignette("repository-service")`](https://datagration.github.io/petrovisor-r-api/articles/repository-service.md)
  for examples

## Public fields

- `name`:

  The name of the entity set.

- `entities`:

  List of
  [Entity](https://datagration.github.io/petrovisor-r-api/reference/Entity.md)
  objects.

- `formula`:

  The entity set's definition as string (P# syntax).

- `description`:

  The description of the item.

- `labels`:

  A list of strings holding the labels of the entity set.

## Methods

### Public methods

- [`EntitySet$new()`](#method-EntitySet-new)

- [`EntitySet$toList()`](#method-EntitySet-toList)

- [`EntitySet$clone()`](#method-EntitySet-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new EntitySet instance.

#### Usage

    EntitySet$new(
      name = NULL,
      entities = NULL,
      formula = NULL,
      description = NULL,
      labels = list()
    )

#### Arguments

- `name`:

  The name of the entity set.

- `entities`:

  List of
  [Entity](https://datagration.github.io/petrovisor-r-api/reference/Entity.md)
  objects.

- `formula`:

  The entity set's definition as string (P# syntax).

- `description`:

  The description of the item.

- `labels`:

  A list of strings holding the labels of the entity set.

------------------------------------------------------------------------

### Method `toList()`

#### Usage

    EntitySet$toList()

#### Details

Convert the object to a list. This function is mainly used by the
[RepositoryService](https://datagration.github.io/petrovisor-r-api/reference/RepositoryService.md)
to convert the objects to lists and then call the web API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    EntitySet$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
EntitySet$new(name = "MyEntities",
              entities = c(Entity$new(name = "Well1",
                                      entity_type_name = "Well",
                                      alias = "WellAlias1"),
                           Entity$new(name = "Well2",
                                      entity_type_name = "Well",
                                      alias = "WellAlias2")))
} # }
```
