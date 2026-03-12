# Entity

Class representing a PetroVisor entity object.

## See also

- [RepositoryService](https://datagration.github.io/petrovisor-r-api/reference/RepositoryService.md)
  for loading and saving entities

- [EntitySet](https://datagration.github.io/petrovisor-r-api/reference/EntitySet.md)
  for grouping entities

- [EntityType](https://datagration.github.io/petrovisor-r-api/reference/EntityType.md)
  for entity type definitions

- [DataServices](https://datagration.github.io/petrovisor-r-api/reference/DataServices.md)
  for working with entity data

- [`vignette("repository-service")`](https://datagration.github.io/petrovisor-r-api/articles/repository-service.md)
  for repository examples

## Public fields

- `name`:

  The name of the entity.

- `entity_type_name`:

  The name of the entity's type.

- `alias`:

  The alias of the entity.

- `is_opportunity`:

  Whether the entity is an opportunity or not.

## Methods

### Public methods

- [`Entity$new()`](#method-Entity-new)

- [`Entity$toList()`](#method-Entity-toList)

- [`Entity$clone()`](#method-Entity-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new Entity instance.

#### Usage

    Entity$new(
      name = NULL,
      entity_type_name = NULL,
      alias = NULL,
      is_opportunity = FALSE
    )

#### Arguments

- `name`:

  The name of entity.

- `entity_type_name`:

  The name of the entity' type.

- `alias`:

  The alias of the entity.

- `is_opportunity`:

  Whether the entity is an opportunity or not.

------------------------------------------------------------------------

### Method `toList()`

#### Usage

    Entity$toList()

#### Details

Convert the object to a list. This function is mainly used by the
[RepositoryService](https://datagration.github.io/petrovisor-r-api/reference/RepositoryService.md)
to convert the objects to lists and then call the web API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Entity$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
Entity$new(
  name = "NewWell",
  entity_type_name = "Well",
  alias = "NewWellAlias",
  is_opportunity = FALSE)
} # }
```
