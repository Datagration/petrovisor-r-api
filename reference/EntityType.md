# EntityType

Class representing a PetroVisor entityType object.

## See also

- [Entity](https://datagration.github.io/petrovisor-r-api/reference/Entity.md)
  for entity definitions

- [RepositoryService](https://datagration.github.io/petrovisor-r-api/reference/RepositoryService.md)
  for loading entity types

- [`vignette("repository-service")`](https://datagration.github.io/petrovisor-r-api/articles/repository-service.md)
  for examples

## Public fields

- `name`:

  The name of the entity type.

- `image`:

  The image of the entity type represented as string.

## Methods

### Public methods

- [`EntityType$new()`](#method-EntityType-new)

- [`EntityType$toList()`](#method-EntityType-toList)

- [`EntityType$clone()`](#method-EntityType-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new EntityType instance.

#### Usage

    EntityType$new(name = NULL, image = NULL)

#### Arguments

- `name`:

  The name of entity type.

- `image`:

  The image of the entity type represented as string.

------------------------------------------------------------------------

### Method `toList()`

#### Usage

    EntityType$toList()

#### Details

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    EntityType$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
EntityType$new(name = "Section")
} # }
```
