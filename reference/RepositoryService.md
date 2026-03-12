# RepositoryService

Provides access to PetroVisor item related functionalities.

## Details

A new instance of this class will be created by the ServiceProvider
automatically.

## See also

Related classes:

- [ServiceProvider](https://datagration.github.io/petrovisor-r-api/reference/ServiceProvider.md)
  for creating a service provider instance

- [DataServices](https://datagration.github.io/petrovisor-r-api/reference/DataServices.md)
  for loading and saving data using repository items

Item classes that can be managed:

- [Entity](https://datagration.github.io/petrovisor-r-api/reference/Entity.md)
  for well, field, reservoir entities

- [EntityType](https://datagration.github.io/petrovisor-r-api/reference/EntityType.md)
  for entity type definitions

- [EntitySet](https://datagration.github.io/petrovisor-r-api/reference/EntitySet.md)
  for entity collections

- [Signal](https://datagration.github.io/petrovisor-r-api/reference/Signal.md)
  for signal definitions

- [Unit](https://datagration.github.io/petrovisor-r-api/reference/Unit.md)
  and
  [UnitMeasurement](https://datagration.github.io/petrovisor-r-api/reference/UnitMeasurement.md)
  for unit definitions

- [Hierarchy](https://datagration.github.io/petrovisor-r-api/reference/Hierarchy.md)
  for organizational structures

- [Scope](https://datagration.github.io/petrovisor-r-api/reference/Scope.md)
  for entity and signal selections

- [Scenario](https://datagration.github.io/petrovisor-r-api/reference/Scenario.md)
  for data versions

- [Tag](https://datagration.github.io/petrovisor-r-api/reference/Tag.md)
  and
  [TagEntry](https://datagration.github.io/petrovisor-r-api/reference/TagEntry.md)
  for metadata

- [Workflow](https://datagration.github.io/petrovisor-r-api/reference/Workflow.md)
  for automated processes

- [RScript](https://datagration.github.io/petrovisor-r-api/reference/RScript.md)
  and
  [PSharpScript](https://datagration.github.io/petrovisor-r-api/reference/PSharpScript.md)
  for scripts

- [MLModel](https://datagration.github.io/petrovisor-r-api/reference/MLModel.md)
  for machine learning models

Vignettes:

- [`vignette("repository-service")`](https://datagration.github.io/petrovisor-r-api/articles/repository-service.md)
  for comprehensive repository operations

- [`vignette("getting-started")`](https://datagration.github.io/petrovisor-r-api/articles/getting-started.md)
  for basic usage

## Super class

[`Myrconn.PetroVisor.Client::ApiRequests`](https://datagration.github.io/petrovisor-r-api/reference/ApiRequests.md)
-\> `RepositoryService`

## Methods

### Public methods

- [`RepositoryService$new()`](#method-RepositoryService-new)

- [`RepositoryService$load_names()`](#method-RepositoryService-load_names)

- [`RepositoryService$delete()`](#method-RepositoryService-delete)

- [`RepositoryService$load()`](#method-RepositoryService-load)

- [`RepositoryService$save()`](#method-RepositoryService-save)

- [`RepositoryService$clone()`](#method-RepositoryService-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new RepositoryService instance. This is done by the
[ServiceProvider](https://datagration.github.io/petrovisor-r-api/reference/ServiceProvider.md)
automatically.

#### Usage

    RepositoryService$new()

------------------------------------------------------------------------

### Method `load_names()`

Get the names of all items of the given type available in PetroVisor.

#### Usage

    RepositoryService$load_names(type)

#### Arguments

- `type`:

  The type of the item.

#### Returns

A character array containing the names of the items.

------------------------------------------------------------------------

### Method `delete()`

Delete an item by name.

#### Usage

    RepositoryService$delete(type, name)

#### Arguments

- `type`:

  The type of the item.

- `name`:

  Name of the item to delete.

------------------------------------------------------------------------

### Method [`load()`](https://rdrr.io/r/base/load.html)

Get an item by name.

#### Usage

    RepositoryService$load(type, name)

#### Arguments

- `type`:

  The type of the item.

- `name`:

  Name of the item to retrieve.

#### Returns

An object of the specified type class.

------------------------------------------------------------------------

### Method [`save()`](https://rdrr.io/r/base/save.html)

#### Usage

    RepositoryService$save(type, item)

#### Arguments

- `type`:

  The type of the item.

- `item`:

  Item to add or edit. Has to be an object of the respective class.

#### Details

Add or edit an item.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    RepositoryService$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# create a new instance of the service provider
sp <- ServiceProvider$new("Host", 8095, "WorkspaceA", "UserX", "Password")

# get the names of all available entities
entityNames <- sp$items$load_names("Entity")

# delete an item (delete the hierarchy with the name "test")
sp$items$delete("Hierarchy", "test")

# get an item by name
well01 <- sp$items$load("Entity", "Well01")

# add or edit an item
entity <- Entity$new(
  name = "TestWell01",
  entity_type_name = "Well",
  alias = "TestAlias01"
)
sp$items$save("Entity", entity)
} # }
```
