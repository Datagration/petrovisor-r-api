# Context

Class representing a PetroVisor context object.

## See also

- [Scope](https://datagration.github.io/petrovisor-r-api/reference/Scope.md)
  for scope definitions

- [EntitySet](https://datagration.github.io/petrovisor-r-api/reference/EntitySet.md)
  for entity set definitions

- [Hierarchy](https://datagration.github.io/petrovisor-r-api/reference/Hierarchy.md)
  for hierarchy definitions

- [RepositoryService](https://datagration.github.io/petrovisor-r-api/reference/RepositoryService.md)
  for loading and saving contexts

- [DataServices](https://datagration.github.io/petrovisor-r-api/reference/DataServices.md)
  for using contexts in data queries

- [`vignette("working-with-data")`](https://datagration.github.io/petrovisor-r-api/articles/working-with-data.md)
  for data examples

## Public fields

- `name`:

  The name of the context.

- `entity_set`:

  The context's
  [EntitySet](https://datagration.github.io/petrovisor-r-api/reference/EntitySet.md)
  defining which entities to include.

- `scope`:

  The context's
  [Scope](https://datagration.github.io/petrovisor-r-api/reference/Scope.md)
  defining the time or depth range.

- `hierarchy`:

  (Optional) The
  [Hierarchy](https://datagration.github.io/petrovisor-r-api/reference/Hierarchy.md)
  used for automatic aggregation.

- `loading_scenario_name`:

  (Optional) The name of the loading scenario.

- `saving_scenario_name`:

  (Optional) The name of the saving scenario.

- `scenario_data_only`:

  (Optional) Whether to load data from the specified loading scenario
  only. If `FALSE`, data will be merged with workspace data.

- `formula`:

  The context's definition as string (P# syntax).

- `description`:

  The description of the item.

- `labels`:

  A list of strings holding the labels of the context.

## Methods

### Public methods

- [`Context$new()`](#method-Context-new)

- [`Context$toList()`](#method-Context-toList)

- [`Context$clone()`](#method-Context-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new Context instance.

#### Usage

    Context$new(
      name = NULL,
      entity_set = NULL,
      scope = NULL,
      hierarchy = NULL,
      loading_scenario_name = NULL,
      saving_scenario_name = NULL,
      scenario_data_only = FALSE,
      formula = NULL,
      description = NULL,
      labels = list()
    )

#### Arguments

- `name`:

  The name of the context.

- `entity_set`:

  The context's
  [EntitySet](https://datagration.github.io/petrovisor-r-api/reference/EntitySet.md)
  defining which entities to include.

- `scope`:

  The context's
  [Scope](https://datagration.github.io/petrovisor-r-api/reference/Scope.md)
  defining the time or depth range.

- `hierarchy`:

  (Optional) The
  [Hierarchy](https://datagration.github.io/petrovisor-r-api/reference/Hierarchy.md)
  used for automatic aggregation.

- `loading_scenario_name`:

  (Optional) The name of the loading scenario.

- `saving_scenario_name`:

  (Optional) The name of the saving scenario.

- `scenario_data_only`:

  (Optional) Whether to load data from the specified loading scenario
  only. If `FALSE`, data will be merged with workspace data.

- `formula`:

  The context's definition as string (P# syntax).

- `description`:

  The description of the item.

- `labels`:

  A list of strings holding the labels of the context.

------------------------------------------------------------------------

### Method `toList()`

#### Usage

    Context$toList()

#### Details

Convert the object to a list. This function is mainly used by the
[RepositoryService](https://datagration.github.io/petrovisor-r-api/reference/RepositoryService.md)
to convert the objects to lists and then call the web API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Context$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
Context$new(name = "MyContext",
          entity_set = EntitySet$new(name = "MyEntities",
              entities = c(Entity$new(name = "Well1",
                                      entity_type_name = "Well",
                                      alias = "WellAlias1"),
                           Entity$new(name = "Well2",
                                      entity_type_name = "Well",
                                      alias = "WellAlias2"))),
          scope = Scope$new(name = "MyScope",
                            start = "2020-01-01T00:00:00.000Z",
                            end = "2020-03-01T00:00:00.000Z",
                            time_increment = "Daily"))
} # }
```
