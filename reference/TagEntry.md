# TagEntry

Class representing a PetroVisor tag entry.

## See also

- [Tag](https://datagration.github.io/petrovisor-r-api/reference/Tag.md)
  for tag definitions

- [TagEntriesService](https://datagration.github.io/petrovisor-r-api/reference/TagEntriesService.md)
  for loading and saving tag entries

- [Entity](https://datagration.github.io/petrovisor-r-api/reference/Entity.md)
  for entity definitions

- [RepositoryService](https://datagration.github.io/petrovisor-r-api/reference/RepositoryService.md)
  for repository operations

## Public fields

- `tag_name`:

  The name of the
  [Tag](https://datagration.github.io/petrovisor-r-api/reference/Tag.md).

- `entity_name`:

  The name of the tagged
  [Entity](https://datagration.github.io/petrovisor-r-api/reference/Entity.md).

- `start`:

  The start date of the tag entry.

- `end`:

  The end date of the tag entry.

## Methods

### Public methods

- [`TagEntry$new()`](#method-TagEntry-new)

- [`TagEntry$to_list()`](#method-TagEntry-to_list)

- [`TagEntry$clone()`](#method-TagEntry-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new TagEntry instance.

#### Usage

    TagEntry$new(tag_name = NULL, entity_name = NULL, start = NULL, end = NULL)

#### Arguments

- `tag_name`:

  The name of the
  [Tag](https://datagration.github.io/petrovisor-r-api/reference/Tag.md).

- `entity_name`:

  The name of the tagged
  [Entity](https://datagration.github.io/petrovisor-r-api/reference/Entity.md).

- `start`:

  The start date of the tag entry.

- `end`:

  (Optional) The end date of the tag entry.

------------------------------------------------------------------------

### Method `to_list()`

Convert the object to a list. This function is mainly used by the
TagEntriesService to convert the objects to lists and then call the web
API.

#### Usage

    TagEntry$to_list()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TagEntry$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# create a new instance of the service provider
sp <- ServiceProvider$new("Host", 8095, "WorkspaceA", "UserX", "Password")

# create a new tag entry
TagEntry$new(entity_name = "Well 01" ,
             tag_name = "Active",
             start = "2020-02-01T00:00:00.000Z")
} # }
```
