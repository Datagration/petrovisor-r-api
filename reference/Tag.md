# Tag

Class representing a PetroVisor tag object.

## See also

- [TagEntry](https://datagration.github.io/petrovisor-r-api/reference/TagEntry.md)
  for tag entry definitions

- [TagEntriesService](https://datagration.github.io/petrovisor-r-api/reference/TagEntriesService.md)
  for managing tag entries

- [RepositoryService](https://datagration.github.io/petrovisor-r-api/reference/RepositoryService.md)
  for loading and saving tags

- [Entity](https://datagration.github.io/petrovisor-r-api/reference/Entity.md)
  for entity definitions that can be tagged

## Public fields

- `name`:

  The name of the tag.

- `tag_group`:

  The tag's tag group.

## Methods

### Public methods

- [`Tag$new()`](#method-Tag-new)

- [`Tag$toList()`](#method-Tag-toList)

- [`Tag$clone()`](#method-Tag-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new Tag instance.

#### Usage

    Tag$new(name = NULL, tag_group = NULL)

#### Arguments

- `name`:

  The name of tag.

- `tag_group`:

  The tag's tag group.

------------------------------------------------------------------------

### Method `toList()`

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

#### Usage

    Tag$toList()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Tag$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
Tag$new(name = "NewTag", tag_group = "Group 1")
} # }
```
