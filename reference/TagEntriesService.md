# TagEntriesService

Provides access to functionality related to tag entries provided through
the web API.

## Details

A new instance of this class will be created by the ServiceProvider
automatically.

## See also

- [ServiceProvider](https://datagration.github.io/petrovisor-r-api/reference/ServiceProvider.md)
  for accessing the tag entries service via `sp$tag_entries`

- [Tag](https://datagration.github.io/petrovisor-r-api/reference/Tag.md)
  for tag definitions

- [TagEntry](https://datagration.github.io/petrovisor-r-api/reference/TagEntry.md)
  for tag entry structure

- [RepositoryService](https://datagration.github.io/petrovisor-r-api/reference/RepositoryService.md)
  for managing tags

- [`vignette("getting-started")`](https://datagration.github.io/petrovisor-r-api/articles/getting-started.md)
  for basic usage

## Super class

[`Myrconn.PetroVisor.Client::ApiRequests`](https://datagration.github.io/petrovisor-r-api/reference/ApiRequests.md)
-\> `TagEntriesService`

## Methods

### Public methods

- [`TagEntriesService$new()`](#method-TagEntriesService-new)

- [`TagEntriesService$save()`](#method-TagEntriesService-save)

- [`TagEntriesService$delete_range()`](#method-TagEntriesService-delete_range)

- [`TagEntriesService$delete()`](#method-TagEntriesService-delete)

- [`TagEntriesService$load()`](#method-TagEntriesService-load)

- [`TagEntriesService$clone()`](#method-TagEntriesService-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new TagEntriesService instance. This is done by the
[ServiceProvider](https://datagration.github.io/petrovisor-r-api/reference/ServiceProvider.md)
automatically.

#### Usage

    TagEntriesService$new()

#### Arguments

- `url`:

  the URL for the API calls.

- `token_type`:

  the type of the issued token.

- `token`:

  the issued token.

------------------------------------------------------------------------

### Method [`save()`](https://rdrr.io/r/base/save.html)

Add new or update single or multiple tag entries at once.

#### Usage

    TagEntriesService$save(tag_entries)

#### Arguments

- `tag_entries`:

  A dataframe specifying the tag entries to save or update. The
  dataframe has to have for columns with the names `tag_name`,
  `entity_name`, `start`, `end`.

------------------------------------------------------------------------

### Method `delete_range()`

Delete all tag entries with a start date in the specified time range.

#### Usage

    TagEntriesService$delete_range(entity_name, tag_name, start, end = NULL)

#### Arguments

- `entity_name`:

  The name of the entity for which to delete the tag entries.

- `tag_name`:

  The name of the tag for which to delete the tag entries.

- `start`:

  The start date of the range.

- `end`:

  (Optional) The end date of the range.

------------------------------------------------------------------------

### Method `delete()`

Delete the specified tag entries.

#### Usage

    TagEntriesService$delete(tag_entries)

#### Arguments

- `tag_entries`:

  A dataframe specifying the tag entries to delete. The dataframe has to
  have for columns with the names `tag_name`, `entity_name`, `start`,
  `end`.

------------------------------------------------------------------------

### Method [`load()`](https://rdrr.io/r/base/load.html)

Get tag entries according to the given filter.

#### Usage

    TagEntriesService$load(
      entity_names = list(),
      entity_set_name = "",
      tag_names = list(),
      tag_group_names = list(),
      start = NULL,
      end = NULL,
      start_date_begins = NULL,
      start_date_ends = NULL,
      end_date_begins = NULL,
      end_date_ends = NULL,
      is_end_date_set = NULL
    )

#### Arguments

- `entity_names`:

  Names of the entities for which tag entries shall be retrieved. Has to
  be specified if `entity_set_name` is not used.

- `entity_set_name`:

  Name of the entity set defining the entities for which tag entries
  shall be retrieved. Has to be specified if `entity_names` is not used.

- `tag_names`:

  Tags for which tag entries shall be retrieved. Has to be specified if
  `tag_group_names` is not used.

- `tag_group_names`:

  Tag groups for which tag entries shall be retrieved. Has to be
  specified if `tag_names` is not used.

- `start`:

  Together with `end` defines a date range that is used to filter tag
  entries. All tag entries overlapping with this date range will be
  returned.

- `end`:

  Together with `start` defines a date range that is used to filter tag
  entries. All tag entries overlapping with this date range will be
  returned.

- `start_date_begins`:

  Together with `start_date_ends` defines a date range for the start
  dates of the tag entries. Tag entries with a start date within this
  date range will be returned.

- `start_date_ends`:

  Together with `start_date_begins` defines a date range for the start
  dates of the tag entries. Tag entries with a start date within this
  date range will be returned.

- `end_date_begins`:

  Together with `end_date_ends` defines a date range for the end dates
  of the tag entries. Tag entries with an end date within this date
  range will be returned.

- `end_date_ends`:

  Together with `end_date_begins` defines a date range for the end dates
  of the tag entries. Tag entries with an end date within this date
  range will be returned.

- `is_end_date_set`:

  Whether to return open or closed tag entries only. If not specified or
  set to NULL all tag entries (closed and ongoing) will be returned.

#### Returns

A dataframe containing the tag entries or a list of length zero if no
tag entries for the specified filter are available.

#### Examples

    \dontrun{
    tagEntries <- sp$tag_entries$load(tag_group_names = c("Info"))
    }

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TagEntriesService$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# create a new instance of the service provider
sp <- ServiceProvider$new("Host", 8095, "WorkspaceA", "UserX", "Password")

# get tag entries of group "Info"
tagEntries <- sp$tag_entries$load(
  tag_group_names = list("Info")
)

# delete tag entries
sp$tag_entries$delete_range(
  entity_name = "Well01",
  tag_name = "Active",
  start = "2020-01-01T00:00:00.000Z"
)
} # }

## ------------------------------------------------
## Method `TagEntriesService$load`
## ------------------------------------------------

if (FALSE) { # \dontrun{
tagEntries <- sp$tag_entries$load(tag_group_names = c("Info"))
} # }
```
