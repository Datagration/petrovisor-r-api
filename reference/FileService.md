# FileService

Provides access to file related functionality provided through the web
API.

## Value

A character vector containing the names of the file in the workspace's
blob storage.

The content of the file as string.

## Details

A new instance of this class will be created by the ServiceProvider
automatically.

## See also

- [ServiceProvider](https://datagration.github.io/petrovisor-r-api/reference/ServiceProvider.md)
  for accessing the file service via `sp$files`

- [`vignette("getting-started")`](https://datagration.github.io/petrovisor-r-api/articles/getting-started.md)
  for basic usage

## Super class

[`Myrconn.PetroVisor.Client::ApiRequests`](https://datagration.github.io/petrovisor-r-api/reference/ApiRequests.md)
-\> `FileService`

## Methods

### Public methods

- [`FileService$new()`](#method-FileService-new)

- [`FileService$load_names()`](#method-FileService-load_names)

- [`FileService$load()`](#method-FileService-load)

- [`FileService$save()`](#method-FileService-save)

- [`FileService$delete()`](#method-FileService-delete)

- [`FileService$clone()`](#method-FileService-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new FileService instance. This is done by the
[ServiceProvider](https://datagration.github.io/petrovisor-r-api/reference/ServiceProvider.md)
automatically.

#### Usage

    FileService$new()

------------------------------------------------------------------------

### Method `load_names()`

Retrieve the names of the files in the workspace's blob storage.

#### Usage

    FileService$load_names(prefix = NULL)

#### Arguments

- `prefix`:

  If specified, only file names with the given prefix are returned.

------------------------------------------------------------------------

### Method [`load()`](https://rdrr.io/r/base/load.html)

Download the file with the specified name from the workspace's blob
storage.

#### Usage

    FileService$load(name, target_path = "")

#### Arguments

- `name`:

  The name of the file to download.

- `target_path`:

  The path to download the file to.

------------------------------------------------------------------------

### Method [`save()`](https://rdrr.io/r/base/save.html)

Upload a file to the workspace's blob storage.

#### Usage

    FileService$save(file)

#### Arguments

- `file`:

  The file to upload (path incl. file name).

------------------------------------------------------------------------

### Method `delete()`

Delete the file with the specified name from the workspace's blob
storage.

#### Usage

    FileService$delete(name)

#### Arguments

- `name`:

  The name of the file to delete.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FileService$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# create a new instance of the service provider
sp <- ServiceProvider$new(
  url = discovery_url,
  workspace = workspace,
  user = user,
  password = password
)

# load file
file <- sp$files$load("Test_File.csv")

# save file
result <- sp$files$save("Test_File.csv")
} # }
```
