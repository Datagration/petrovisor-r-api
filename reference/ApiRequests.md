# ApiRequests

Performs REST API calls

## Methods

### Public methods

- [`ApiRequests$new()`](#method-ApiRequests-new)

- [`ApiRequests$clone()`](#method-ApiRequests-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new ApiRequests instance. description Issue a POST request to
the api

param body The body of the post request. param url The base url of the
API. param route The route to use. E.g. Signal, Files, etc. param
token_type The type of the used token. param token The token used for
authenticating the request. param expect_data Whether to expect data
being returned from the request. param query Named list of additional
query parameters. description Upload a file using a POST request to the
api

param file The file to upload (path incl. file name). param url The base
url of the API. param route The route to use. E.g. Signal, Files, etc.
param token_type The type of the used token. param token The token used
for authenticating the request. description Issue a GET request to the
api

param url The base url of the API. param route The route to use. E.g.
Signal, Files, etc. param token_type The type of the used token. param
token The token used for authenticating the request. param query named
list of query parameters. param parse_json Whether to parse the result
of the request. Defaults to `TRUE`. If set to `FALSE`, the raw content
will be returned.

returns The result of the GET request. Either as received, or parsed to
an object using
[`jsonlite::fromJSON`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html).
description Issue a DELETE request to the api

param name Name of the item to delete. param url The base url of the
API. param route The route to use. E.g. Signal, Files, etc. param
token_type The type of the used token. param token The token used for
authenticating the request. param query named list of query parameters.
description Issue a PUT request to the api

param body The body of the request. param url The base url of the API.
param route The route to use. E.g. Signal, Files, etc. param token_type
The type of the used token. param token The token used for
authenticating the request. param query named list of query parameters.

#### Usage

    ApiRequests$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ApiRequests$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
