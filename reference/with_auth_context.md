# With Authentication Context

Execute code block with temporary authentication context

## Usage

``` r
with_auth_context(
  token,
  token_type,
  workspace_data_url,
  code,
  user = NULL,
  workspace = NULL
)
```

## Arguments

- token:

  The access token

- token_type:

  The token type

- workspace_data_url:

  The workspace URL

- code:

  Code block to execute

- user:

  Optional user name

- workspace:

  Optional workspace name

## Value

Result of the code block execution

## Details

This function allows you to temporarily set an authentication context
for testing or special operations, then restore the previous context.

## Examples

``` r
if (FALSE) { # \dontrun{
# Execute code with temporary auth context
result <- with_auth_context(
  token = "test-token",
  token_type = "Bearer",
  workspace_data_url = "https://test.api.com/",
  user = "test-user",
  {
    # Code that needs authentication
    some_authenticated_operation()
  }
)
} # }
```
