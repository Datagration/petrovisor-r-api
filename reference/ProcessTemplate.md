# ProcessTemplate

Class representing a PetroVisor process template object.

## Public fields

- `name`:

  The name of the process template.

- `description`:

  The discription of the process template.

- `processTemplateGroup`:

  The group of the process template.

- `dueIntervals`:

  The interval unti a step is due.

- `priority`:

  The priority of the process.

- `severity`:

  The severity of the process.

- `steps`:

  A list containing the definition of the steps of the process template.
  The items of the list must be instances of the class `Step`.

- `customFields`:

  A list of custom fields of the process template. The items of the list
  must be instances of the class `CustomField`.

## Methods

### Public methods

- [`ProcessTemplate$new()`](#method-ProcessTemplate-new)

- [`ProcessTemplate$toList()`](#method-ProcessTemplate-toList)

- [`ProcessTemplate$clone()`](#method-ProcessTemplate-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new ProcessTemplate instance.

#### Usage

    ProcessTemplate$new(
      name = NULL,
      description = NULL,
      processTemplateGroup = NULL,
      dueIntervals = NULL,
      priority = NULL,
      severity = NULL,
      steps = NULL,
      customFields = NULL
    )

#### Arguments

- `name`:

  The name of the process template.

- `description`:

  The discription of the process template.

- `processTemplateGroup`:

  The group of the process template.

- `dueIntervals`:

  The interval unti a step is due.

- `priority`:

  The priority of the process.

- `severity`:

  The severity of the process.

- `steps`:

  A list containing the definition of the steps of the process template.
  The items of the list must be instances of the class `Step`.

- `customFields`:

  A list of custom fields of the process template. The items of the list
  must be instances of the class `CustomField`.

------------------------------------------------------------------------

### Method `toList()`

#### Usage

    ProcessTemplate$toList()

#### Details

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ProcessTemplate$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
ProcessTemplate$new()
} # }
```
