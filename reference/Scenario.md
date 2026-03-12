# Scenario

Class representing a PetroVisor scenario object.

## Public fields

- `name`:

  The name of the scenario

- `configuration_settings`:

  A list holding configuration items associated with the scenario.
  Objects in this list are of type ConfigurationSetting.

- `description`:

  The description of the item.

- `labels`:

  A list of strings holding the labels of the item.

## Methods

### Public methods

- [`Scenario$new()`](#method-Scenario-new)

- [`Scenario$toList()`](#method-Scenario-toList)

- [`Scenario$clone()`](#method-Scenario-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new Scenario instance.

#### Usage

    Scenario$new(
      name = NULL,
      configuration_settings = list(),
      description = NULL,
      labels = list()
    )

#### Arguments

- `name`:

  The name of the scenario

- `configuration_settings`:

  A list holding configuration items associated with the scenario.
  Objects in this list are of type ConfigurationSetting.

- `description`:

  The description of the item.

- `labels`:

  A list of strings holding the labels of the item.

------------------------------------------------------------------------

### Method `toList()`

#### Usage

    Scenario$toList()

#### Details

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Scenario$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
Scenario$new(
  name = "Test R Scenario",
  description = "Test Scenario")
} # }
```
