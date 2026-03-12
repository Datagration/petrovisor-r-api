# ConfigurationSetting

Class representing a PetroVisor ConfigurationSetting object.

## Public fields

- `name`:

  The name of the configuration setting.

- `numeric_value`:

  The numeric value of the configuration setting.

- `string_value`:

  The string value of the configuration setting.

- `list_value`:

  The list value of the configuration setting (type list).

- `enumeration_value`:

  The enumeration value of the configuration setting (named list).

- `dictionary_value`:

  The dictionary value of the configuration setting (named list).

- `value_type`:

  The value type of the configuration setting. Must be one of:
  `"Numeric"`, `"String"`, `"NumericWithUnit"`, `"List"`,
  `"Enumeration"`, `"Dictionary"`, `"StringWithType"`, `"ListWithType"`,
  `"StringFromValues"`, `"ListFromValues"`

- `unit_name`:

  The name of the unit associated with the numeric value if the
  valueType is `"Numeric with unit"`.

- `possible_values`:

  A list containing the possible values of the configuration setting.

- `is_system`:

  This flaf specifies, whether the configuration setting is a PetroVisor
  system setting (immutable).

- `description`:

  The description of the item.

- `labels`:

  A list of strings holding the labels of the item.

## Methods

### Public methods

- [`ConfigurationSetting$new()`](#method-ConfigurationSetting-new)

- [`ConfigurationSetting$toList()`](#method-ConfigurationSetting-toList)

- [`ConfigurationSetting$clone()`](#method-ConfigurationSetting-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new ConfigurationSetting instance.

#### Usage

    ConfigurationSetting$new(
      name = NULL,
      numeric_value = NULL,
      string_value = NULL,
      list_value = list(),
      enumeration_value = list(),
      dictionary_value = list(),
      value_type = c("Numeric", "String", "NumericWithUnit", "List", "Enumeration",
        "Dictionary", "StringWithType", "ListWithType", "StringFromValues", "ListFromValues"),
      unit_name = NULL,
      possible_values = list(),
      is_system = NULL,
      description = NULL,
      labels = list()
    )

#### Arguments

- `name`:

  The name of the configuration setting.

- `numeric_value`:

  The numeric value of the configuration setting.

- `string_value`:

  The string value of the configuration setting.

- `list_value`:

  The list value of the configuration setting.

- `enumeration_value`:

  The enumeration value of the configuration setting (named list).

- `dictionary_value`:

  The dictionary value of the configuration setting (named list).

- `value_type`:

  The value type of the configuration setting. Must be one of:
  `"Numeric"`, `"String"`, `"NumericWithUnit"`, `"List"`,
  `"Enumeration"`, `"Dictionary"`, `"StringWithType"`, `"ListWithType"`,
  `"StringFromValues"`, `"ListFromValues"`

- `unit_name`:

  The name of the unit associated with the numeric value if the
  valueType is `"Numeric with unit"`.

- `possible_values`:

  A list containing the possible values of the configuration setting.

- `is_system`:

  This flaf specifies, whether the configuration setting is a PetroVisor
  system setting (immutable).

- `description`:

  The description of the item.

- `labels`:

  A list of strings holding the labels of the item.

------------------------------------------------------------------------

### Method `toList()`

#### Usage

    ConfigurationSetting$toList()

#### Details

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ConfigurationSetting$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
ConfigurationSetting$new(name = "MyStringValue",
                         value_type = "String",
                         string_value = "Hello World!")
} # }
```
