# CustomField

Class representing a process template custom field.

## Public fields

- `name`:

  The name of the custom field.

- `evaluatedColumn`:

  The evaluated column. Must be an instance of the class `Column`.

- `evaluatedDate`:

  The date of the evaluation (string).

- `evaluatedValue`:

  The result of the evaluation (string).

- `fieldType`:

  The type of the custom field (integer).

- `scriptName`:

  The name of the script used by the custom field.

- `scriptColumnName`:

  The name of the column used by the custom field.

- `currentManualValue`:

  The current manually entered value.

- `initialValue`:

  The initial value of the custom field.

- `expressionTime`:

  The time of the expression.

- `activeStep`:

  The name of the step the custom field is active in.

- `settingName`:

  The name of the custom field's setting.

## Methods

### Public methods

- [`CustomField$new()`](#method-CustomField-new)

- [`CustomField$toList()`](#method-CustomField-toList)

- [`CustomField$clone()`](#method-CustomField-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new CustomField instance.

#### Usage

    CustomField$new(
      name = NULL,
      evaluatedColumn = NULL,
      evaluatedDate = NULL,
      evaluatedValue = NULL,
      fieldType = NULL,
      scriptName = NULL,
      scriptColumnName = NULL,
      currentManualValue = NULL,
      initialValue = NULL,
      expressionTime = NULL,
      activeStep = NULL,
      settingName = NULL
    )

#### Arguments

- `name`:

  The name of the custom field.

- `evaluatedColumn`:

  The evaluated column. Must be an instance of the class `Column`.

- `evaluatedDate`:

  The date of the evaluation (string).

- `evaluatedValue`:

  The result of the evaluation (string).

- `fieldType`:

  The type of the custom field (integer).

- `scriptName`:

  The name of the script used by the custom field.

- `scriptColumnName`:

  The name of the column used by the custom field.

- `currentManualValue`:

  The current manually entered value.

- `initialValue`:

  The initial value of the custom field.

- `expressionTime`:

  The time of the expression.

- `activeStep`:

  The name of the step the custom field is active in.

- `settingName`:

  The name of the custom field's setting.

------------------------------------------------------------------------

### Method `toList()`

Convert the object to a list. This function is mainly used by the
Services to convert the objects to lists and then call the web API.

#### Usage

    CustomField$toList()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    CustomField$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
CustomField$new()
} # }
```
