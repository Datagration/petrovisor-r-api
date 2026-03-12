# ActivityArgument

Class representing an activity argument object.

## Public fields

- `argumentName`:

  The name of the argument.

- `acceptableArgumentTypes`:

  A list of strings containing the acceptable argument types.

- `defaultArgumentType`:

  The default argument type. One of: `TimeSignalUnit`,
  `DepthSignalUnit`, `StaticSignalUnit`, `StringSignalUnit`,
  `StringTimeSignalUnit`, `PVTSignalUnit`, `Hierarchy`, `TagEntries`,
  `WorkspaceValue`, `Model`

- `defaultSignalName`:

  The name of the default signal.

- `defaultUnitName`:

  The name of the default unit.

- `defaultString`:

  The default string.

## Methods

### Public methods

- [`ActivityArgument$new()`](#method-ActivityArgument-new)

- [`ActivityArgument$toList()`](#method-ActivityArgument-toList)

- [`ActivityArgument$clone()`](#method-ActivityArgument-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new activity argument instance.

#### Usage

    ActivityArgument$new(
      argumentName = NULL,
      acceptableArgumentTypes = NULL,
      defaultArgumentType = c("TimeSignalUnit", "DepthSignalUnit", "StaticSignalUnit",
        "StringSignalUnit", "StringTimeSignalUnit", "PVTSignalUnit", "Hierarchy",
        "TagEntries", "WorkspaceValue", "Model"),
      defaultSignalName = NULL,
      defaultUnitName = NULL,
      defaultString = NULL
    )

#### Arguments

- `argumentName`:

  The name of the argument.

- `acceptableArgumentTypes`:

  A list of strings containing the acceptable argument types.

- `defaultArgumentType`:

  The default argument type. One of: `TimeSignalUnit`,
  `DepthSignalUnit`, `StaticSignalUnit`, `StringSignalUnit`,
  `StringTimeSignalUnit`, `PVTSignalUnit`, `Hierarchy`, `TagEntries`,
  `WorkspaceValue`, `Model`

- `defaultSignalName`:

  The name of the default signal.

- `defaultUnitName`:

  The name of the default unit.

- `defaultString`:

  The default string.

------------------------------------------------------------------------

### Method `toList()`

#### Usage

    ActivityArgument$toList()

#### Details

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ActivityArgument$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
ActivityArgument$new()
} # }
```
