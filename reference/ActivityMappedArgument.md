# ActivityMappedArgument

Class representing an activity mapped argument object.

## Public fields

- `argumentName`:

  The name of the argument.

- `argumentType`:

  The argument type. One of: `TimeSignalUnit`, `DepthSignalUnit`,
  `StaticSignalUnit`, `StringSignalUnit`, `StringTimeSignalUnit`,
  `PVTSignalUnit`, `Hierarchy`, `TagEntries`, `WorkspaceValue`, `Model`

- `acceptableArgumentTypes`:

  A list of strings containing the acceptable argument types.

- `mappedSignalName`:

  The name of the mapped signal.

- `mappedUnitName`:

  The name of the mapped unit.

- `mappedString`:

  The mapped string.

## Methods

### Public methods

- [`ActivityMappedArgument$new()`](#method-ActivityMappedArgument-new)

- [`ActivityMappedArgument$toList()`](#method-ActivityMappedArgument-toList)

- [`ActivityMappedArgument$clone()`](#method-ActivityMappedArgument-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new activity argument instance.

#### Usage

    ActivityMappedArgument$new(
      argumentName = NULL,
      argumentType = c("TimeSignalUnit", "DepthSignalUnit", "StaticSignalUnit",
        "StringSignalUnit", "StringTimeSignalUnit", "PVTSignalUnit", "Hierarchy",
        "TagEntries", "WorkspaceValue", "Model"),
      acceptableArgumentTypes = NULL,
      mappedSignalName = NULL,
      mappedUnitName = NULL,
      mappedString = NULL
    )

#### Arguments

- `argumentName`:

  The name of the argument.

- `argumentType`:

  The argument type. One of: `TimeSignalUnit`, `DepthSignalUnit`,
  `StaticSignalUnit`, `StringSignalUnit`, `StringTimeSignalUnit`,
  `PVTSignalUnit`, `Hierarchy`, `TagEntries`, `WorkspaceValue`, `Model`

- `acceptableArgumentTypes`:

  A list of strings containing the acceptable argument types.

- `mappedSignalName`:

  The name of the mapped signal.

- `mappedUnitName`:

  The name of the mapped unit.

- `mappedString`:

  The mapped string.

------------------------------------------------------------------------

### Method `toList()`

#### Usage

    ActivityMappedArgument$toList()

#### Details

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ActivityMappedArgument$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
ActivityMappedArgument$new()
} # }
```
