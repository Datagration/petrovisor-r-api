# MLTrainingFeature

Represents a feature with importance and weights in ML training results.

## Public fields

- `name`:

  The name of the feature.

- `importance`:

  The importance value of the feature.

- `weights`:

  List of MLTrainingNumericOutcome objects.

## Methods

### Public methods

- [`MLTrainingFeature$new()`](#method-MLTrainingFeature-new)

- [`MLTrainingFeature$toList()`](#method-MLTrainingFeature-toList)

- [`MLTrainingFeature$clone()`](#method-MLTrainingFeature-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new MLTrainingFeature instance.

#### Usage

    MLTrainingFeature$new(name = NULL, importance = NULL, weights = list())

#### Arguments

- `name`:

  The name of the feature.

- `importance`:

  The importance value of the feature.

- `weights`:

  List of MLTrainingNumericOutcome objects.

------------------------------------------------------------------------

### Method `toList()`

Convert the object to a list for API compatibility.

#### Usage

    MLTrainingFeature$toList()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MLTrainingFeature$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
