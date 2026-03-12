# MLTrainingNumericOutcome

Class representing a numeric training outcome for machine learning
models. Contains the name and value of a training metric.

## Public fields

- `name`:

  The name of the outcome (required, string, max 255 chars).

- `value`:

  The value of the metric (numeric, optional).

## Methods

### Public methods

- [`MLTrainingNumericOutcome$new()`](#method-MLTrainingNumericOutcome-new)

- [`MLTrainingNumericOutcome$toList()`](#method-MLTrainingNumericOutcome-toList)

- [`MLTrainingNumericOutcome$clone()`](#method-MLTrainingNumericOutcome-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new MLTrainingNumericOutcome instance.

#### Usage

    MLTrainingNumericOutcome$new(name, value = NULL)

#### Arguments

- `name`:

  The name of the outcome (required, string, max 255 chars).

- `value`:

  The value of the metric (numeric, optional).

------------------------------------------------------------------------

### Method `toList()`

Convert the object to a list for API compatibility.

#### Usage

    MLTrainingNumericOutcome$toList()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MLTrainingNumericOutcome$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
