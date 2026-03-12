# MLTrainingContext

Represents ML training context information.

## Public fields

- `validation_data`:

  Serialized validation data (byte format).

- `test_data`:

  Serialized test data (byte format).

- `test_data_fraction`:

  Test data fraction (nullable).

- `validation_data_fraction`:

  Validation data fraction (nullable).

## Methods

### Public methods

- [`MLTrainingContext$new()`](#method-MLTrainingContext-new)

- [`MLTrainingContext$toList()`](#method-MLTrainingContext-toList)

- [`MLTrainingContext$clone()`](#method-MLTrainingContext-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new MLTrainingContext instance.

#### Usage

    MLTrainingContext$new(
      validation_data = NULL,
      test_data = NULL,
      test_data_fraction = NULL,
      validation_data_fraction = NULL
    )

#### Arguments

- `validation_data`:

  Serialized validation data (byte format).

- `test_data`:

  Serialized test data (byte format).

- `test_data_fraction`:

  Test data fraction (nullable).

- `validation_data_fraction`:

  Validation data fraction (nullable).

------------------------------------------------------------------------

### Method `toList()`

Convert the object to a list for API compatibility.

#### Usage

    MLTrainingContext$toList()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MLTrainingContext$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
