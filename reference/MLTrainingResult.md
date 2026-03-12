# MLTrainingResult

Represents a single training result.

## Public fields

- `entity_name`:

  Entity name (nullable).

- `trained_model`:

  Trained model (byte format).

- `training_context`:

  Model training context (MLTrainingContext object).

- `confusion_matrix`:

  Confusion matrix for classification models.

- `outcome`:

  Training metrics (MLTrainingOutcome object).

- `error`:

  Training error (nullable).

- `trainer_name`:

  Trainer name.

- `features`:

  List of MLTrainingFeature objects.

## Methods

### Public methods

- [`MLTrainingResult$new()`](#method-MLTrainingResult-new)

- [`MLTrainingResult$toList()`](#method-MLTrainingResult-toList)

- [`MLTrainingResult$clone()`](#method-MLTrainingResult-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new MLTrainingResult instance.

#### Usage

    MLTrainingResult$new(
      entity_name = NULL,
      trained_model = NULL,
      training_context = NULL,
      confusion_matrix = NULL,
      outcome = NULL,
      error = NULL,
      trainer_name = NULL,
      features = list()
    )

#### Arguments

- `entity_name`:

  Entity name (nullable).

- `trained_model`:

  Trained model (byte format).

- `training_context`:

  Model training context (MLTrainingContext object).

- `confusion_matrix`:

  Confusion matrix for classification models.

- `outcome`:

  Training metrics (MLTrainingOutcome object).

- `error`:

  Training error (nullable).

- `trainer_name`:

  Trainer name.

- `features`:

  List of MLTrainingFeature objects.

------------------------------------------------------------------------

### Method `toList()`

Convert the object to a list for API compatibility.

#### Usage

    MLTrainingResult$toList()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MLTrainingResult$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
