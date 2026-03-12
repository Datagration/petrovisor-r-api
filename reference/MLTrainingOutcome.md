# MLTrainingOutcome

Represents the outcome metrics from ML training.

## Public fields

- `is_best_model`:

  Whether this is the best model among others.

- `is_best_run`:

  Whether this is the best run (for cross validation folds).

- `group_by`:

  Group TrainingOutcome instances by the value of this property (for
  cross validation folds).

- `sequence`:

  Sequential number (the order this model was trained by auto-ML).

- `values`:

  List of training metric values measured on the validation set.

- `test_values`:

  List of test metric values measured on the test set.

- `is_best_model_test`:

  Whether this is the best among others, measured on the test set.

- `is_best_run_test`:

  Whether this is the best run (for cross validation folds), measured on
  the test set.

## Methods

### Public methods

- [`MLTrainingOutcome$new()`](#method-MLTrainingOutcome-new)

- [`MLTrainingOutcome$toList()`](#method-MLTrainingOutcome-toList)

- [`MLTrainingOutcome$clone()`](#method-MLTrainingOutcome-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new MLTrainingOutcome instance.

#### Usage

    MLTrainingOutcome$new(
      is_best_model = FALSE,
      is_best_run = FALSE,
      group_by = NULL,
      sequence = NULL,
      values = list(),
      test_values = list(),
      is_best_model_test = FALSE,
      is_best_run_test = FALSE
    )

#### Arguments

- `is_best_model`:

  Whether this is the best model among others.

- `is_best_run`:

  Whether this is the best run (for cross validation folds).

- `group_by`:

  Group TrainingOutcome instances by the value of this property (for
  cross validation folds).

- `sequence`:

  Sequential number (the order this model was trained by auto-ML).

- `values`:

  List of training metric values measured on the validation set.

- `test_values`:

  List of test metric values measured on the test set.

- `is_best_model_test`:

  Whether this is the best among others, measured on the test set.

- `is_best_run_test`:

  Whether this is the best run (for cross validation folds), measured on
  the test set.

------------------------------------------------------------------------

### Method `toList()`

Convert the object to a list for API compatibility.

#### Usage

    MLTrainingOutcome$toList()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MLTrainingOutcome$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
