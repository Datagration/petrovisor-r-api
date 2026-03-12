# MLTrainingResults

Represents the complete ML training results collection.

## Public fields

- `runs_metrics_group_by`:

  Training runs grouping (nullable).

- `results`:

  List of MLTrainingResult objects.

- `label_unit_name`:

  Unit name for the label.

- `status`:

  The training status (added by API wrapper).

- `request_id`:

  The training request ID (added by API wrapper).

## Methods

### Public methods

- [`MLTrainingResults$new()`](#method-MLTrainingResults-new)

- [`MLTrainingResults$get_best_models()`](#method-MLTrainingResults-get_best_models)

- [`MLTrainingResults$toList()`](#method-MLTrainingResults-toList)

- [`MLTrainingResults$clone()`](#method-MLTrainingResults-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new MLTrainingResults instance.

#### Usage

    MLTrainingResults$new(
      runs_metrics_group_by = NULL,
      results = list(),
      label_unit_name = NULL,
      status = NULL,
      request_id = NULL
    )

#### Arguments

- `runs_metrics_group_by`:

  Training runs grouping (nullable).

- `results`:

  List of MLTrainingResult objects.

- `label_unit_name`:

  Unit name for the label.

- `status`:

  The training status (added by API wrapper).

- `request_id`:

  The training request ID (added by API wrapper).

------------------------------------------------------------------------

### Method `get_best_models()`

Get the best models from the results.

#### Usage

    MLTrainingResults$get_best_models()

#### Returns

List of MLTrainingResult objects that are marked as best models.

------------------------------------------------------------------------

### Method `toList()`

Convert the object to a list for API compatibility.

#### Usage

    MLTrainingResults$toList()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MLTrainingResults$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
