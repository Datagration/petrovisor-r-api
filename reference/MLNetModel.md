# MLNetModel

Class representing a .NET ML model with comprehensive API field handling
for trained models.

## Super class

[`Myrconn.PetroVisor.Client::ApiRequests`](https://datagration.github.io/petrovisor-r-api/reference/ApiRequests.md)
-\> `MLNetModel`

## Public fields

- `name`:

  The name of the ML.NET model (required, max 255 chars).

- `is_reviewed`:

  Whether trained model was reviewed.

- `metrics`:

  Training metrics measured on the validation set (list of
  MLTrainingNumericOutcome).

- `test_metrics`:

  Training metrics measured on the test set (list of
  MLTrainingNumericOutcome).

- `features`:

  Feature contributions, weights, biases for trained models (list of
  MLModelFeature).

- `trainer_name`:

  Algorithm name used for training (nullable).

- `trained_mlnet_model`:

  Trained model in byte format.

- `training_context`:

  Model training context (MLTrainingContext object).

- `confusion_matrix`:

  Confusion matrix for classification models.

## Methods

### Public methods

- [`MLNetModel$new()`](#method-MLNetModel-new)

- [`MLNetModel$toList()`](#method-MLNetModel-toList)

- [`MLNetModel$clone()`](#method-MLNetModel-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new MLNetModel instance.

#### Usage

    MLNetModel$new(
      name,
      is_reviewed = NULL,
      metrics = list(),
      test_metrics = list(),
      features = list(),
      trainer_name = NULL,
      trained_mlnet_model = NULL,
      training_context = NULL,
      confusion_matrix = NULL
    )

#### Arguments

- `name`:

  The name of the ML.NET model (required, max 255 chars).

- `is_reviewed`:

  Whether trained model was reviewed.

- `metrics`:

  Training metrics measured on the validation set.

- `test_metrics`:

  Training metrics measured on the test set.

- `features`:

  Feature contributions, weights, biases for trained models.

- `trainer_name`:

  Algorithm name used for training (nullable).

- `trained_mlnet_model`:

  Trained model in byte format.

- `training_context`:

  Model training context (MLTrainingContext object).

- `confusion_matrix`:

  Confusion matrix for classification models.

------------------------------------------------------------------------

### Method `toList()`

Convert the MLNetModel instance to a list suitable for API submission.

#### Usage

    MLNetModel$toList()

#### Returns

A list representation of the MLNetModel object compatible with the
PetroVisor API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MLNetModel$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a MLNetModel instance
net_model <- MLNetModel$new(
  name = "My NetML Model"
)

# Set metrics
net_model$set_metrics("Accuracy", 0.95, "F1Score", 0.92)

# Set features
net_model$set_features(
  list(
    MLModelFeature$new(name = "feature1", importance = 0.8),
    MLModelFeature$new(name = "feature2", importance = 0.6)
  )
)

# Convert to list for API submission
api_data <- net_model$toList()
} # }
```
