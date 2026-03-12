# MLCustomTrainingOptions

Options for custom trainers.

## Public fields

- `l2_regularization`:

  OLS: L2 regularization weight (0-1). Adding L2 regularization turns
  this algorithm into a form of ridge regression.

- `enable_pruning`:

  GAM: Enable post-training tree pruning to avoid overfitting. Requires
  a validation set.

- `entropy_coefficient`:

  GAM: The entropy (regularization) coefficient between 0 and 1.

- `learning_rate`:

  GAM: The learning rate (0-1).

- `maximum_bin_count_per_feature`:

  GAM: The maximum number of distinct values (bins) per feature (2-500).

- `minimum_example_count_per_leaf`:

  GAM: The minimal number of data points required to form a new tree
  leaf (1-100).

- `number_of_iterations`:

  GAM: Total number of passes over the training data (100-20000).

- `pruning_metrics`:

  GAM: Determines what metric to use for pruning. One of
  `"LeastAbsoluteDeviation"`, `"LeastSquares"`.

- `rank`:

  Randomized PCA: The number of components in the PCA.

- `oversampling`:

  Randomized PCA: Oversampling parameter for randomized PCA training
  (1-200).

- `ensure_zero_mean`:

  Randomized PCA: If true, data is centered to have zero mean. Defaults
  to true.

- `onnx_model`:

  Gets or sets the ONNX model (string, nullable).

- `neural_network_training_options`:

  Neural Network Training Options. Object of type
  MLNeuralNetworkTrainingOptions.

## Methods

### Public methods

- [`MLCustomTrainingOptions$new()`](#method-MLCustomTrainingOptions-new)

- [`MLCustomTrainingOptions$toList()`](#method-MLCustomTrainingOptions-toList)

- [`MLCustomTrainingOptions$clone()`](#method-MLCustomTrainingOptions-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new MLCustomTrainingOptions instance.

#### Usage

    MLCustomTrainingOptions$new(
      l2_regularization = NULL,
      enable_pruning = NULL,
      entropy_coefficient = NULL,
      learning_rate = NULL,
      maximum_bin_count_per_feature = NULL,
      minimum_example_count_per_leaf = NULL,
      number_of_iterations = NULL,
      pruning_metrics = NULL,
      rank = NULL,
      oversampling = NULL,
      ensure_zero_mean = TRUE,
      onnx_model = NULL,
      neural_network_training_options = NULL
    )

#### Arguments

- `l2_regularization`:

  OLS: L2 regularization weight (0-1). Adding L2 regularization turns
  this algorithm into a form of ridge regression.

- `enable_pruning`:

  GAM: Enable post-training tree pruning to avoid overfitting. Requires
  a validation set.

- `entropy_coefficient`:

  GAM: The entropy (regularization) coefficient between 0 and 1.

- `learning_rate`:

  GAM: The learning rate (0-1).

- `maximum_bin_count_per_feature`:

  GAM: The maximum number of distinct values (bins) per feature (2-500).

- `minimum_example_count_per_leaf`:

  GAM: The minimal number of data points required to form a new tree
  leaf (1-100).

- `number_of_iterations`:

  GAM: Total number of passes over the training data (100-20000).

- `pruning_metrics`:

  GAM: Determines what metric to use for pruning. One of
  `"LeastAbsoluteDeviation"`, `"LeastSquares"`.

- `rank`:

  Randomized PCA: The number of components in the PCA.

- `oversampling`:

  Randomized PCA: Oversampling parameter for randomized PCA training
  (1-200).

- `ensure_zero_mean`:

  Randomized PCA: If true, data is centered to have zero mean. Defaults
  to true.

- `onnx_model`:

  Gets or sets the ONNX model (string, nullable).

- `neural_network_training_options`:

  Neural Network Training Options. Object of type
  MLNeuralNetworkTrainingOptions.

------------------------------------------------------------------------

### Method `toList()`

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

#### Usage

    MLCustomTrainingOptions$toList()

#### Returns

A list representation of the MLCustomTrainingOptions object compatible
with the API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MLCustomTrainingOptions$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
