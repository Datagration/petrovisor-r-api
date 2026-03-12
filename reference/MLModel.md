# MLModel

Class representing a PetroVisor ML model with minimal required inputs
and comprehensive API field handling.

## See also

- [MlTrainingService](https://datagration.github.io/petrovisor-r-api/reference/MlTrainingService.md)
  for training and managing ML models

- [MLTrainingResult](https://datagration.github.io/petrovisor-r-api/reference/MLTrainingResult.md)
  for training results

- [MLTrainingResults](https://datagration.github.io/petrovisor-r-api/reference/MLTrainingResults.md)
  for multiple training results

- [MLTrainingOptions](https://datagration.github.io/petrovisor-r-api/reference/MLTrainingOptions.md)
  for training configuration

- [MLPreProcessor](https://datagration.github.io/petrovisor-r-api/reference/MLPreProcessor.md)
  for preprocessing methods

- [Scope](https://datagration.github.io/petrovisor-r-api/reference/Scope.md)
  for defining training context

- [EntitySet](https://datagration.github.io/petrovisor-r-api/reference/EntitySet.md)
  for entity filtering

- [`vignette("machine-learning")`](https://datagration.github.io/petrovisor-r-api/articles/machine-learning.md)
  for ML workflow examples

## Super class

[`Myrconn.PetroVisor.Client::ApiRequests`](https://datagration.github.io/petrovisor-r-api/reference/ApiRequests.md)
-\> `MLModel`

## Public fields

- `name`:

  The name of the ML model (required).

- `type`:

  The model's type (defaults to "Regression").

- `table_formula`:

  The model's table formula in P# language (required).

- `context_formula`:

  The model's context formula in P# language (optional - can be
  reconstructed from scope and entity_set).

- `description`:

  Optional description of the model.

- `label_column_name`:

  The label/target column name. Required for: "Regression",
  "BinaryClassification", "MultipleClassification", "NaiveBayes",
  "NaiveBayesCategorical". Not used for: "Clustering". Not required for:
  "SurvivalAnalysis".

- `survival_data`:

  Survival data configuration (required for SurvivalAnalysis).

- `labels`:

  List of string labels for the model.

- `is_model_per_entity`:

  Whether to create model per entity (default FALSE).

- `test_data_mode`:

  Test data mode (default "Union").

- `validation_data_mode`:

  Validation data mode (default "Union").

- `outlier_filters`:

  Outlier filtering method (default "None").

- `trained_models`:

  List of trained models.

- `is_depth_data`:

  Whether model uses depth data.

- `is_automatic`:

  Whether model was trained automatically.

- `trained`:

  Training timestamp.

- `is_reviewed`:

  Whether model has been reviewed.

- `include_incomplete_cases`:

  Whether to include incomplete cases.

- `data_provider_config`:

  Data provider configuration.

- `training_options`:

  Complete training options configuration with all sub-fields.

- `validation_scope_formula`:

  Validation scope formula.

- `validation_entity_set_formula`:

  Validation entity set formula.

- `test_scope_formula`:

  Test scope formula.

- `test_entity_set_formula`:

  Test entity set formula.

## Methods

### Public methods

- [`MLModel$new()`](#method-MLModel-new)

- [`MLModel$toList()`](#method-MLModel-toList)

- [`MLModel$set_training_options()`](#method-MLModel-set_training_options)

- [`MLModel$set_preprocessing()`](#method-MLModel-set_preprocessing)

- [`MLModel$set_outlier_filtering()`](#method-MLModel-set_outlier_filtering)

- [`MLModel$set_neural_network_options()`](#method-MLModel-set_neural_network_options)

- [`MLModel$set_custom_training()`](#method-MLModel-set_custom_training)

- [`MLModel$set_trainer_hyperparameters()`](#method-MLModel-set_trainer_hyperparameters)

- [`MLModel$get_summary()`](#method-MLModel-get_summary)

- [`MLModel$clone()`](#method-MLModel-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new MLModel instance with minimal required parameters.

#### Usage

    MLModel$new(
      name,
      table_formula,
      type = "Regression",
      context_formula = NULL,
      description = NULL,
      label_column_name = NULL,
      survival_data = NULL,
      labels = list()
    )

#### Arguments

- `name`:

  The name of the ML model (required).

- `table_formula`:

  The model's table formula in P# language (required).

- `type`:

  The model's type. One of "Regression", "BinaryClassification",
  "MultipleClassification", "Clustering", "NaiveBayes",
  "NaiveBayesCategorical", "SurvivalAnalysis". Defaults to "Regression".

- `context_formula`:

  The model's context formula in P# language (optional - can be
  reconstructed from scope and entity_set).

- `description`:

  Optional description of the model.

- `label_column_name`:

  The label/target column name. Required for most model types except
  Clustering.

- `survival_data`:

  Survival data configuration. Required for SurvivalAnalysis.

- `labels`:

  List of string labels for the model (default empty list).

------------------------------------------------------------------------

### Method `toList()`

Convert the MLModel instance to a list suitable for API submission.

#### Usage

    MLModel$toList()

#### Returns

A list representation of the MLModel object compatible with the
PetroVisor API.

------------------------------------------------------------------------

### Method `set_training_options()`

Configure training options for model validation and testing

#### Usage

    MLModel$set_training_options(
      scope,
      entity_set,
      test_fraction = 0,
      validation_fraction = 0,
      time_to_train = 60,
      max_models = 1000
    )

#### Arguments

- `scope`:

  The training scope (Scope object, required)

- `entity_set`:

  The training entity set (EntitySet object, required)

- `test_fraction`:

  Fraction of data to use for testing (0.0-1.0)

- `validation_fraction`:

  Fraction of data to use for validation (0.0-1.0)

- `time_to_train`:

  Maximum time to train in seconds (default 60)

- `max_models`:

  Maximum number of models to train (default 1000)

------------------------------------------------------------------------

### Method `set_preprocessing()`

Enable specific data preprocessing methods

#### Usage

    MLModel$set_preprocessing(
      methods = c(),
      apply_pre_processors_before_training = TRUE
    )

#### Arguments

- `methods`:

  Vector of preprocessing method names to enable. Options: "MinMax",
  "MeanVariance", "LogMeanVariance", "Binning", "SupervisedBinning",
  "RobustScaling", "LpNorm", "GlobalContrast",
  "ProjectToPrincipalComponents", "ApproximatedKernelMap", "BoxCox",
  "BoxTidwell"

- `apply_pre_processors_before_training`:

  Whether to apply preprocessors before training (default TRUE)

------------------------------------------------------------------------

### Method `set_outlier_filtering()`

Set outlier filtering method

#### Usage

    MLModel$set_outlier_filtering(method = c("None", "IQR", "CooksDistance"))

#### Arguments

- `method`:

  Outlier filtering method. Options: "None", "IQR", "CooksDistance"

------------------------------------------------------------------------

### Method `set_neural_network_options()`

Configure neural network training options

#### Usage

    MLModel$set_neural_network_options(
      library = "TensorFlow",
      model_path = NULL,
      transfer_learning = FALSE,
      hyperparameter_tuning = FALSE,
      features_mapping = NULL,
      optimizer = "Adam",
      learning_rate = 0.001,
      epochs = 100,
      batch_size = 32,
      sequence_window_size = NULL,
      activation_function = "ReLU",
      dense_layers = 2,
      neurons = 64,
      conv1d_layers = NULL,
      conv1d_filters = NULL,
      conv1d_kernel_size = NULL,
      lstm_layers = NULL,
      lstm_neurons = NULL
    )

#### Arguments

- `library`:

  Neural network library name (e.g., "TensorFlow")

- `model_path`:

  Path to the neural network model file

- `transfer_learning`:

  Whether to use transfer learning

- `hyperparameter_tuning`:

  Whether to enable hyperparameter tuning

- `features_mapping`:

  Features and label names mapping from internal names to trained model
  names

- `optimizer`:

  Optimizer to use (e.g. "Adam", "SGD")

- `learning_rate`:

  Learning rate (0-1)

- `epochs`:

  Number of training epochs

- `batch_size`:

  Batch size for training

- `sequence_window_size`:

  Series sequence window size

- `activation_function`:

  Activation function

- `dense_layers`:

  Number of dense layers

- `neurons`:

  Number of neurons per layer

- `conv1d_layers`:

  Number of Conv1D layers (for time series)

- `conv1d_filters`:

  Number of Conv1D filters

- `conv1d_kernel_size`:

  Conv1D kernel size

- `lstm_layers`:

  Number of LSTM layers

- `lstm_neurons`:

  Number of LSTM neurons

------------------------------------------------------------------------

### Method `set_custom_training()`

Set custom training options for specific algorithms

#### Usage

    MLModel$set_custom_training(
      training_type = "Auto",
      l2_regularization = NULL,
      enable_pruning = NULL,
      entropy_coefficient = NULL,
      learning_rate = NULL,
      maximum_bin_count_per_feature = NULL,
      minimum_example_count_per_leaf = NULL,
      max_iterations = NULL,
      pruning_metrics = NULL,
      rank = NULL,
      oversampling = NULL,
      ensure_zero_mean = TRUE,
      onnx_model = NULL
    )

#### Arguments

- `training_type`:

  Training type: "Auto", "Ols", "Gam", "RandomizedPca", "NeuralNetwork",
  "OnnxModel"

- `l2_regularization`:

  OLS: L2 regularization weight (0-1) for ridge regression

- `enable_pruning`:

  GAM: Enable post-training tree pruning to avoid overfitting

- `entropy_coefficient`:

  GAM: The entropy (regularization) coefficient (0-1)

- `learning_rate`:

  GAM: The learning rate (0-1)

- `maximum_bin_count_per_feature`:

  GAM: Maximum number of distinct values (bins) per feature (2-500)

- `minimum_example_count_per_leaf`:

  GAM: Minimal number of data points required to form a new tree leaf
  (1-100)

- `max_iterations`:

  GAM: Total number of passes over the training data (100-20000)

- `pruning_metrics`:

  GAM: Metric to use for pruning ("LeastAbsoluteDeviation" or
  "LeastSquares")

- `rank`:

  Randomized PCA: The number of components in the PCA

- `oversampling`:

  Randomized PCA: Oversampling parameter for randomized PCA training
  (1-200)

- `ensure_zero_mean`:

  Randomized PCA: If TRUE, data is centered to have zero mean (defaults
  to TRUE)

- `onnx_model`:

  ONNX model string (for ONNX models)

------------------------------------------------------------------------

### Method `set_trainer_hyperparameters()`

Configure hyperparameters for specific trainers

#### Usage

    MLModel$set_trainer_hyperparameters(trainer_name, hyperparameters)

#### Arguments

- `trainer_name`:

  Name of the trainer (e.g. "FastForest", "LightGbm")

- `hyperparameters`:

  Named list of hyperparameter values

------------------------------------------------------------------------

### Method `get_summary()`

Get a summary of the model configuration

#### Usage

    MLModel$get_summary()

#### Returns

List containing key model configuration details description Create
default training options with comprehensive field structure using
MLTrainingOptions R6 class param scope The training scope (required)
param entity_set The training entity set (required) return
MLTrainingOptions instance representing default training options
description Generate context formula from scope and entity_set param
scope The training scope param entity_set The training entity set return
Context formula string description Create default preprocessors
structure return List of MLPreProcessor instances description Update
optimization metric based on model type param type Model type
description Merge API training options with default structure param
default_options Default training options structure param api_options
Training options from API return Merged training options list
description Create default trainer hyperparameters structure return List
of default trainer hyperparameters description Get trainers for a given
type param type Model type return List of trainers for the given type

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MLModel$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a minimal ML model (Regression)
table_formula <- 'Table "WellData"
  Column "feature1" in "m"
    "Production Feature" in "m"
  End Column
  Column "target" in "bbl"
    "Production Target" in "bbl"
  End Column
End Table'

model <- MLModel$new(
  name = "My Production Model",
  table_formula = table_formula,
  label_column_name = "target",
  description = "Production prediction model"
)

# Create scope and entity set
scope <- Scope$new(
  name = "TestScope",
  start = "2023-01-01T00:00:00.000Z",
  end = "2023-12-31T00:00:00.000Z",
  time_increment = "Daily",
  depth_increment = "Meter",
  start_depth = 0,
  end_depth = 0,
  formula = 'Scope "TestScope"
    Between #01/01/2023#
    And #31/12/2023#
    Step Daily
  End Scope'
)

entity_set <- EntitySet$new(
  name = "TestEntities",
  entities = list(),
  formula = 'Entity Set "TestEntities"
  End Set'
)

# Set training options with scope and entity_set (required)
model$set_training_options(scope, entity_set,
                          test_fraction = 0.2,
                          validation_fraction = 0.2,
                          time_to_train = 300)

# Configure preprocessing
model$set_preprocessing(c("MinMax", "MeanVariance", "RobustScaling"))

# Configure outlier filtering
model$set_outlier_filtering("CooksDistance")

# Configure neural network (optional)
model$set_neural_network_options(
  library = "TensorFlow",
  optimizer = "Adam",
  learning_rate = 0.001,
  epochs = 100,
  batch_size = 64,
  dense_layers = 3,
  neurons = 128
)

# Convert to list for API submission
api_data <- model$toList()

# Save model using service provider
sp$items$save("MLModel", model)

# Load model from API
retrieved_model <- sp$items$load("MLModel", "My Production Model")

# Configure custom training (alternative to neural network)
model$set_custom_training(
  training_type = "Gam",
  enable_pruning = TRUE,
  learning_rate = 0.05,
  max_iterations = 1500
)
} # }
```
