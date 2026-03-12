# MlTrainingService

Provides access to PetroVisor model training functionalities.

## Details

This service provides methods to train machine learning models, publish
trained models, and make predictions. It uses the singleton AuthContext
for authentication instead of requiring auth parameters.

## See also

Related classes:

- [ServiceProvider](https://datagration.github.io/petrovisor-r-api/reference/ServiceProvider.md)
  for accessing the ML service via `sp$ml`

- [MLModel](https://datagration.github.io/petrovisor-r-api/reference/MLModel.md)
  for model configuration

- [MLTrainingOptions](https://datagration.github.io/petrovisor-r-api/reference/MLTrainingOptions.md)
  for training configuration

- [MLTrainingContext](https://datagration.github.io/petrovisor-r-api/reference/MLTrainingContext.md)
  for training context definitions

- [MLTrainingFeature](https://datagration.github.io/petrovisor-r-api/reference/MLTrainingFeature.md)
  for feature definitions

- [MLTrainingOutcome](https://datagration.github.io/petrovisor-r-api/reference/MLTrainingOutcome.md)
  for outcome metric definitions

- [MLTrainingResult](https://datagration.github.io/petrovisor-r-api/reference/MLTrainingResult.md)
  and
  [MLTrainingResults](https://datagration.github.io/petrovisor-r-api/reference/MLTrainingResults.md)
  for training results

- [DataServices](https://datagration.github.io/petrovisor-r-api/reference/DataServices.md)
  for loading training data

- [RepositoryService](https://datagration.github.io/petrovisor-r-api/reference/RepositoryService.md)
  for managing ML models

Vignettes:

- [`vignette("machine-learning")`](https://datagration.github.io/petrovisor-r-api/articles/machine-learning.md)
  for comprehensive ML guide

- [`vignette("getting-started")`](https://datagration.github.io/petrovisor-r-api/articles/getting-started.md)
  for basic usage

## Super class

[`Myrconn.PetroVisor.Client::ApiRequests`](https://datagration.github.io/petrovisor-r-api/reference/ApiRequests.md)
-\> `MlTrainingService`

## Methods

### Public methods

- [`MlTrainingService$new()`](#method-MlTrainingService-new)

- [`MlTrainingService$train()`](#method-MlTrainingService-train)

- [`MlTrainingService$get_training_status()`](#method-MlTrainingService-get_training_status)

- [`MlTrainingService$get_training_results()`](#method-MlTrainingService-get_training_results)

- [`MlTrainingService$save_best_model()`](#method-MlTrainingService-save_best_model)

- [`MlTrainingService$predict()`](#method-MlTrainingService-predict)

- [`MlTrainingService$clone()`](#method-MlTrainingService-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new MlTrainingService instance. This is done by the
[ServiceProvider](https://datagration.github.io/petrovisor-r-api/reference/ServiceProvider.md)
automatically. The service uses the singleton
[AuthContext](https://datagration.github.io/petrovisor-r-api/reference/AuthContext.md)
for authentication.

#### Usage

    MlTrainingService$new()

------------------------------------------------------------------------

### Method `train()`

Train machine learning models.

#### Usage

    MlTrainingService$train(model)

#### Arguments

- `model`:

  An
  [MLModel](https://datagration.github.io/petrovisor-r-api/reference/MLModel.md)
  object with configured training options.

#### Returns

Training response from the API containing request ID.

------------------------------------------------------------------------

### Method `get_training_status()`

Get the status of a model training request.

#### Usage

    MlTrainingService$get_training_status(request_id)

#### Arguments

- `request_id`:

  The ID of the model training request.

#### Returns

The status of the model training request.

------------------------------------------------------------------------

### Method `get_training_results()`

Get model training results.

#### Usage

    MlTrainingService$get_training_results(request_id)

#### Arguments

- `request_id`:

  The ID of the model training request.

#### Returns

MLTrainingResults object containing structured training results.

------------------------------------------------------------------------

### Method `save_best_model()`

Save the selected model.

#### Usage

    MlTrainingService$save_best_model(model, best_models, request_id)

#### Arguments

- `model`:

  The
  [MLModel](https://datagration.github.io/petrovisor-r-api/reference/MLModel.md)
  object to update and save.

- `best_models`:

  A list of best model results, typically from
  MLTrainingResults\$get_best_models(). Each item should contain model
  outcome, features, trainer name, trained model, training context, and
  confusion matrix.

- `request_id`:

  The ID of the model training request (character).

#### Returns

The API response from updating the model training state (typically a
list).

------------------------------------------------------------------------

### Method [`predict()`](https://rdrr.io/r/stats/predict.html)

Make predictions using trained models.

#### Usage

    MlTrainingService$predict(model_name, prediction_data, entity_name = NULL)

#### Arguments

- `model_name`:

  The name of the model to make predictions on (character).

- `prediction_data`:

  The data to make predictions on.

- `entity_name`:

  The name of the entity to make predictions on.

#### Returns

Prediction results from the API. description Parse raw training results
into structured R6 objects param raw_result Raw API response from
training results endpoint param request_id The training request ID
return MLTrainingResults object description Parse a single training
result item param raw_item Raw result item from API return
MLTrainingResult object description Parse outcome data param raw_outcome
Raw outcome data from API return MLTrainingOutcome object description
Parse metric values array param raw_values Raw values array from API
return List of MLTrainingNumericOutcome objects description Parse
features array param raw_features Raw features array from API return
List of MLTrainingFeature objects description Parse a single feature
param raw_feature Raw feature data from API return MLTrainingFeature
object description Parse feature weights array param raw_weights Raw
weights array from API return List of MLTrainingNumericOutcome objects
description Parse training context data param raw_context Raw training
context data from API return MLTrainingContext object or NULL

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MlTrainingService$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# The service is created automatically by ServiceProvider
sp <- ServiceProvider$new(url = "...", ...)

# Create a model with training options
model <- MLModel$new(
  name = "Test Model",
  table_formula = "Table \"TestData\" ... End Table",
  label_column_name = "target"
)

# Set scope and entity set for training
scope <- Scope$new(name = "TestScope", ...)
entity_set <- EntitySet$new(name = "TestEntities", ...)
model$set_training_options(scope, entity_set)

# Train the model
request <- sp$ml$train(model)

# Check training status
status <- sp$ml$get_training_status(request$id)

# Get training results
results <- sp$ml$get_training_results(request$id)

# Save the best model
best_models <- results$get_best_models()
sp$ml$save_best_model(model, best_models, request$id)

# Make predictions
predictions <- sp$ml$predict(model$name, predictor_values)
} # }
```
