# MLTrainingOptions

ML model training options.

## Public fields

- `test_fraction`:

  Test set fraction.

- `test_latin_hypercube`:

  Logical indicating whether the test set sampling is latin hypercube
  (if true) or uniform (otherwise).

- `validation_fraction`:

  Validation set fraction.

- `optimization_metric`:

  Name of the optimization metric.

- `time_to_train`:

  Maximal time to train in seconds.

- `num_clusters`:

  Number of clusters (when applicable).

- `num_cv_folds`:

  Number of cross validation folds.

- `trainers_to_exclude`:

  The names of the trainers (algorithms) to exclude.

- `scope`:

  The training scope. Object of type Scope.

- `entity_set`:

  The training entity set. Object of type EntitySet.

- `include_incomplete_cases`:

  Logical whether to include incomplete cases (rows with some features
  equal to null) for training.

- `custom_training_type`:

  Customized training type. One of Auto, Ols, Gam, RandomizedPca,
  GamOva, BernoulliNaiveBayes, OnnxModel, NeuralNetwork.

- `custom_training_options`:

  Options for custom trainer. Object of type MLCustomTrainingOptions.

- `pre_processors`:

  Data pre-processors. List of MLPreProcessor.

- `apply_pre_processors_before_training`:

  When to apply pre-processors: before (true) or after (false) training.

- `tuner`:

  Tuner name.

- `maximum_models_to_train`:

  Maximum models to train.

- `survival_data`:

  Names of the columns containing survival data.

- `trainer_hyperparameters`:

  Trainer hyperparameters.

## Methods

### Public methods

- [`MLTrainingOptions$new()`](#method-MLTrainingOptions-new)

- [`MLTrainingOptions$toList()`](#method-MLTrainingOptions-toList)

- [`MLTrainingOptions$clone()`](#method-MLTrainingOptions-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new MLTrainingOptions instance.

#### Usage

    MLTrainingOptions$new(
      scope,
      entity_set,
      test_fraction = NULL,
      test_latin_hypercube = FALSE,
      validation_fraction = NULL,
      optimization_metric = NULL,
      time_to_train = 60,
      num_clusters = 1,
      num_cv_folds = 1,
      trainers_to_exclude = NULL,
      include_incomplete_cases = FALSE,
      custom_training_type = "Auto",
      custom_training_options = NULL,
      pre_processors = NULL,
      apply_pre_processors_before_training = TRUE,
      tuner = "EciCostFrugal",
      maximum_models_to_train = 1000,
      survival_data = NULL,
      trainer_hyperparameters = NULL
    )

#### Arguments

- `scope`:

  The training scope. Object of type Scope (required).

- `entity_set`:

  The training entity set. Object of type EntitySet (required).

- `test_fraction`:

  Test set fraction.

- `test_latin_hypercube`:

  Logical indicating whether the test set sampling is latin hypercube
  (if true) or uniform (otherwise).

- `validation_fraction`:

  Validation set fraction.

- `optimization_metric`:

  Name of the optimization metric.

- `time_to_train`:

  Maximal time to train in seconds.

- `num_clusters`:

  Number of clusters (when applicable).

- `num_cv_folds`:

  Number of cross validation folds.

- `trainers_to_exclude`:

  The names of the trainers (algorithms) to exclude.

- `include_incomplete_cases`:

  Logical whether to include incomplete cases (rows with some features
  equal to null) for training.

- `custom_training_type`:

  Customized training type. One of Auto, Ols, Gam, RandomizedPca,
  GamOva, BernoulliNaiveBayes, OnnxModel, NeuralNetwork.

- `custom_training_options`:

  Options for custom trainer. Object of type MLCustomTrainingOptions.

- `pre_processors`:

  Data pre-processors. List of MLPreProcessor.

- `apply_pre_processors_before_training`:

  When to apply pre-processors: before (true) or after (false) training.

- `tuner`:

  Tuner name.

- `maximum_models_to_train`:

  Maximum models to train.

- `survival_data`:

  Names of the columns containing survival data.

- `trainer_hyperparameters`:

  Trainer hyperparameters.

------------------------------------------------------------------------

### Method `toList()`

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

#### Usage

    MLTrainingOptions$toList()

#### Returns

A list representation of the MLTrainingOptions object compatible with
the API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MLTrainingOptions$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
