# MLNeuralNetworkTrainingOptions

Represents ML neural network training options.

## Public fields

- `library`:

  The name of the neural network library.

- `model_path`:

  The path to the model.

- `transfer_learning`:

  Whether to use transfer learning.

- `hyperparameter_tuning`:

  Whether to tune hyperparameters.

- `features_mapping`:

  Features and label names mapping from internal names to trained model
  names.

- `optimizer`:

  The optimizer to use.

- `learning_rate`:

  The learning rate used (value between 0 and 1).

- `epochs`:

  The number of training iterations.

- `batch_size`:

  The number of samples to use for mini-batch training.

- `sequence_window_size`:

  Series sequence window size.

- `activation_function`:

  The activation function.

- `number_of_dense_layers`:

  Number of dense layers.

- `number_of_neurons`:

  Number of neurons.

- `number_of_conv1d_layers`:

  Number of Conv1D layers.

- `number_of_conv1d_filters`:

  Number of Conv1D filters.

- `conv1d_kernel_size`:

  Conv1D kernel size.

- `number_of_lstm_layers`:

  Number of LSTM layers.

- `number_of_lstm_neurons`:

  Number of LSTM neurons.

## Methods

### Public methods

- [`MLNeuralNetworkTrainingOptions$new()`](#method-MLNeuralNetworkTrainingOptions-new)

- [`MLNeuralNetworkTrainingOptions$toList()`](#method-MLNeuralNetworkTrainingOptions-toList)

- [`MLNeuralNetworkTrainingOptions$clone()`](#method-MLNeuralNetworkTrainingOptions-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new MLNeuralNetworkTrainingOptions instance.

#### Usage

    MLNeuralNetworkTrainingOptions$new(
      library = NULL,
      model_path = NULL,
      transfer_learning = NULL,
      hyperparameter_tuning = NULL,
      features_mapping = NULL,
      optimizer = NULL,
      learning_rate = NULL,
      epochs = NULL,
      batch_size = NULL,
      sequence_window_size = NULL,
      activation_function = NULL,
      number_of_dense_layers = NULL,
      number_of_neurons = NULL,
      number_of_conv1d_layers = NULL,
      number_of_conv1d_filters = NULL,
      conv1d_kernel_size = NULL,
      number_of_lstm_layers = NULL,
      number_of_lstm_neurons = NULL
    )

#### Arguments

- `library`:

  The name of the neural network library.

- `model_path`:

  The path to the model.

- `transfer_learning`:

  Whether to use transfer learning.

- `hyperparameter_tuning`:

  Whether to tune hyperparameters.

- `features_mapping`:

  Features and label names mapping from internal names to trained model
  names.

- `optimizer`:

  The optimizer to use.

- `learning_rate`:

  The learning rate used (value between 0 and 1).

- `epochs`:

  The number of training iterations.

- `batch_size`:

  The number of samples to use for mini-batch training.

- `sequence_window_size`:

  Series sequence window size.

- `activation_function`:

  The activation function.

- `number_of_dense_layers`:

  Number of dense layers.

- `number_of_neurons`:

  Number of neurons.

- `number_of_conv1d_layers`:

  Number of Conv1D layers.

- `number_of_conv1d_filters`:

  Number of Conv1D filters.

- `conv1d_kernel_size`:

  Conv1D kernel size.

- `number_of_lstm_layers`:

  Number of LSTM layers.

- `number_of_lstm_neurons`:

  Number of LSTM neurons.

------------------------------------------------------------------------

### Method `toList()`

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

#### Usage

    MLNeuralNetworkTrainingOptions$toList()

#### Returns

A list representation of the MLNeuralNetworkTrainingOptions object
compatible with the API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MLNeuralNetworkTrainingOptions$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
