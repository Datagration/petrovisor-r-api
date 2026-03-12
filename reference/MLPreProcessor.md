# MLPreProcessor

Represents a ML data pre-processor.

## Public fields

- `normalization_type`:

  The type of the pre-processor.

- `transformer_options`:

  Options for the pre-processor. Object of type MLTransformerOptions.

- `order`:

  Ordinal number.

- `is_enabled`:

  Whether the pre-processor is enabled.

## Methods

### Public methods

- [`MLPreProcessor$new()`](#method-MLPreProcessor-new)

- [`MLPreProcessor$toList()`](#method-MLPreProcessor-toList)

- [`MLPreProcessor$clone()`](#method-MLPreProcessor-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new MLPreProcessor instance.

#### Usage

    MLPreProcessor$new(
      normalization_type = c("MinMax", "MeanVariance", "LogMeanVariance", "Binning",
        "SupervisedBinning", "RobustScaling", "LpNorm", "GlobalContrast",
        "ProjectToPrincipalComponents", "ApproximatedKernelMap", "BoxCox", "BoxTidwell"),
      transformer_options = NULL,
      order = NULL,
      is_enabled = FALSE
    )

#### Arguments

- `normalization_type`:

  The type of the pre-processor.

- `transformer_options`:

  Options for the pre-processor. Object of type
  MLTransformerOptionsation.

- `order`:

  Ordinal number.

- `is_enabled`:

  Whether the pre-processor is enabled.

------------------------------------------------------------------------

### Method `toList()`

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

#### Usage

    MLPreProcessor$toList()

#### Returns

A list representation of the MLPreProcessor object compatible with the
API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MLPreProcessor$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
