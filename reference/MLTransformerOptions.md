# MLTransformerOptions

Options for ML transformers.

## Public fields

- `rank`:

  The dimension of the feature space to map the input to.

- `fix_zero`:

  Whether to map zero to zero, preserving sparsity.

- `maximum_bin_count`:

  Maximum number of bins (power of 2 recommended).

- `minimum_examples_per_bin`:

  Minimum number of examples per bin.

- `ensure_zero_mean`:

  If true, subtract mean from each value before normalizing and use the
  raw input otherwise. Defaults to false for MLNormalizationType.LpNorm,
  true for MLNormalizationType.GlobalContrast.

- `norm`:

  Type of norm to use to normalize each sample. The indicated norm of
  the resulting vector will be normalized to one. One of `L2`,
  `StandardDeviation`, `L1`, `Infinity`.

- `center_data`:

  Whether to center the data around 0 by removing the median.

- `quantile_min`:

  Quantile min used to scale the data.

- `quantile_max`:

  Quantile max used to scale the data.

- `ensure_unit_standard_deviation`:

  If true, the resulting vector's standard deviation would be one.
  Otherwise, the resulting vector's L2-norm would be one.

- `scale`:

  Scale features by this value.

- `use_cdf`:

  Whether to use CDF as the output. Defaults to false for
  MLNormalizationType.MeanVariance, true for
  MLNormalizationType.LogMeanVariance.

## Methods

### Public methods

- [`MLTransformerOptions$new()`](#method-MLTransformerOptions-new)

- [`MLTransformerOptions$toList()`](#method-MLTransformerOptions-toList)

- [`MLTransformerOptions$clone()`](#method-MLTransformerOptions-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new MLTransformerOptions instance.

#### Usage

    MLTransformerOptions$new(
      rank = NULL,
      fix_zero = NULL,
      maximum_bin_count = NULL,
      minimum_examples_per_bin = NULL,
      ensure_zero_mean = NULL,
      norm = c(NULL, "L2", "StandardDeviation", "L1", "Infinity"),
      center_data = NULL,
      quantile_min = NULL,
      quantile_max = NULL,
      ensure_unit_standard_deviation = NULL,
      scale = NULL,
      use_cdf = NULL
    )

#### Arguments

- `rank`:

  The dimension of the feature space to map the input to.

- `fix_zero`:

  Whether to map zero to zero, preserving sparsity.

- `maximum_bin_count`:

  Maximum number of bins (power of 2 recommended).

- `minimum_examples_per_bin`:

  Minimum number of examples per bin.

- `ensure_zero_mean`:

  If true, subtract mean from each value before normalizing and use the
  raw input otherwise. Defaults to false for MLNormalizationType.LpNorm,
  true for MLNormalizationType.GlobalContrast.

- `norm`:

  Type of norm to use to normalize each sample. The indicated norm of
  the resulting vector will be normalized to one. One of `L2`,
  `StandardDeviation`, `L1`, `Infinity`.

- `center_data`:

  Whether to center the data around 0 by removing the median.

- `quantile_min`:

  Quantile min used to scale the data.

- `quantile_max`:

  Quantile max used to scale the data.

- `ensure_unit_standard_deviation`:

  If true, the resulting vector's standard deviation would be one.
  Otherwise, the resulting vector's L2-norm would be one.

- `scale`:

  Scale features by this value.

- `use_cdf`:

  Whether to use CDF as the output. Defaults to false for
  MLNormalizationType.MeanVariance, true for
  MLNormalizationType.LogMeanVariance.

------------------------------------------------------------------------

### Method `toList()`

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

#### Usage

    MLTransformerOptions$toList()

#### Returns

A list representation of the MLTransformerOptions object compatible with
the API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MLTransformerOptions$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
