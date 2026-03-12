# PivotTable

Class representing a PetroVisor pivot table object.

## Public fields

- `name`:

  The name of the pivot table.

- `add_entity_alias_column`:

  This flag specifies whether a column containing the entity's alias
  shall be added to the pivot table.

- `entity_parent_columns`:

  Defines the number of parent columns added to the pivot table (when a
  hierarchy is specified).

- `add_entity_type_column`:

  This flag specifies whether a column containing the entity's type
  shall be added to the pivot table.

- `scope_formula`:

  The scope definition as string (P# syntax).

- `entity_set_formula`:

  The entity set definition as string (P# syntax).

- `table_formula`:

  The table definition as string (P# syntax).

- `hierarchy_name`:

  The name of the hierarchy used to retrieve the parent name.

- `tag_entry_date`:

  The date used to evaluate tag entries.

- `saved_date`:

  The date the pivot table was saved to the database.

- `used_tags`:

  A list of tag names used in the pivot table.

- `skip_empty_rows`:

  This flag specifies whether empty rows shall be skipped during table
  generation.

- `add_is_opportunity_column`:

  This flag specifies whether the column IsOpportunity shall be added to
  the pivot table.

- `append_data`:

  This flag specifies whether new data shall be appended to the saved
  table.

- `description`:

  The description of the item.

- `labels`:

  A list of strings holding the labels of the pivot table.

## Methods

### Public methods

- [`PivotTable$new()`](#method-PivotTable-new)

- [`PivotTable$toList()`](#method-PivotTable-toList)

- [`PivotTable$clone()`](#method-PivotTable-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new PivotTable instance.

#### Usage

    PivotTable$new(
      name = NULL,
      add_entity_alias_column = NULL,
      entity_parent_columns = 0,
      add_entity_type_column = NULL,
      scope_formula = NULL,
      entity_set_formula = NULL,
      table_formula = NULL,
      hierarchy_name = "",
      tag_entry_date = NULL,
      saved_date = NULL,
      used_tags = list(),
      skip_empty_rows = NULL,
      add_is_opportunity_column = NULL,
      append_data = NULL,
      description = NULL,
      labels = list()
    )

#### Arguments

- `name`:

  The name of the pivot table.

- `add_entity_alias_column`:

  This flag specifies whether a column containing the entity's alias
  shall be added to the pivot table.

- `entity_parent_columns`:

  Defines the number of parent columns added to the pivot table (when a
  hierarchy is specified).

- `add_entity_type_column`:

  This flag specifies whether a column containing the entity's type
  shall be added to the pivot table.

- `scope_formula`:

  The scope definition as string (P# syntax).

- `entity_set_formula`:

  The entity set definition as string (P# syntax).

- `table_formula`:

  The table definition as string (P# syntax).

- `hierarchy_name`:

  The name of the hierarchy used to retrieve the parent name.

- `tag_entry_date`:

  The date used to evaluate tag entries.

- `saved_date`:

  The date the pivot table was saved to the database.

- `used_tags`:

  A list of tag names used in the pivot table.

- `skip_empty_rows`:

  This flag specifies whether empty rows shall be skipped during table
  generation.

- `add_is_opportunity_column`:

  This flag specifies whether the column IsOpportunity shall be added to
  the pivot table.

- `append_data`:

  This flag specifies whether new data shall be appended to the saved
  table.

- `description`:

  The description of the item.

- `labels`:

  A list of strings holding the labels of the pivot table.

------------------------------------------------------------------------

### Method `toList()`

#### Usage

    PivotTable$toList()

#### Details

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PivotTable$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
PivotTable$new()
} # }
```
