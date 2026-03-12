# DataSource

Class representing a PetroVisor data source object.

## Public fields

- `name`:

  The name of the data source.

- `dataConnectionName`:

  The name of the associated data connection.

- `connectionType`:

  The type of the data connection.

- `cultureName`:

  The name of the culture settings.

- `settings`:

  The settings of the data source.

- `dataKind`:

  The type of the dat in the data source.

- `extraCategory`:

  A string stating an extra category.

- `isStackedEntities`:

  This flag specifies whether the source data contains entity
  information in a stacked column.

- `isStackedSignals`:

  This flag specifies whether the source data contains signal
  information in a stacked column.

- `isNewEntitiesTracked`:

  This flag specifies whether new entities are tracked in the log.

- `createNewEntitiesTypeName`:

  If new entities are added to the database automatically, this field
  gives the name of the entity type with which the new entities are
  created.

- `createNewEntitiesRank`:

  If new entities are added to the database automatically, this field
  gices the ranke of the entity type with which the new entities are
  created.

- `isNewMappingsCreated`:

  This flag specifies whether mappings for new entities are added to the
  source automatically.

- `addNewEntitiesToHierarchies`:

  ??

- `importTagEntriesQuery`:

  The query that is executed to add tag entries.

- `entityAliasQuery`:

  The query that is executed to retrieve aliases for entities.

- `stepColumn`:

  The index of the column that specifies the timestamp.

- `stackedEntitiesColumn`:

  The index of the column that specifies the entities.

- `stackedSignalsColumn`:

  The index of the column that specifies the signals.

- `dataMappings`:

  A list stating the data mappings. Each item of the list must be an
  instance of the class `DataMapping`.

- `isLocked`:

  This flag specifies whether the data source is locked. Defaults to
  `FALSE`.

- `user`:

  The user the data source belongs to.

- `isFavorite`:

  This flag specifies whether the data source is marked as favorite
  item, and thus shown in the favorites tab on the home module in
  PetroVisor. Defaults to `FALSE`.

- `labels`:

  A list of strings holding the labels of the data source.

## Methods

### Public methods

- [`DataSource$new()`](#method-DataSource-new)

- [`DataSource$toList()`](#method-DataSource-toList)

- [`DataSource$clone()`](#method-DataSource-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new DataSource instance.

#### Usage

    DataSource$new(
      name = NULL,
      dataConnectionName = NULL,
      connectionType = NULL,
      cultureName = NULL,
      settings = NULL,
      dataKind = NULL,
      extraCategory = NULL,
      isStackedEntities = NULL,
      isStackedSignals = NULL,
      isNewEntitiesTracked = NULL,
      createNewEntitiesTypeName = NULL,
      createNewEntitiesRank = NULL,
      isNewMappingsCreated = NULL,
      addNewEntitiesToHierarchies = NULL,
      importTagEntriesQuery = NULL,
      entityAliasQuery = NULL,
      stepColumn = NULL,
      stackedEntitiesColumn = NULL,
      stackedSignalsColumn = NULL,
      dataMappings = NULL,
      isLocked = FALSE,
      user = NULL,
      isFavorite = FALSE,
      labels = NULL
    )

#### Arguments

- `name`:

  The name of the data source.

- `dataConnectionName`:

  The name of the associated data connection.

- `connectionType`:

  The type of the data connection.

- `cultureName`:

  The name of the culture settings.

- `settings`:

  The settings of the data source.

- `dataKind`:

  The type of the dat in the data source.

- `extraCategory`:

  A string stating an extra category.

- `isStackedEntities`:

  This flag specifies whether the source data contains entity
  information in a stacked column.

- `isStackedSignals`:

  This flag specifies whether the source data contains signal
  information in a stacked column.

- `isNewEntitiesTracked`:

  This flag specifies whether new entities are tracked in the log.

- `createNewEntitiesTypeName`:

  If new entities are added to the database automatically, this field
  gives the name of the entity type with which the new entities are
  created.

- `createNewEntitiesRank`:

  If new entities are added to the database automatically, this field
  gices the ranke of the entity type with which the new entities are
  created.

- `isNewMappingsCreated`:

  This flag specifies whether mappings for new entities are added to the
  source automatically.

- `addNewEntitiesToHierarchies`:

  ??

- `importTagEntriesQuery`:

  The query that is executed to add tag entries.

- `entityAliasQuery`:

  The query that is executed to retrieve aliases for entities.

- `stepColumn`:

  The index of the column that specifies the timestamp.

- `stackedEntitiesColumn`:

  The index of the column that specifies the entities.

- `stackedSignalsColumn`:

  The index of the column that specifies the signals.

- `dataMappings`:

  A list stating the data mappings. Each item of the list must be an
  instance of the class `DataMapping`.

- `isLocked`:

  This flag specifies whether the data source is locked. Defaults to
  `FALSE`.

- `user`:

  The user the data source belongs to.

- `isFavorite`:

  This flag specifies whether the data source is marked as favorite
  item, and thus shown in the favorites tab on the home module in
  PetroVisor. Defaults to `FALSE`.

- `labels`:

  A list of strings holding the labels of the data source.

------------------------------------------------------------------------

### Method `toList()`

#### Usage

    DataSource$toList()

#### Details

Convert the object to a list. This function is mainly used by the
RepositoryService to convert the objects to lists and then call the web
API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DataSource$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
DataSource$new()
} # }
```
