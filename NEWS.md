# Myrconn.PetroVisor.Client (development version)

* add `send_mail()` method to `ServiceProvider` class for sending emails using PetroVisor email configuration
* deprecate `DataSetRequest` class - never integrated into DataServices, use DataServices methods directly instead
* add comprehensive documentation with @seealso cross-references and vignette integration across all major classes

# Myrconn.PetroVisor.Client 3.6.0

* implement singleton authentication pattern via `AuthContext` class for centralized auth management
  * add authentication utility functions: `get_auth_context()`, `require_authentication()`, `with_auth_context()`
  * enhance `AuthenticationService` with comprehensive token management and API key authentication
  * update all services to use singleton authentication context
* add Machine Learning support with model training capabilities
  * add `MLModel` class with support for multiple model types (Regression, BinaryClassification, MultipleClassification, Clustering, NaiveBayes, SurvivalAnalysis)
  * add `MlTrainingService` class providing model training, status checking, and prediction functionality
  * add extensive ML data structures including training options, features, outcomes, results, and transformer configurations
  * add comprehensive unit tests for machine learning and authentication functionality
* improve API documentation with detailed examples and usage patterns
* fix ServiceProvider$convert_unit to work with units containing forward slashes ("/")
* add Myrconn.PetroVisor.Client-package.R to enable help topic for the package itself

# Myrconn.PetroVisor.Client 3.5.0

* update data handling in DataServices.R
* remove obsolete methods (load, save, delete)
* update documentation
* update unit tests
* remove UserSetting from the package
* teams notification workflow trigger

# Myrconn.PetroVisor.Client 3.4.2

* add github action to automate R CMD check
* update test_file_service.R to use temporary files
* fix prefix handling in files$load_names()

# Myrconn.PetroVisor.Client 3.4.1

* fix issue with reshaping data pulled from PetroVisor

# Myrconn.PetroVisor.Client 3.4.0

* add a generic unit converter to the service provider
* update README and DISCRIPTION to adhere to best practices

# Myrconn.PetroVisor.Client 3.3.0

* add support for PivotTables (objects and data)
  * update class PivotTable
  * update RepositoryService to handle pivot table objects
  * add functions to handle pivot table data
  * update ApiRequests to handle query parameters in GET calls correctly
  * update unit tests for PivotTables
  * update documentation

# Myrconn.PetroVisor.Client 3.2.0

* update hierarchy handling
  * change type of field relationship from named list to data frame (static) or
  list of data frames (time-dependent)
  * add support for time dependent hierarchies (retrieve / save relationships)
  * add unit tests
* fix typo in documentation of FileService.R

# Myrconn.PetroVisor.Client 3.1.1

* remove objects no longer supported by PetroVisor
  * VoronoiGrid
  * CrossPlot
  * GeoDataGrid
  * Plot
  * Polygon

# Myrconn.PetroVisor.Client 3.1.0

* add reference table support (table definitions and data)
  * add class ReferenceTable
  * update RepositoryService to handle reference table objects
  * update DataServices to require an instance of the ServiceProvider for instantiation
  * rename existing load, save and delete functions to be more specific
  * add functions to handle reference table data
  * update ApiRequests to handle query parameters in PUT and DELETE calls correctly
  * add unit tests for ReferenceTables
  * update documentation

# Myrconn.PetroVisor.Client 3.0.0

* fix broken unit tests
* fix issues identified by package check
* fix license
* update error handling in ApiRequests.R
