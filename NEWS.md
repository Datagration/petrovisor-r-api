**3.3.0 (2025-06-12)**

- add support for PivotTables (objects and data)
  - update class PivotTable
  - update RepositoryService to handle pivot table objects
  - add functions to handle pivot table data
  - update ApiRequests to handle query parameters in GET calls correctly
  - update unit tests for PivotTables
  - update documentation

**3.2.0 (2025-06-04)**

- update hierarchy handling
  - change type of field relationship from named list to data frame (static) or
  list of data frames (time-dependent)
  - add support for time dependent hierarchies (retrieve / save relationships)
  - add unit tests
- fix typo in documentation of FileService.R

**3.1.1 (2025-05-28)**

- remove objects no longer supported by PetroVisor
  - VoronoiGrid
  - CrossPlot
  - GeoDataGrid
  - Plot
  - Polygon

**3.1.0 (2025-05-26)**

- add reference table support (table definitions and data)
  - add class ReferenceTable
  - update RepositoryService to handle reference table objects
  - update DataServices to require an instance of the ServiceProvider for instantiation
  - rename existing load, save and delete functions to be more specific
  - add functions to handle reference table data
  - update ApiRequests to handle query parameters in PUT and DELETE calls correctly
  - add unit tests for ReferenceTables
  - update documentation

**3.0.0 (2025-05-09)**

- fix broken unit tests
- fix issues identified by package check
- fix license
- update error handling in ApiRequests.R
