# AgeBandDecomposition 2.0.1
   
   * Replaced Excel import with standard RWL format support via `import_rwl()`
   * Deprecated `TRW_readExcel()` in favor of `import_rwl()`
   * Added `firs_age_class` parameter for flexible age_class filtering
   * Added `zero_as_na` parameter to handle zero values
   * Response to CRAN reviewer comments