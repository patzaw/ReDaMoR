# Changelog

## Version 1.0.0

- Decouple primary keys and foreign keys from index creation
- Allow ordering of indexes
- Order of fields in index is conserved (not sorted by alphabetic order
  aymore)
- Improved display of fields and comments in data model plot

## Version 0.8.2

CRAN release: 2025-02-21

- New function
  [`guess_constraints()`](https://patzaw.github.io/ReDaMoR/reference/guess_constraints.md)
  guesses constraints of a data model according to provided data.

## Version 0.7.7

- Documentation correction (Fixes
  [\#6](https://github.com/patzaw/ReDaMoR/issues/6))

## Version 0.7.6

CRAN release: 2024-07-07

- Code speed up: faster evaluation of nullable and distinct constraints

## Version 0.7.4

CRAN release: 2024-02-07

- fixes [\#4](https://github.com/patzaw/ReDaMoR/issues/4): change
  default values of
  [`RelTableModel()`](https://patzaw.github.io/ReDaMoR/reference/RelTableModel.md)

## Version 0.7.3

- Default values for nullable, unique and comment properties of fields

## Version 0.7.2

CRAN release: 2023-07-05

- Bug fix in
  [`as_type()`](https://patzaw.github.io/ReDaMoR/reference/as_type.md)
  when converting empty base64.

## Version 0.7.1

CRAN release: 2022-10-19

- add
  [`copy_fields()`](https://patzaw.github.io/ReDaMoR/reference/copy_fields.md)
  function and corresponding functionality in
  [`model_relational_data()`](https://patzaw.github.io/ReDaMoR/reference/model_relational_data.md).

## Version 0.7.0

- fixes [\#2](https://github.com/patzaw/ReDaMoR/issues/2): change
  signature of
  [`RelTableModel()`](https://patzaw.github.io/ReDaMoR/reference/RelTableModel.md)

## Version 0.6.6

- Fix
  [`df_to_model()`](https://patzaw.github.io/ReDaMoR/reference/df_to_model.md)
  documentation

## Version 0.6.5

CRAN release: 2022-09-05

- Fix warnings when calling `icon()`
- Fix edition of foreign keys
- Fix display of index menu

## Version 0.6.4

- Correction of vignette display
- Fix display bug when editing primary key

## Version 0.6.3

CRAN release: 2022-04-13

- smoother interface for updating fields
- edit multiple fields at once

## Version 0.6.1

### Improvements

- faster
  [`fromDBM()`](https://patzaw.github.io/ReDaMoR/reference/fromDBM.md)
  function
- NA values not taken into account when checking uniqueness

## Version 0.6.0

### New features

- Expanding matrix support to the Matrix class from the Matrix package

## Version 0.5.2

CRAN release: 2022-01-13

### New features

- Supporting “base64” data type (BLOB).

## Version 0.5.1

- Simplification of matrix implementation.

## Version 0.5.0

### New features

- Modeling matrix as a table with 3 fields: 2 of types ‘row’ and
  ‘column’ and the 3rd of your choice.

## Version 0.4.4

### Improved documentation

- TKCat information
- How to create foreign keys with the Shiny user interface

## Version 0.4.3

CRAN release: 2020-12-14

### New features

- `length.RelTableModel()` returns the number of fields in the table.
  Then the
  [`lengths()`](https://patzaw.github.io/ReDaMoR/reference/lengths.md)
  function applied on a RelDataModel object returns the number of fields
  in each table.
- [`confront_data()`](https://patzaw.github.io/ReDaMoR/reference/confront_data.md):
  `success=TRUE` when an empty model is confronted to empty data.

### Bug fixes

- ‘\[.RelDataModel’: now correctly handles tables without foreign keys.
- Allowing the dispatching of `lengths` methods.
- More reliable foreign key checks
- Remove irrelevant message when comparing 2 empty RelDataModel
- Solved display issue with foreign key interfaces in firefox (selectize
  did not work as expected in modalDialog)

## Version 0.4.2

CRAN release: 2020-03-31

- Available on CRAN
- Slights modifications to fit CRAN requirements

## Version 0.4.0

### New features

- Some *common* keyboard shortcuts are implemented
  - **Ctrl+Z** / **Ctrl+Shift+Z** for undo and redo
  - **Enter** for validating a new name
  - **del** for deleting tables or foreign keys
  - **F2** for renaming tables or editing foreign key cardinalities
- The order of the fields can be updated
- Tables can be duplicated
- Cardinalities of existing relationships can be updated
- Explicit messages are displayed regarding “*unexpected*” behaviours
- Get foreign keys in a model

### Implementation changes

- Models from
  [`model_relational_data()`](https://patzaw.github.io/ReDaMoR/reference/model_relational_data.md)
  are autosaved in a dedicated environment instead of .GlobalEnv.
- CSS and JS are maintained in dedicated files
- Help tour is not launched by default but the button is blinking for 10
  seconds when the application starts
- Correction of several bugs in the interface and underlying functions
