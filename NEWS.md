<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.8.0

- New function `guess_constraints()` guesses constraints of a data model
according to provided data.

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.7.7

- Documentation correction
(Fixes [#6](https://github.com/patzaw/ReDaMoR/issues/6))

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.7.6

- Code speed up: faster evaluation of nullable and distinct constraints

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.7.4

- fixes [#4](https://github.com/patzaw/ReDaMoR/issues/4):
change default values of `RelTableModel()`

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.7.3

- Default values for nullable, unique and comment properties of fields

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.7.2

- Bug fix in `as_type()` when converting empty base64.

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.7.1

- add `copy_fields()` function and corresponding functionality
in `model_relational_data()`.

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.7.0

- fixes [#2](https://github.com/patzaw/ReDaMoR/issues/2):
change signature of `RelTableModel()`

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.6.6

- Fix `df_to_model()` documentation

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.6.5

- Fix warnings when calling `icon()`
- Fix edition of foreign keys
- Fix display of index menu

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.6.4

- Correction of vignette display
- Fix display bug when editing primary key

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.6.3

- smoother interface for updating fields
- edit multiple fields at once

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.6.1

### Improvements

- faster `fromDBM()` function
- NA values not taken into account when checking uniqueness

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.6.0

### New features

- Expanding matrix support to the Matrix class from the Matrix package

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.5.2

### New features

- Supporting "base64" data type (BLOB).

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.5.1

- Simplification of matrix implementation.

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.5.0

### New features

- Modeling matrix as a table with 3 fields: 2 of types 'row' and 'column' and
the 3rd of your choice.

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.4.4

### Improved documentation

- TKCat information
- How to create foreign keys with the Shiny user interface

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.4.3

### New features

- `length.RelTableModel()` returns the number of fields in the table.
Then the `lengths()` function applied on a RelDataModel object returns
the number of fields in each table.
- `confront_data()`: `success=TRUE` when an empty model is confronted
to empty data.

### Bug fixes

- '[.RelDataModel': now correctly handles tables without foreign keys.
- Allowing the dispatching of `lengths` methods.
- More reliable foreign key checks
- Remove irrelevant message when comparing 2 empty RelDataModel
- Solved display issue with foreign key interfaces in firefox
(selectize did not work as expected in modalDialog)

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.4.2

- Available on CRAN
- Slights modifications to fit CRAN requirements

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.4.0

### New features

- Some *common* keyboard shortcuts are implemented
   - **Ctrl+Z** / **Ctrl+Shift+Z ** for undo and redo
   - **Enter** for validating a new name
   - **del** for deleting tables or foreign keys
   - **F2** for renaming tables or editing foreign key cardinalities
- The order of the fields can be updated
- Tables can be duplicated
- Cardinalities of existing relationships can be updated
- Explicit messages are displayed regarding "*unexpected*" behaviours
- Get foreign keys in a model

### Implementation changes

- Models from `model_relational_data()` are autosaved in a dedicated
environment instead of .GlobalEnv.
- CSS and JS are maintained in dedicated files
- Help tour is not launched by default but the button is blinking for
10 seconds when the application starts
- Correction of several bugs in the interface and underlying functions
