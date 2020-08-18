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
- Remove irrelevant message when comparing 2 empty RelDataModel

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
