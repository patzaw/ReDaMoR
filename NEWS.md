<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.4.0

### New features

- Some *common* keyboard shortcuts are implemented
   - **Ctrl+Z** / **Ctrl+Shift+Z ** for undo and redo
   - **Enter** for validating a new name
   - **del** for deleting tables
   - **F2** for renaming tables
- The order of the fields can be updated
- Tables can be duplicated
- Cardinalities of existing relationships can be updated
- Explicit messages are displayed regarding "*unexpected*" behaviours

### Implementation changes

- Models from `model_relational_data()` are autosaved in a dedicated
environment instead of .GlobalEnv.
- CSS and JS are maintained in dedicated files
- Help tour is not launched by default but the button is blinking for
10 seconds when the application starts
- Correction of several bugs in the interface and underlying functions
