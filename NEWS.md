# MMVshiny 1.2.0

* Added `checkbox input` TYPE: renders as `checkboxInput()`, wired as a reactive, and
  supported throughout `InitState()`, `Reset()`, `Get()`, `ValidateValue()`,
  `CreateUIInput()`, `GenerateJavaScriptEventHandlers()`, and
  `GenerateScriptCreatingObservers()`.
* Added `UpdateCheckboxInput()`: mirrors the existing `UpdateSelectInput()` pattern and
  handles `MockShinySession` correctly in tests.
* Added `DemoApp()`: runs the bundled demo app from `inst/extdata/MMVshinyDemo.zip`.
* Declared missing `Imports` entries: `R.utils`, `shinyjs`, `tibble`, `magrittr`.
* Fixed Roxygen2 `@param` documentation for `InitState()`, `GetValidationNote()`,
  `ValidateValue()`, `GetGuiLabel()`, `GetReportLabel()`, `GenerateScriptRenderingIcons()`,
  `GenerateScriptCreatingReactives()`, and `UpdateSelectInput()`.
* Suppress status icons when their collapse panel is hidden via `outputOptions(suspendWhenHidden = FALSE)`.
