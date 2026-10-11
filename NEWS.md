# MMVshiny 1.6.0

* Seeded initial state (#21): `BuildStateSnapshot()` settles one live state per R process and caches a snapshot of the initialised values (validated, displayed, status, icon, event, resetCount, plus default/min/max for comparison), keyed by the spec, the generated scripts and the package versions. `InitState(seed = TRUE)` writes that snapshot into new states before their observers are created, and `GenerateScriptCreatingObservers(seedable = TRUE)` creates the resetCount/default/min/max observers with `ignoreInit` for seeded states. Falls back to the live path if no snapshot can be built. New helpers `ExtractStateSnapshot()` and `ClearStateSnapshotCache()`. Snapshot build and seeding are labelled SafeShiny reactives (`seedState`) when SafeShiny is installed.

# MMVshiny 1.5.0

* New `GetStateStatus(state, ids = NULL)`: rolls up the per-parameter `state$status` into a state-level worst-of severity (`OK` < `WARN` < `ERROR`), a source mix (default / data / user / mixed, `hasUserOverride`) a per-id `details` table, and the list of user-overwritten parameters (`overriddenIds`, `overrides` with current and default value) (#9).

# MMVshiny 1.4.1

* With `useSafeShiny = TRUE`, the generated observers, reactives and status icons now pass an explicit `label` (e.g. `reset <ID>`, `<ID>`, `<ID> icon`), so SafeShiny's timing output and flame chart are readable (#18).

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
