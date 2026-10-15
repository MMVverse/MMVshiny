# MMVshiny 1.7.0

* Persistent snapshot cache (#29): `BuildStateSnapshot()` now also stores each snapshot as an RDS file named after the cache key, by default in a self-ignoring folder `.MMVshiny-cache` in the working directory, i.e. the deployed app's home folder (option `MMVshiny.snapshotCacheDir`, `FALSE` disables), so each deployed copy of an app has its own cache and a new deployment starts with an empty one, and loads it when the in-memory cache has no snapshot. A new R process (app-process restart, server reboot) therefore starts without rebuilding the snapshot; a deployment that changes the spec, scripts, `listObjects` values or package versions changes the key and builds a new one. Files unused for 30 days (option `MMVshiny.snapshotMaxAgeDays`) are pruned on write; corrupt files and unwritable directories are ignored. Writes are atomic. `ClearStateSnapshotCache()` gains `disk = TRUE`. No change needed in apps using `seed = TRUE`.
* Fix (#27): `BuildStateSnapshot()` / `InitState(seed = TRUE)` no longer hash session- or process-specific `listObjects` (e.g. shiny's `input`, whose digest differs in every session, or Java references such as a SMILES parser) into the snapshot cache key. Only the names of `listObjects` and the values of plain-data elements (NULL, atomic vectors, lists and data frames of those) enter the key; the other objects are supplied live to every state by `InitState()` and never stored in a snapshot. A snapshot built at startup with surrogates for such objects is now found by `InitState(seed = TRUE)` in every session. Existing snapshot keys change, so caches are simply rebuilt.

# MMVshiny 1.6.1

* Fix (#24): with `GenerateScriptCreatingObservers(seedable = TRUE)`, the default/min/max observers of a seeded state skipped their first run unconditionally (`ignoreInit`), losing any change made to the state between `InitState(seed = TRUE)` and the first flush (e.g. uploading a Science Cloud file left predicted values N/A and the prediction plot empty). The handlers are now guarded by the new `SkipSeededRun()`, which skips the first run only if the event value equals the snapshot value; resetCount observers always run. The skip additionally requires the id's validated value, source and event to be unchanged since seeding (a report upload sets `validated = NA` with source "Default value", which the first run repairs). Regenerate the observer scripts (they are regenerated at app start).

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
