# MMVshiny Tasks

## Plan of Action – Open Tasks Prioritization

1. [Issue #2](https://github.com/MMVverse/MMVshiny/issues/2) — Add `"checkbox input"` TYPE

---

## Add `"checkbox input"` TYPE for reactive checkboxInput() support

[Issue #2](https://github.com/MMVverse/MMVshiny/issues/2) | assignee: venelin | 2026-04-20

Add a new `TYPE = "checkbox input"` to MMVshiny so that checkboxes can be declared in
the parameter spec and managed as first-class reactive inputs.

Needed by the LAI tab dose prediction tables in MMVSola (interactive legend checkboxes
to show/hide simulated dose curves). Relates to
[MedicinesForMalariaVenture/MMVSola#128](https://github.com/MedicinesForMalariaVenture/MMVSola/issues/128).

### Files to change

- `R/ProcessState.R` — `CreateUIInput()`: dispatch on `TYPE == "checkbox input"` →
  `shiny::checkboxInput()`.
- `R/ProcessState.R` — `GenerateJavaScriptEventHandlers()`: add `change` handler for
  checkbox inputs (same pattern as `"select input"`).
- Generated `CreateReactivesInState.R` logic: wire `"checkbox input"` as a standard
  reactive.
- Tests: new test case for `TYPE = "checkbox input"`.
