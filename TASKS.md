# MMVshiny Tasks

## Plan of Action – Open Tasks Prioritization

1. [Issue #8](https://github.com/MMVverse/MMVshiny/issues/8) — `GetType()` re-derived dozens of times per compound instead of cached
2. [Issue #2](https://github.com/MMVverse/MMVshiny/issues/2) — Add `"checkbox input"` TYPE

---

## `GetType()` re-derives the same parameter type dozens of times per compound instead of caching it

[Issue #8](https://github.com/MMVverse/MMVshiny/issues/8) | assignee: venelin | 2026-09-09

Found while profiling slow post-upload responsiveness in
[MedicinesForMalariaVenture/MMVFree#69](https://github.com/MedicinesForMalariaVenture/MMVFree/issues/69) and
[MedicinesForMalariaVenture/MMVSola#194](https://github.com/MedicinesForMalariaVenture/MMVSola/issues/194).
`GetType(id, spec)` (`R/ProcessState.R:701`) is deterministic for a fixed `id`/`spec` but gets
re-derived from scratch by many independent call sites instead of being computed once and reused —
`InitState()`'s per-parameter loop calls it directly plus 5x indirectly via `NAVal()`, and
`ValidateValue()` calls it up to 4x across its `if/else if` chain. Measured live: 13,879 calls
(~13.1s) for MMVSola's 166-parameter spec, 2,606 calls (~2.4s) for MMVFree's 28-parameter spec,
both against the same 4-compound test upload — no single call is slow (~0.9ms avg), it's pure
call-count. Not caused by reactive/observer re-firing (confirmed `InitState`/`SetSCRawData` etc.
each fire exactly once per compound, as expected).

Suggested fix: compute `type <- GetType(id, spec)` once per parameter in `InitState()` and thread
it down to `NAVal()`/`Validate*()` instead of re-deriving; also consider having `GetType()` itself
use the key `InitState()` already sets (`setkey(spec, ID)`) via `spec[.(id), TYPE]` instead of
`spec[ID == id, TYPE]`, which would help every caller for free. Not yet fixed.

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
