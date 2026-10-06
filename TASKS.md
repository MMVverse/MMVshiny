# MMVshiny Tasks

## Plan of Action – Open Tasks Prioritization

1. [Issue #2](https://github.com/MMVverse/MMVshiny/issues/2) — Add `"checkbox input"` TYPE

---

## Remaining `spec[ID == id, ...]` unindexed boolean-filter scans [RESOLVED]

[Issue #12](https://github.com/MMVverse/MMVshiny/issues/12) | assignee: venelin | 2026-10-06 | resolved 2026-10-06 in v1.2.3

Follow-up to #8, which indexed `GetType()`/`GetSource()` and batched `GetGuiLabel()`/
`GetReportLabel()`/`CalculateSCInputs()`, but left several sibling accessors doing the same
unindexed `spec[ID == id, COL]` scan: `GetUnit()`, `GetOutOfBounds()`, `GetDecDigits()`,
`GetRadioChoices()`, `CreateUIInput()`'s `GUILABEL`/`STATUSICON` lookups, the `GetMin`/`GetMax`/
`GetMinNote`/`GetMaxNote` no-`state` fallback branch, and the handful of `spec[ID == id, ExprXXX]`
reads inside `InitState()`'s per-parameter loop (the same loop #8 already touched, just adjacent
lines it didn't cover).

Fixed in v1.2.3: all of the above now use the indexed `spec[.(id), COL, on = "ID"]` lookup
(`InitState()`'s six `ExprVAL`/`ExprDEFNOTE`/`ExprMIN`/`ExprMINNOTE`/`ExprMAX`/`ExprMAXNOTE` reads
batched into one join per id). Measured against a real upload through the running MMVSola app
(chromote-driven, real Shiny reactive session, MMVSola's 166-parameter spec, same methodology as
#8): 31.46s → 27.87s busy-to-idle (mean of 3 reps each), an ~11% reduction — smaller than #8's own
1.37x because the larger remaining cost (the still-redundant multi-call `GetType()` chains in
`ValidateValue()`, `SetDisplayed()`, `ValidateBoundaries()`, `CreateUIInput()`) was intentionally
left out of scope here; flagged as a follow-up if warranted.

## Compound-state initialization retriggers the DefaultFus reactive many times instead of once [MOVED]

[Issue #10](https://github.com/MMVverse/MMVshiny/issues/10) (closed, misfiled) → moved to
[MedicinesForMalariaVenture/MMVSola#195](https://github.com/MedicinesForMalariaVenture/MMVSola/issues/195)

Found while investigating [MedicinesForMalariaVenture/MMVFree#69](https://github.com/MedicinesForMalariaVenture/MMVFree/issues/69)
as a follow-up to [Issue #8](https://github.com/MMVverse/MMVshiny/issues/8): `predict_DefaultFus()`
(MMVSola's own function) gets called up to 16x per compound state instead of once. On closer
inspection, the cascade is caused by how MMVSola's own spec wires ~20 parameters' `VALEXPR` to a
shared reactive, all listed in the same `observerTypes="default"` `ids` list — `GenerateScript-
CreatingObservers()`'s generic per-parameter pattern behaves correctly here; it's an app-level
spec design consequence, not a package defect. Re-filed with the corrected root cause at
MMVSola#195 above.

## `GetType()` re-derives the same parameter type dozens of times per compound instead of caching it [RESOLVED]

[Issue #8](https://github.com/MMVverse/MMVshiny/issues/8) | assignee: venelin | 2026-09-09 | resolved 2026-09-10 in v1.2.2

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

Fixed in v1.2.2: `GetType()` now does an indexed lookup (`spec[.(id), TYPE, on = "ID"]`)
instead of a full scan; `InitState()` computes `type` once per parameter and threads it into
`NAVal()` (now takes an optional `type =` to skip its internal `GetType()` call); `GetGuiLabel()`/
`GetReportLabel()` batch-join all `ids` in one call instead of scanning once per id (also fixes a
second, previously unflagged O(n²) hotspot in `SetSCRawData()`'s per-row `GetGuiLabel()` call, which
turned out to cost more than the original `GetType()`/`NAVal()` redundancy); `CalculateSCInputs()`
batch-looks-up `SCFILTER`/`SCVALUE`/`TYPE` once instead of scanning per id in its loop; `GetSource()`
also indexed. Measured against real uploads through the running apps (chromote-driven, real Shiny
reactive session, not a synthetic microbenchmark): MMVFree total upload time 4.74s → 3.05s (1.55x),
MMVSola 16.1s → 11.8s (1.37x). Remaining hotspots not covered by this fix: `CreateUIInput()`'s
6-call `GetType()` if/else-if chain, and `LoadStateSpecification()`'s pre-`setkey()` per-id loop —
left for a follow-up issue if warranted.

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
