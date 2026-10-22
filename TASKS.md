# MMVshiny Tasks

## Plan of Action – Open Tasks Prioritization

1. [Issue #22](https://github.com/MMVverse/MMVshiny/issues/22) — Group presentation reactives/observers and delegate colour/status icon to the client
2. [Issue #4](https://github.com/MMVverse/MMVshiny/issues/4) — Document `compoundActivationTrigger` pattern for per-compound reactives that use `isolate()`
3. [Issue #3](https://github.com/MMVverse/MMVshiny/issues/3) — `GenerateHandleEventsJS`: add `FLAG_UseStaticBind` for dynamically created inputs
4. [Issue #1](https://github.com/MMVverse/MMVshiny/issues/1) — Add state-level status (disable report generation for invalid states)

---

## Persist the state snapshot cache on disk to speed up cold starts [RESOLVED]

[Issue #29](https://github.com/MMVverse/MMVshiny/issues/29) | assignee: venelin | 2026-10-15 | resolved 2026-10-15 in v1.7.0

`BuildStateSnapshot()` cached snapshots only in memory, so every new R process (Shiny Server restart, idle respawn, reboot, redeploy) rebuilt the snapshot at the first session. Snapshots are now persisted as RDS files keyed by the existing cache key, in a cache directory persisted in the deployed app's folder.

## StateSnapshotKey hashes session-specific listObjects, so seed = TRUE never hits the cache [RESOLVED]

[Issue #27](https://github.com/MMVverse/MMVshiny/issues/27) | assignee: venelin | 2026-10-15 | resolved 2026-10-15 in v1.7.0

`StateSnapshotKey()` hashed the values of session-specific `listObjects` elements (`input`, Java refs such as MMVFree's `smilesParser`), so the key differed per session/process. Fixed (v1.6.2): such objects are ignored in the key; the key no longer depends on the row order of the spec (v1.7.0).

## Seeded observers skip first run even when the state changed before the first flush [RESOLVED]

[Issue #24](https://github.com/MMVverse/MMVshiny/issues/24) | 2026-10-11 | resolved 2026-10-13 in v1.6.1

Follow-up to #21: uploading MMV008_Chloroquine.xlsx in MMVSola with seeded states left predicted values N/A and the plot empty, because seeded default/min/max observers used `ignoreInit = TRUE` and skipped a first run that would have seen the post-`InitState()` changes. Fixed: `SkipSeededRun()` skips the first run only if the event value, validated value and source are identical to the snapshot.

## Group presentation reactives/observers and delegate colour/status icon to the client

[Issue #22](https://github.com/MMVverse/MMVshiny/issues/22) | assignee: venelin | 2026-10-11

Per input id MMVshiny creates reset, displayed, default/min/max and count observers plus a status-icon `renderUI`; in the MMVSola startup trace about half of the displayed phase (1.58 s) is wrapper/scheduling overhead. Proposals: (A) group and synchronise presentation on the server (one observer pushing changed ids, one reset loop, lazy SC/report reactive values); (B) send a single `{id: [status, text]}` message and let the client set a CSS class/data attribute for colour, icon and text. Open question: is the client-side variant faster end to end (measure with chromote)? Acceptance: identical GUI state checked by a test; startup/upload/single-edit timings before/after. Related to #21.

Preferred direction (2026-10-21): not grouping, which would lose the fine-grained reactivity of a single changed input, but a synchronous per-id displayed push: `SetDisplayed()` calls `js$backgroundCol()`/`js$fontCol()`/`update*Input()` directly instead of an observer reacting to the `DISPLAYED` event; the status-icon `renderUI` is kept. Trade-offs measured on the bundled demo (`js$backgroundCol` calls, observer vs synchronous): startup 5 vs 6, edit 2 vs 2, Reset all 2 vs 6 — the observer coalesces several writes per flush and ignores identical ones, the synchronous push does neither (a last-pushed cache would fix identical writes only); a source change without a `SetDisplayed()` call does not repaint the colour (not the case in the package's own flows, where `Validate()` is always followed by `SetDisplayed()`). Apps like MMVSola, where several states share the same GUI inputs and one active state controls them at a time, cannot easily use the synchronous mode (`SetDisplayed()` is also called on non-active states during uploads, and repainting on a change of the active state is not clearly feasible), so the observer mode stays the default and is the more generally usable one.

Status: PR [#30](https://github.com/MMVverse/MMVshiny/pull/30) (merged 2026-10-22 in v1.8.0) added the opt-in `synchronous` argument to `SetDisplayed()` (default `isTRUE(state$synchronousDisplayed)`, i.e. off), refs #22 without closing it. Demo for the SafeShiny trace of both modes: `mmvshiny-sync-demo` on `mmvserver-test` (`?safeshinytab=track`, checkbox switches mode). Remaining: decide whether to adopt synchronous mode in apps with a single state, last-pushed cache, client-side variant (B) measurements.

## Cache the initialised state object across sessions (seeded initial state) [RESOLVED]

[Issue #21](https://github.com/MMVverse/MMVshiny/issues/21) | assignee: venelin | 2026-10-11 | resolved 2026-10-11 in v1.6.0 (re-landed in v1.6.1, see #24)

MMVSola startup took ~7-8 s, of which the initial reset and default phases were ~3 s plus ~1.6 s in `InitState`, recomputed identically each session. New states are now seeded from a cached snapshot of the initialised state instead of re-running reset/defaults; only reactives/observers are created per session.

## Label generated SafeShiny observers/reactives/icons for readable timing output [RESOLVED]

[Issue #18](https://github.com/MMVverse/MMVshiny/issues/18) | assignee: venelin | 2026-10-08 | resolved 2026-10-08 in v1.4.1

With `useSafeShiny = TRUE`, generated `SafeObserveEvent`/`SafeReactive`/`SafeRenderUI` calls now pass an explicit `label` (e.g. `reset <ID>`, `<ID>`, `<ID> icon`) so SafeShiny's timing tables and `PlotSafeShinyFlame()` are readable.

## Use SafeShiny::SafeReactive() in generated reactives [RESOLVED]

[Issue #16](https://github.com/MMVverse/MMVshiny/issues/16) | assignee: venelin | 2026-10-08 | resolved 2026-10-08 in v1.4.0

Follow-up to [Issue #9](https://github.com/MMVverse/MMVshiny/issues/9): SafeShiny 0.3.0 adds `SafeReactive()` ([SafeShiny#4](https://github.com/pmxlab/SafeShiny/issues/4)).
`GenerateScriptCreatingReactives(useSafeShiny = FALSE)` now optionally generates `SafeShiny::SafeReactive()` with an `onError` that records an `"ERROR:"` status/icon (then re-raises, so Shiny's native degradation is unchanged). SafeShiny requirement bumped to `>= 0.3.0` in `Suggests`.

## Optional SafeShiny integration: SafeObserveEvent()/SafeRenderUI() in generated observers/icons [RESOLVED]

[Issue #14](https://github.com/MMVverse/MMVshiny/issues/14) | assignee: venelin | 2026-10-07 | resolved 2026-10-07 in v1.3.0

Follow-up to the design discussion in #9: an uncaught error inside a plain `observeEvent()`/
`observe()` terminates the whole Shiny session (confirmed via a deployed crash-test app), unlike
an error inside a `reactive()` consumed only by a render output, which Shiny already converts into
a localized, session-surviving error display on its own — #61's actual crash mechanism. The `Safe*`
design from #9 was built as its own standalone package, [pmxlab/SafeShiny](https://github.com/pmxlab/SafeShiny)
(kept separate since it's genuinely generic, no dependency on `state$status`/`SetStatus()`), rather
than inside MMVshiny itself.

Fixed in v1.3.0: `GenerateScriptCreatingObservers(useSafeShiny = FALSE)` and
`GenerateScriptRenderingIcons(useSafeShiny = FALSE)` — both default to `FALSE`, generated output
byte-for-byte unchanged either way. `useSafeShiny = TRUE` wires generated observers into
`SafeShiny::SafeObserveEvent()` (catches and records an `"ERROR:"` status via the existing
`SetStatus()`/`SetInfoIcon()` machinery — the crash-class bug is gone) and generated icon renders
into `SafeShiny::SafeRenderUI()` (an observability hook only, since Shiny already degrades a failed
`renderUI()` gracefully). `SafeShiny` added as `Suggests`/`Remotes`, not `Imports`.

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

## State object: roll up per-parameter status to a state-level status, and catch errors in derived reactives [RESOLVED]

[Issue #9](https://github.com/MMVverse/MMVshiny/issues/9) | assignee: venelin | 2026-09-09 | resolved 2026-10-09 in v1.5.0

Per-parameter status in `state$status[[id]]` had no aggregate answering "is this compound OK overall?" (e.g. MMVSola #121/#123: an ERROR-level parameter crashes report generation). Added `GetStateStatus()`, a state-level roll-up of per-parameter status; error catching in derived reactives was delivered via the SafeShiny integration ([Issue #14](https://github.com/MMVverse/MMVshiny/issues/14), [Issue #16](https://github.com/MMVverse/MMVshiny/issues/16)). Related to [Issue #1](https://github.com/MMVverse/MMVshiny/issues/1).

## Document the countID echo-avoidance rationale (and its selectize limitation) [RESOLVED]

[Issue #6](https://github.com/MMVverse/MMVshiny/issues/6) | assignee: venelin | 2026-09-09 | resolved 2026-09-09 in v1.2.1

The vignette and roxygen docs now explain why the `count<ID>` mechanism works: a server-side `update*Input()` never dispatches the native browser event the counter is bound to, so only genuine user interaction increments it, including the selectize limitation.

## Document compoundActivationTrigger pattern for per-compound reactives that use isolate()

[Issue #4](https://github.com/MMVverse/MMVshiny/issues/4) | assignee: venelin | 2026-04-21

A per-compound reactive in `STATE_Objects.R` that uses `isolate()` to suppress a dependency cannot re-invalidate when its compound becomes active again after a compound switch (MMVSola scenario: `OralDosePredictions` reads checkbox states via `isolate(Get(MYSTATE, id))` to avoid DT flicker, so after a compound switch its cache holds stale checkbox HTML). Document the `compoundActivationTrigger` pattern as the fix.

## GenerateHandleEventsJS: add FLAG_UseStaticBind argument to support event delegation

[Issue #3](https://github.com/MMVverse/MMVshiny/issues/3) | unassigned | 2026-04-21

`GenerateHandleEventsJS` generates direct `$('#id').bind('change', ...)` bindings set up once at page load, which fail silently for inputs inside DataTables (`DT::renderDT`) or other widgets that recreate their DOM on redraw. Add `FLAG_UseStaticBind = TRUE`; when `FALSE`, generate event-delegation code instead.

## Add `"checkbox input"` TYPE for reactive checkboxInput() support [RESOLVED]

[Issue #2](https://github.com/MMVverse/MMVshiny/issues/2) | assignee: venelin | 2026-04-20 | resolved 2026-05-07 in v1.2.0

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

## Add state-level status

[Issue #1](https://github.com/MMVverse/MMVshiny/issues/1) | assignee: venelin | 2026-03-17

When an input field is invalid (red status icon), the whole state object should become invalid, e.g. to disable selecting it for report generation. Partly addressed by `GetStateStatus()` ([Issue #9](https://github.com/MMVverse/MMVshiny/issues/9)); the remaining work is using it to gate selection/report generation.
