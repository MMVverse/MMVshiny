library(testthat)
library(shiny)
library(data.table)

# Spec with the dependency shapes of the real use case:
# - B: default depends on A (leaf -> dependent chain), C: max depends on B
# - X <-> Y: mutually guarded defaults, like BPratio/BP_R
make_chain_spec <- function() {
  mk <- function(id, val = "1", max = "100", dec = "2") {
    data.table(
      ID = id, TYPE = "numeric input", GUILABEL = "Label", STATUSICON = TRUE,
      VALEXPR = val, DEFAULTVALUENOTE = "NA", MIN = "0", MINNOTE = "NA",
      MAX = max, MAXNOTE = "NA", DECDIGITS = dec, OUTOFBOUNDS = "nearest",
      SCFILTER = NA_character_, SCVALUE = NA_character_,
      ExprVAL = val, ExprDEFNOTE = "NA_character_", ExprMIN = "0",
      ExprMINNOTE = "NA_character_", ExprMAX = max, ExprMAXNOTE = "NA_character_")
  }
  rbind(mk("A", "2.345"), mk("B"), mk("C", max = "50"), mk("X"), mk("Y"),
        mk("Z", "7"))
}

make_state_scripts <- function(spec) {
  extra <- tempfile(fileext = ".R")
  writeLines(c(
    'MYSTATE$default[["B"]] <- shiny::reactive(Get(MYSTATE, "A") * 3.14159)',
    'MYSTATE$max[["C"]] <- shiny::reactive(Get(MYSTATE, "B") / 2)',
    'MYSTATE$default[["X"]] <- shiny::reactive(if(GetSource(MYSTATE, "Y") == "Default value") 1.5 else Get(MYSTATE, "Y"))',
    'MYSTATE$default[["Y"]] <- shiny::reactive(if(GetSource(MYSTATE, "X") == "Default value") 1.5 else Get(MYSTATE, "X"))'
  ), extra)
  reset <- GenerateScriptCreatingObservers(spec, stateObjectName = "MYSTATE", observerTypes = "resetCount",
                                           seedable = TRUE, filename = tempfile(fileext = ".R"))
  def <- GenerateScriptCreatingObservers(spec, stateObjectName = "MYSTATE", observerTypes = "default",
                                         ids = c("B", "X", "Y"), seedable = TRUE, filename = tempfile(fileext = ".R"))
  max <- GenerateScriptCreatingObservers(spec, stateObjectName = "MYSTATE", observerTypes = "max",
                                         ids = "C", seedable = TRUE, filename = tempfile(fileext = ".R"))
  c(extra, reset, def, max)
}

test_that("seeded initial state equals the live one for all ids, including chains", {
  spec <- make_chain_spec()
  scripts <- make_state_scripts(spec)
  ClearStateSnapshotCache()
  testServer(function(input, output, session) {
    live <- InitState(spec, stateId = "live", scriptsToSource = scripts, seed = FALSE)
    seeded <- InitState(spec, stateId = "seeded", scriptsToSource = scripts, seed = TRUE)
  }, {
    session$flushReact()
    expect_false(live$seeded)
    expect_true(seeded$seeded)
    sLive <- ExtractStateSnapshot(live)
    sSeeded <- ExtractStateSnapshot(seeded)
    expect_identical(sSeeded, sLive)
    # chains actually resolved to non-initial values
    expect_equal(as.numeric(sLive$displayed$B), 7.37)
    expect_equal(sLive$max$C, as.numeric(sLive$validated$B) / 2)
    expect_equal(sLive$status$X$source, "Default value")
    expect_equal(sLive$status$Y$source, "Default value")
    expect_equal(as.numeric(sLive$validated$X), 1.5)
    expect_equal(sLive$statusIconClass$A, "glyphicon glyphicon-info-sign")
  })
})

test_that("seeded observers skip the first run but still react to later changes", {
  spec <- make_chain_spec()
  scripts <- make_state_scripts(spec)
  ClearStateSnapshotCache()
  n <- 0L
  origValidate <- Validate
  testthat::local_mocked_bindings(Validate = function(...) { n <<- n + 1L; origValidate(...) }, .package = "MMVshiny")
  testServer(function(input, output, session) {
    live <- InitState(spec, stateId = "live", scriptsToSource = scripts, seed = FALSE)
  }, {
    session$flushReact()
    nLive <- n
    expect_gt(nLive, 0L)
  })
  nBefore <- n
  testServer(function(input, output, session) {
    seeded <- InitState(spec, stateId = "seeded", scriptsToSource = scripts, seed = TRUE)
  }, {
    nAfterSeed <- n   # includes the one-off snapshot build
    session$flushReact()
    expect_equal(n, nAfterSeed)  # no Validate() calls from the seeded observers
    # Change A as a user would; default of B, then max of C, must propagate
    ProcessGuiInputEvent(seeded, input = NULL, id = "A", inputValue = 10)
    session$flushReact()
    expect_equal(as.numeric(GetDisplayed(seeded, "B")), round(10 * 3.14159, 2))
    expect_equal(GetMax(seeded, "C"), as.numeric(Get(seeded, "B")) / 2)
    expect_equal(GetStatus(seeded, "A")$source, "User input")
  })
})

test_that("a user override of X moves Y off its guarded default in seeded and live states alike", {
  spec <- make_chain_spec()
  scripts <- make_state_scripts(spec)
  ClearStateSnapshotCache()
  testServer(function(input, output, session) {
    live <- InitState(spec, stateId = "live", scriptsToSource = scripts, seed = FALSE)
    seeded <- InitState(spec, stateId = "seeded", scriptsToSource = scripts, seed = TRUE)
  }, {
    session$flushReact()
    for(s in list(live, seeded)) {
      ProcessGuiInputEvent(s, input = NULL, id = "X", inputValue = 3)
    }
    session$flushReact()
    expect_identical(ExtractStateSnapshot(seeded), ExtractStateSnapshot(live))
    expect_equal(as.numeric(Get(seeded, "Y")), 3)
  })
})

test_that("snapshot is cached by key and invalidated when the spec or scripts change", {
  spec <- make_chain_spec()
  scripts <- make_state_scripts(spec)
  ClearStateSnapshotCache()
  s1 <- BuildStateSnapshot(spec, scriptsToSource = scripts)
  expect_s3_class(s1, "MMVshinyStateSnapshot")
  expect_identical(BuildStateSnapshot(spec, scriptsToSource = scripts), s1)
  expect_length(ls(MMVshiny:::snapshotCache), 1L)

  spec2 <- data.table::copy(spec)
  spec2[ID == "A", `:=`(VALEXPR = "9", ExprVAL = "9")]
  s2 <- BuildStateSnapshot(spec2, scriptsToSource = scripts)
  expect_length(ls(MMVshiny:::snapshotCache), 2L)
  expect_equal(as.numeric(s2$validated$A), 9)
  expect_equal(as.numeric(s1$validated$A), 2.345)

  writeLines("# changed", scripts[1])
  expect_false(identical(MMVshiny:::StateSnapshotKey(spec, scriptsToSource = scripts),
                         MMVshiny:::StateSnapshotKey(spec2, scriptsToSource = scripts)))
})

test_that("a failing build warns and InitState(seed = TRUE) falls back to the live path", {
  spec <- make_chain_spec()
  bad <- tempfile(fileext = ".R")
  writeLines("stop('boom')", bad)
  ClearStateSnapshotCache()
  expect_warning(snap <- BuildStateSnapshot(spec, scriptsToSource = bad), "falling back")
  expect_null(snap)
  expect_error(suppressWarnings(InitState(spec, stateId = "bad", scriptsToSource = bad, seed = TRUE)), "boom")

  ok <- InitState(spec, stateId = "ok", scriptsToSource = make_state_scripts(spec), seed = TRUE)
  expect_true(ok$seeded)
  expect_false(InitState(spec, stateId = "ok2", seed = FALSE)$seeded)
})

test_that("seedable = FALSE leaves generated observers unchanged, seedable = TRUE adds ignoreInit", {
  spec <- make_chain_spec()
  for(safe in c(FALSE, TRUE)) {
    skip_if(safe && !requireNamespace("SafeShiny", quietly = TRUE))
    f0 <- GenerateScriptCreatingObservers(spec, ids = "A", observerTypes = c("resetCount", "default", "min", "max"),
                                          useSafeShiny = safe, filename = tempfile(fileext = ".R"))
    f1 <- GenerateScriptCreatingObservers(spec, ids = "A", observerTypes = c("resetCount", "default", "min", "max"),
                                          useSafeShiny = safe, seedable = TRUE, filename = tempfile(fileext = ".R"))
    code0 <- paste(readLines(f0), collapse = "\n")
    code1 <- paste(readLines(f1), collapse = "\n")
    expect_false(grepl("seeded", code0, fixed = TRUE))
    expect_equal(lengths(regmatches(code1, gregexpr("ignoreInit = isTRUE(state$seeded)", code1, fixed = TRUE))), 4L)
    expect_no_error(parse(text = code1))
  }
})
