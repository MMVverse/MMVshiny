library(testthat)
library(shiny)
library(data.table)

make_state <- function() {
  spec <- data.table(
    ID = c("HillIN", "invitroPRR"), TYPE = "numeric input", GUILABEL = "Label", STATUSICON = TRUE,
    VALEXPR = c("1.3", "NA"), DEFAULTVALUENOTE = "NA", MIN = "0.1", MINNOTE = "NA",
    MAX = "10", MAXNOTE = "NA", DECDIGITS = "2", OUTOFBOUNDS = "keep with error",
    SCFILTER = NA_character_, SCVALUE = NA_character_,
    ExprVAL = c("1.3", "NA_real_"), ExprDEFNOTE = "NA_character_",
    ExprMIN = "0.1", ExprMINNOTE = "NA_character_",
    ExprMAX = "10", ExprMAXNOTE = "NA_character_")
  InitState(spec = spec, stateId = "testGSS")
}

test_that("fresh state: OK severity, all default", {
  st <- shiny::isolate(GetStateStatus(make_state()))
  expect_equal(st$severity, "OK")
  expect_equal(st$sourceMix, "default")
  expect_false(st$hasUserOverride)
  expect_equal(st$nERROR, 0L)
  expect_equal(nrow(st$details), 2L)
})

test_that("worst-of severity: ERROR beats WARN beats OK, details sorted by severity", {
  state <- make_state()
  shiny::isolate({
    SetStatus(state, "invitroPRR", validationNote = "WARN: low")
    expect_equal(GetStateStatus(state)$severity, "WARN")
    SetStatus(state, "HillIN", validationNote = c("OK: fine", "ERROR: Hill must be positive"))
    st <- GetStateStatus(state)
    expect_equal(st$severity, "ERROR")
    expect_equal(c(st$nOK, st$nWARN, st$nERROR), c(0L, 1L, 1L))
    expect_equal(st$details$id, c("HillIN", "invitroPRR"))
    expect_match(st$details$note[1], "Hill must be positive")
    expect_false(grepl("^ERROR", st$details$note[1]))
  })
})

test_that("ids subsets the rollup and ignores unknown ids", {
  state <- make_state()
  shiny::isolate({
    SetStatus(state, "HillIN", validationNote = "ERROR: bad")
    expect_equal(GetStateStatus(state, ids = "invitroPRR")$severity, "OK")
    expect_equal(GetStateStatus(state, ids = c("HillIN", "nope"))$severity, "ERROR")
    expect_equal(GetStateStatus(state, ids = "nope")$sourceMix, "none")
  })
})

test_that("source mix reports user override and mixed sources", {
  state <- make_state()
  shiny::isolate({
    SetStatus(state, "HillIN", source = "User input")
    expect_equal(GetStateStatus(state)$sourceMix, "user")
    expect_true(GetStateStatus(state)$hasUserOverride)
    SetStatus(state, "invitroPRR", source = "Science Cloud")
    expect_equal(GetStateStatus(state)$sourceMix, "mixed")
    SetStatus(state, "HillIN", source = "Default value")
    expect_equal(GetStateStatus(state)$sourceMix, "data")
  })
})

test_that("is reactive: depends on status changes", {
  state <- make_state()
  shiny::testServer(function(input, output, session) {
    sev <- reactive(GetStateStatus(state)$severity)
    session$userData$sev <- sev
  }, expr = {
    expect_equal(session$userData$sev(), "OK")
    SetStatus(state, "HillIN", validationNote = "ERROR: x")
    expect_equal(session$userData$sev(), "ERROR")
  })
})

test_that("overriddenIds/overrides list the user-overwritten parameters with value and default", {
  state <- make_state()
  shiny::isolate({
    st0 <- GetStateStatus(state)
    expect_length(st0$overriddenIds, 0)
    expect_equal(nrow(st0$overrides), 0L)
    expect_named(st0$overrides, c("id", "value", "default"))

    state$validated[["HillIN"]] <- 6
    SetStatus(state, "HillIN", source = "User input")
    SetStatus(state, "invitroPRR", source = "Science Cloud")
    st <- GetStateStatus(state)
    expect_equal(st$overriddenIds, "HillIN")
    expect_equal(st$overrides$id, "HillIN")
    expect_equal(st$overrides$value, "6")
    expect_equal(st$overrides$default, "1.3")
    expect_equal(GetStateStatus(state, ids = "invitroPRR")$overriddenIds, character(0))
  })
})

test_that("clearing a user override (NA in the GUI input) resets the source, so the id drops out of overriddenIds", {
  state <- make_state()
  shiny::isolate({
    # User enters a value.
    state$guiInput[["HillIN"]] <- 6
    SetEvent(state, "HillIN", "USER")
    Validate(state, "HillIN")
    st <- GetStateStatus(state)
    expect_equal(st$overriddenIds, "HillIN")
    expect_true(st$hasUserOverride)
    expect_equal(st$sourceMix, "user")

    # User deletes the value => default is used again and the override disappears.
    state$guiInput[["HillIN"]] <- NA_real_
    SetEvent(state, "HillIN", "USER")
    Validate(state, "HillIN")
    st <- GetStateStatus(state)
    expect_length(st$overriddenIds, 0)
    expect_equal(nrow(st$overrides), 0L)
    expect_false(st$hasUserOverride)
    expect_equal(st$sourceMix, "default")
    expect_equal(Get(state, "HillIN"), 1.3)
  })
})

test_that("only the cleared id drops out when several parameters were overridden", {
  state <- make_state()
  shiny::isolate({
    for (id in c("HillIN", "invitroPRR")) {
      state$guiInput[[id]] <- 2
      SetEvent(state, id, "USER")
      Validate(state, id)
    }
    expect_setequal(GetStateStatus(state)$overriddenIds, c("HillIN", "invitroPRR"))

    state$guiInput[["invitroPRR"]] <- NA_real_
    SetEvent(state, "invitroPRR", "USER")
    Validate(state, "invitroPRR")
    expect_equal(GetStateStatus(state)$overriddenIds, "HillIN")
  })
})
