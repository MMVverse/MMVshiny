# SetDisplayed(synchronous = ) pushes to the browser directly (#22) ----

library(data.table)

make_state <- function() {
  spec <- data.table(
    ID = "HillIN", TYPE = "numeric input", GUILABEL = "Label", STATUSICON = TRUE,
    VALEXPR = "1.3", DEFAULTVALUENOTE = "NA", MIN = "0.1", MINNOTE = "NA",
    MAX = "10", MAXNOTE = "NA", DECDIGITS = "2", OUTOFBOUNDS = "keep with error",
    SCFILTER = NA_character_, SCVALUE = NA_character_,
    ExprVAL = "1.3", ExprDEFNOTE = "NA_character_",
    ExprMIN = "0.1", ExprMINNOTE = "NA_character_",
    ExprMAX = "10", ExprMAXNOTE = "NA_character_")
  InitState(spec = spec, stateId = "testSetDisplayedSync")
}

# Record the js$ calls and the update*Input calls instead of talking to a browser ----
with_recorded_push <- function(code) {
  calls <- character()
  fakeJs <- list(
    backgroundCol = function(id, col) calls <<- c(calls, paste("backgroundCol", id, col)),
    fontCol = function(id, col) calls <<- c(calls, paste("fontCol", id, col)))
  testthat::local_mocked_bindings(
    js = fakeJs,
    UpdateNumericInput = function(inputId, value, ...) {
      calls <<- c(calls, paste("UpdateNumericInput", inputId, value))
    })
  force(code)
  calls
}

test_that("synchronous = TRUE pushes colour and value once and still writes state$displayed", {
  state <- make_state()
  calls <- with_recorded_push(shiny::isolate(SetDisplayed(state, "HillIN", 2.345, synchronous = TRUE)))
  expect_length(calls, 3L)
  expect_match(calls[1], "^backgroundCol .*HillIN azure$")
  expect_match(calls[2], "^fontCol .*HillIN black$")
  expect_match(calls[3], "^UpdateNumericInput .*HillIN 2.35$")
  expect_equal(shiny::isolate(GetDisplayed(state, "HillIN")), 2.35)
})

test_that("synchronous = FALSE (default) only writes state$displayed", {
  state <- make_state()
  calls <- with_recorded_push(shiny::isolate(SetDisplayed(state, "HillIN", 2.345)))
  expect_length(calls, 0L)
  expect_equal(shiny::isolate(GetDisplayed(state, "HillIN")), 2.35)
})

test_that("state$synchronousDisplayed makes synchronous the default; the argument overrides it", {
  state <- make_state()
  state$synchronousDisplayed <- TRUE
  calls <- with_recorded_push(shiny::isolate(SetDisplayed(state, "HillIN", 3)))
  expect_length(calls, 3L)
  calls <- with_recorded_push(shiny::isolate(SetDisplayed(state, "HillIN", 4, synchronous = FALSE)))
  expect_length(calls, 0L)
})

test_that("the colour follows the source of the value", {
  state <- make_state()
  calls <- with_recorded_push(shiny::isolate({
    SetStatus(state, "HillIN", source = "User input")
    SetDisplayed(state, "HillIN", 3, synchronous = TRUE)
  }))
  expect_match(calls[1], "^backgroundCol .*HillIN white$")
})

test_that("SetDisplayed still rejects ids of an unsupported type", {
  state <- make_state()
  expect_error(shiny::isolate(SetDisplayed(state, "nope", 1, synchronous = TRUE)))
})
