library(testthat)
library(shiny)
library(data.table)

make_icon_spec <- function(id) {
  data.table(ID = id, TYPE = "numeric input", STATUSICON = TRUE)
}

test_that("useSafeShiny = FALSE (default) generates plain renderUI() calls, unchanged", {
  spec <- make_icon_spec("HillIN")
  outfile <- tempfile(fileext = ".R")
  GenerateScriptRenderingIcons(spec = spec, ids = "HillIN", filename = outfile)
  code <- paste(readLines(outfile), collapse = "\n")

  expect_true(grepl('<- renderUI({', code, fixed = TRUE))
  expect_false(grepl("SafeShiny", code, fixed = TRUE))
})

test_that("useSafeShiny = TRUE generates SafeShiny::SafeRenderUI() calls", {
  spec <- make_icon_spec("HillIN")
  outfile <- tempfile(fileext = ".R")
  GenerateScriptRenderingIcons(spec = spec, ids = "HillIN", useSafeShiny = TRUE, filename = outfile)
  code <- paste(readLines(outfile), collapse = "\n")

  expect_true(grepl('<- SafeShiny::SafeRenderUI({', code, fixed = TRUE))
})

test_that("useSafeShiny = TRUE errors clearly at generation time when SafeShiny isn't installed", {
  testthat::local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")

  spec <- make_icon_spec("HillIN")
  expect_error(
    GenerateScriptRenderingIcons(spec = spec, ids = "HillIN", useSafeShiny = TRUE),
    "SafeShiny"
  )
})

test_that("useSafeShiny = TRUE: an error in the generated icon render is caught (Shiny's own error display still happens) and the session keeps working", {
  spec <- make_icon_spec("HillIN")
  state <- InitState(spec = data.table(
    ID = "HillIN", TYPE = "numeric input", GUILABEL = "Label", STATUSICON = TRUE,
    VALEXPR = "1.3", DEFAULTVALUENOTE = "NA", MIN = "0.1", MINNOTE = "NA",
    MAX = "10", MAXNOTE = "NA", DECDIGITS = "2", OUTOFBOUNDS = "nearest",
    SCFILTER = NA_character_, SCVALUE = NA_character_,
    ExprVAL = "1.3", ExprDEFNOTE = "NA_character_",
    ExprMIN = "0.1", ExprMINNOTE = "NA_character_",
    ExprMAX = "10", ExprMAXNOTE = "NA_character_"
  ), stateId = "testStateIcon")

  outfile <- tempfile(fileext = ".R")
  GenerateScriptRenderingIcons(spec = spec, ids = "HillIN", useSafeShiny = TRUE, filename = outfile)

  shiny::testServer(function(input, output, session) {
    stateObj <- state
    # Force the icon's own statusIconClass lookup to throw ("subscript out of bounds", from
    # [[ on an atomic vector with a non-existent name) to exercise the Safe wrapper in
    # isolation - this does not touch state$status itself (see roxygen note: no onError is
    # wired here, to avoid a confusing circularity).
    stateObj$statusIconClass <- character(0)
    source(outfile, local = TRUE)

    stillAlive <- FALSE
    SafeShiny::SafeObserveEvent(input$ping, {
      stillAlive <<- TRUE
    }, ignoreInit = TRUE)

    session$getStillAlive <- function() stillAlive
  }, expr = {
    session$flushReact()
    # Shiny's own behavior for a failed renderUI(): the output errors (testServer's
    # getOutput() surfaces this the same way a real browser would see a local error display) -
    # SafeRenderUI() doesn't change that, it only adds the console log line already seen above.
    expect_error(session$getOutput("HillINicon"), "subscript out of bounds")

    session$setInputs(ping = 1)
    session$flushReact()
    expect_true(session$getStillAlive())
  })
})
