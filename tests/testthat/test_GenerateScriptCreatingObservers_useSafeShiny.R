library(testthat)
library(shiny)
library(data.table)

make_numeric_spec <- function(id) {
  # InitState() reads the Expr* columns directly (normally derived from VALEXPR/MIN/MAX/etc. by
  # LoadStateSpecification()'s preprocessing, which this minimal hand-built spec skips) - set
  # them directly instead, as plain literal-value expressions (no Get(...) substitution needed).
  data.table(
    ID = id, TYPE = "numeric input", GUILABEL = "Label", STATUSICON = FALSE,
    VALEXPR = "1.3", DEFAULTVALUENOTE = "NA", MIN = "0.1", MINNOTE = "NA",
    MAX = "10", MAXNOTE = "NA", DECDIGITS = "2", OUTOFBOUNDS = "nearest",
    SCFILTER = NA_character_, SCVALUE = NA_character_,
    ExprVAL = "1.3", ExprDEFNOTE = "NA_character_",
    ExprMIN = "0.1", ExprMINNOTE = "NA_character_",
    ExprMAX = "10", ExprMAXNOTE = "NA_character_"
  )
}

test_that("useSafeShiny = FALSE (default) generates plain observeEvent() calls, unchanged", {
  spec <- make_numeric_spec("HillIN")
  outfile <- tempfile(fileext = ".R")
  GenerateScriptCreatingObservers(spec = spec, ids = "HillIN", observerTypes = "countID", filename = outfile)
  code <- paste(readLines(outfile), collapse = "\n")

  expect_true(grepl("observeEvent(input$countHillIN", code, fixed = TRUE))
  expect_false(grepl("SafeShiny", code, fixed = TRUE))
  expect_false(grepl("onError", code, fixed = TRUE))
})

test_that("useSafeShiny = TRUE generates SafeShiny::SafeObserveEvent() with a status/icon onError", {
  spec <- make_numeric_spec("HillIN")
  outfile <- tempfile(fileext = ".R")
  GenerateScriptCreatingObservers(
    spec = spec, ids = "HillIN", observerTypes = "countID", useSafeShiny = TRUE, filename = outfile
  )
  code <- paste(readLines(outfile), collapse = "\n")

  expect_true(grepl("SafeShiny::SafeObserveEvent(input$countHillIN", code, fixed = TRUE))
  expect_true(grepl('SetStatus("HillIN", validationNote = paste0("ERROR: "', code, fixed = TRUE))
  expect_true(grepl('SetInfoIcon("HillIN")', code, fixed = TRUE))
})

test_that("useSafeShiny = TRUE errors clearly at generation time when SafeShiny isn't installed", {
  testthat::local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")

  spec <- make_numeric_spec("HillIN")
  expect_error(
    GenerateScriptCreatingObservers(spec = spec, ids = "HillIN", observerTypes = "countID", useSafeShiny = TRUE),
    "SafeShiny"
  )
})

test_that("useSafeShiny = TRUE: an error in the generated observer is caught, recorded as state status, and the session keeps working", {
  spec <- make_numeric_spec("HillIN")
  state <- InitState(spec = spec, stateId = "testStateError")

  outfile <- tempfile(fileext = ".R")
  GenerateScriptCreatingObservers(
    spec = spec, ids = "HillIN", observerTypes = "countID", useSafeShiny = TRUE, filename = outfile
  )

  shiny::testServer(function(input, output, session) {
    stateObj <- state
    inputObj <- input
    outputObj <- output
    # Stub out the real handler so this test exercises only the Safe wrapper, not
    # ProcessGuiInputEvent()'s own (unrelated) behavior.
    ProcessGuiInputEvent <- function(...) stop("deliberate test error")

    source(outfile, local = TRUE)

    pingCount <- 0
    SafeShiny::SafeObserveEvent(input$ping, {
      pingCount <<- pingCount + 1
    }, ignoreInit = TRUE)

    session$getStatus <- function() GetStatus(stateObj, "HillIN")
    session$getPingCount <- function() pingCount
  }, expr = {
    session$flushReact() # let observers settle/register their baseline first

    session$setInputs(countHillIN = 1)
    session$flushReact()

    status <- session$getStatus()
    expect_true(grepl("^ERROR: deliberate test error", status$validationNote))

    # the session must still be fully functional afterward - a second, unrelated observer
    # still fires normally.
    session$setInputs(ping = 1)
    session$flushReact()
    expect_equal(session$getPingCount(), 1)
  })
})

test_that("useSafeShiny = TRUE: a req()-style silent stop is not misreported as an ERROR status", {
  spec <- make_numeric_spec("HillIN")
  state <- InitState(spec = spec, stateId = "testStateReq")
  baselineNote <- shiny::isolate(GetStatus(state, "HillIN")$validationNote)

  outfile <- tempfile(fileext = ".R")
  GenerateScriptCreatingObservers(
    spec = spec, ids = "HillIN", observerTypes = "countID", useSafeShiny = TRUE, filename = outfile
  )

  shiny::testServer(function(input, output, session) {
    stateObj <- state
    inputObj <- input
    outputObj <- output
    # A real validation short-circuit (shiny::req()/validate()) is not a bug - it must not be
    # recorded as an ERROR status.
    ProcessGuiInputEvent <- function(...) shiny::req(FALSE)

    source(outfile, local = TRUE)

    session$getStatus <- function() GetStatus(stateObj, "HillIN")
  }, expr = {
    session$flushReact()

    session$setInputs(countHillIN = 1)
    session$flushReact()

    status <- session$getStatus()
    expect_equal(status$validationNote, baselineNote)
    expect_false(isTRUE(grepl("^ERROR:", status$validationNote)))
  })
})
