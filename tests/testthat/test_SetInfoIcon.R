library(testthat)
library(shiny)
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
  InitState(spec = spec, stateId = "testSII")
}

icon <- function(state) {
  shiny::isolate(list(class = state$statusIconClass[["HillIN"]],
                      color = state$statusIconColor[["HillIN"]],
                      title = state$statusTitle[["HillIN"]],
                      text = state$statusText[["HillIN"]]))
}

test_that("default note without prefix keeps the blue info icon (#33)", {
  state <- make_state()
  shiny::isolate({
    state$defaultNote[["HillIN"]] <- "Predicted using X"
    SetStatus(state, "HillIN", source = "Default value", validationNote = NA_character_)
    SetInfoIcon(state, "HillIN")
  })
  i <- icon(state)
  expect_equal(i$class, "glyphicon glyphicon-info-sign")
  expect_equal(i$title, "Predicted using X")
})

test_that("default note starting with WARN: gives the orange icon and the prefix is stripped (#33)", {
  state <- make_state()
  shiny::isolate({
    state$defaultNote[["HillIN"]] <- function() "WARN: No prediction available."
    SetStatus(state, "HillIN", source = "Default value", validationNote = NA_character_)
    SetInfoIcon(state, "HillIN")
  })
  i <- icon(state)
  expect_equal(i$class, "glyphicon glyphicon-exclamation-sign")
  expect_equal(i$color, "#ff8533")
  expect_equal(i$title, "No prediction available.")
})

test_that("default note starting with ERROR: gives the red icon (#33)", {
  state <- make_state()
  shiny::isolate({
    state$defaultNote[["HillIN"]] <- "ERROR: Cannot compute."
    SetStatus(state, "HillIN", source = "Default value", validationNote = NA_character_)
    SetInfoIcon(state, "HillIN")
  })
  i <- icon(state)
  expect_equal(i$color, "#ff0000")
  expect_equal(i$title, "Cannot compute.")
})

test_that("the default note is ignored when the source is not a default (#33)", {
  state <- make_state()
  shiny::isolate({
    state$defaultNote[["HillIN"]] <- "WARN: No prediction available."
    SetStatus(state, "HillIN", source = "User input", validationNote = "OK: fine")
    SetInfoIcon(state, "HillIN")
  })
  i <- icon(state)
  expect_equal(i$class, "glyphicon glyphicon-ok-circle")
  expect_equal(i$title, "User input")
})

test_that("SetInfoIcon is exported and refreshes the icon after SetStatus with an ERROR (#36)", {
  expect_true("SetInfoIcon" %in% getNamespaceExports("MMVshiny"))
  state <- make_state()
  shiny::isolate({
    SetStatus(state, "HillIN", source = "User input", validationNote = "ERROR: something went wrong")
    # as the onError handlers of the generated scripts do
    expect_no_error(state %>% SetInfoIcon("HillIN"))
  })
  i <- icon(state)
  expect_equal(i$class, "glyphicon glyphicon-exclamation-sign")
  expect_equal(i$color, "#ff0000")
})

test_that("GetMinNote without a state reads MINNOTE, not MIN (#35)", {
  spec <- data.table(ID = "HillIN", TYPE = "numeric input", MIN = "0.1", MINNOTE = "Lower bound note")
  expect_equal(GetMinNote(id = "HillIN", spec = spec), "Lower bound note")
})
