
library(testthat)
library(shiny)
library(data.table)

make_spec <- function(id, type, label = "Label", statusicon = FALSE) {
  data.table(
    ID          = id,
    TYPE        = type,
    GUILABEL    = label,
    STATUSICON  = statusicon
  )
}

test_that("CreateUIInput returns checkboxInput for 'checkbox input' type", {
  spec <- make_spec("MyCheckbox", "checkbox input", label = "Show curve")
  result <- CreateUIInput(state = NULL, id = "MyCheckbox", spec = spec, useLabel = TRUE)
  expect_type(result, "list")
  # Returns a list with one td (no status icon)
  expect_length(result, 1L)
  td_html <- as.character(result[[1]])
  expect_true(grepl('type="checkbox"', td_html))
  expect_true(grepl('id="MyCheckbox"', td_html))
})

test_that("NAVal returns NA for 'checkbox input' type", {
  spec <- make_spec("MyCheckbox", "checkbox input")
  val <- NAVal(id = "MyCheckbox", spec = spec)
  expect_true(is.na(val))
  expect_type(val, "logical")
})

test_that("GenerateJavaScriptEventHandlers writes change handler for 'checkbox input'", {
  spec <- make_spec("MyCheckbox", "checkbox input")
  outfile <- tempfile(fileext = ".js")
  GenerateJavaScriptEventHandlers(spec = spec, ids = "MyCheckbox", filename = outfile)
  js <- paste(readLines(outfile), collapse = "\n")
  expect_true(grepl('"#MyCheckbox"', js))
  expect_true(grepl('"change"', js))
  expect_true(grepl('countMyCheckbox', js))
})
