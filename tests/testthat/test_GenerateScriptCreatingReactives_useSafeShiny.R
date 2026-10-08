library(testthat)
library(data.table)

test_that("useSafeShiny = FALSE (default) generates plain reactive() calls, unchanged", {
  spec <- data.table(ID = "HillIN")
  outfile <- tempfile(fileext = ".R")
  GenerateScriptCreatingReactives(spec = spec, filename = outfile)
  code <- paste(readLines(outfile, warn = FALSE), collapse = "\n")
  expect_true(grepl('HillIN <- reactive({Get(state, "HillIN")})', code, fixed = TRUE))
  expect_false(grepl("SafeShiny", code, fixed = TRUE))
})

test_that("useSafeShiny = TRUE generates SafeShiny::SafeReactive() with a status/icon onError", {
  skip_if_not_installed("SafeShiny", minimum_version = "0.3.0")
  spec <- data.table(ID = "HillIN")
  outfile <- tempfile(fileext = ".R")
  GenerateScriptCreatingReactives(spec = spec, useSafeShiny = TRUE, filename = outfile)
  code <- paste(readLines(outfile, warn = FALSE), collapse = "\n")
  expect_true(grepl('HillIN <- SafeShiny::SafeReactive({Get(state, "HillIN")}', code, fixed = TRUE))
  expect_true(grepl('SetStatus("HillIN"', code, fixed = TRUE))
  expect_true(grepl('SetInfoIcon("HillIN")', code, fixed = TRUE))
  expect_silent(parse(outfile))
})

test_that("useSafeShiny = TRUE labels the generated reactive with its ID and the script parses", {
  spec <- data.table(ID = "HillIN")
  outfile <- tempfile(fileext = ".R")
  GenerateScriptCreatingReactives(spec = spec, useSafeShiny = TRUE, filename = outfile)
  expect_true(grepl('label = "HillIN"', paste(readLines(outfile), collapse = "\n"), fixed = TRUE))
  expect_error(parse(outfile), NA)
})
