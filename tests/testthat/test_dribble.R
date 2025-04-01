
library(testthat)

test_that("dribble creates data.frame rowwise", {
  listEntries <- list(
    "Parameter",                                "Value",          "Unit",
    "Selected PK method",                   "Allometry",     "PK method",
    "Species Available for Allometry",                2,       "Species",
    "Predicted plasma CL in man",                   3.2,     "mL/min/kg"
  )
  expected <- data.frame(
    Parameter = c("Selected PK method", "Species Available for Allometry", "Predicted plasma CL in man"),
    Value = c("Allometry", 2, 3.2),
    Unit = c("PK method", "Species", "mL/min/kg"),
    stringsAsFactors = FALSE
  )
  expect_equal(dribble(listEntries, ncol = 3), expected)
})

test_that("dribble raises error when number of entries does not divide on ncol", {
  listEntries <- list(
    "Parameter",                                "Value",          "Unit",
    "Selected PK method",                   "Allometry",     "PK method",
    "Species Available for Allometry",                2,       "Species",
    "Predicted plasma CL in man",                   3.2,     "mL/min/kg",
    "Extra entry"
  )
  expect_error(dribble(listEntries, ncol = 3), "dribble: number of entries does not divide on ncol.")
})
