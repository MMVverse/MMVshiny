library(testthat)
library(data.table)

test_that("GenerateJavaScriptEventHandlers binds radio, select and checkbox inputs to 'change' (#31)", {
  spec <- data.table(
    ID   = c("MyRadio", "MySelect", "MyCheck", "MyNum", "MyBtn"),
    TYPE = c("radio input", "select input", "checkbox input", "numeric input", "action button input")
  )
  f <- GenerateJavaScriptEventHandlers(spec, filename = tempfile(fileext = ".js"))
  js <- paste(readLines(f), collapse = "\n")

  expect_match(js, '$("#MyRadio").bind("change"', fixed = TRUE)
  expect_false(grepl('$("#MyRadio").bind("click"', js, fixed = TRUE))
  expect_match(js, '$("#MySelect").bind("change"', fixed = TRUE)
  expect_match(js, '$("#MyCheck").bind("change"', fixed = TRUE)
  expect_match(js, '$("#MyBtn").bind("click"', fixed = TRUE)
  expect_match(js, '$("#MyNum").bind("blur keyup"', fixed = TRUE)
})
