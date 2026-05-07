#' @importFrom magrittr %>%
#' @importFrom stats na.omit
#' @importFrom data.table := .SD data.table
#' @importFrom shiny actionButton checkboxInput numericInput radioButtons
#'   selectInput tags textInput uiOutput getDefaultReactiveDomain
#'   updateCheckboxInput updateNumericInput updateRadioButtons
#'   updateSelectInput updateTextInput
#' @importFrom shinyjs js
NULL

utils::globalVariables(c(
  # data.table column names used in NSE
  "ID", "TYPE", "GUILABEL", "STATUSICON", "SOURCE", "UNIT", "DECDIGITS",
  "RADIOVALUES", "OUTOFBOUNDS", "MIN", "MINNOTE", "MAX", "MAXNOTE",
  "DEFAULTVALUENOTE", "VALEXPR", "SCFILTER", "SCVALUE", "REPORTLABEL",
  "NAME", "NAVAL", "PARAMETERS AFFECTED", "ACTIONEXPR",
  # generated expression column names
  "ExprVAL", "ExprDEFNOTE", "ExprMIN", "ExprMINNOTE", "ExprMAX", "ExprMAXNOTE",
  "ExprACTION",
  # shinyjs object reference
  "js"
))

cat("Setting option MMVshiny.verbose to FALSE\n")
options("MMVshiny.verbose" = FALSE)
options("MMVshiny.source2FontColour" = c(`Default value` = "black",
                                         `User input` = "black",
                                         `Science Cloud` = "black"))
options("MMVshiny.source2BackgroundColour" = c(`Default value` = "azure",
                                               `User input` = "white",
                                               `Science Cloud` = "ivory"))
