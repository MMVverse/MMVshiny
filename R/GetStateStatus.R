#' Roll up the per-parameter status of a state object into a state-level status
#'
#' Walks \code{state$status} (see \code{\link{GetStatus}}) and answers two questions about a
#' compound-state as a whole: "is anything wrong?" (worst-of severity over the validation notes)
#' and "where do the values come from?" (mix of sources, e.g. whether the user overrode anything).
#'
#' @param state state object created by \code{\link{InitState}}.
#' @param ids character vector or \code{NULL}: the parameter ids to include. Default \code{NULL}
#' includes every id in \code{state$status}. Ids without a status entry are ignored.
#'
#' @return a list with elements:
#' \itemize{
#' \item \code{severity}: string, the worst severity over the included ids: \code{"OK"} < \code{"WARN"} < \code{"ERROR"}.
#' An id whose \code{validationNote} is missing counts as \code{"OK"}.
#' \item \code{nOK, nWARN, nERROR}: integer counts of included ids per severity.
#' \item \code{sourceMix}: string, one of \code{"default"} (all values are defaults), \code{"data"} (no
#' user input, at least one Science Cloud/report value), \code{"user"} (every non-default value is a
#' user input), \code{"mixed"} (user inputs together with Science Cloud/report values) or
#' \code{"none"} (no ids).
#' \item \code{hasUserOverride}: logical, \code{TRUE} if at least one included id has source \code{"User input"}.
#' \item \code{overriddenIds}: character vector, the included ids whose value was entered by the user (source \code{"User input"}).
#' \item \code{overrides}: \code{data.frame} with one row per overridden id and columns \code{id}, \code{value}
#' (the current validated value) and \code{default} (the value the parameter would have without the override), both as
#' character strings. Zero rows if nothing was overridden. This is the basis for asking the user to document
#' the reason for each change (e.g. before generating a report).
#' \item \code{details}: \code{data.frame} with one row per included id and columns \code{id},
#' \code{severity}, \code{source}, \code{note} (validation note(s) without the severity prefix, collapsed with newlines).
#' Rows are ordered by decreasing severity, so \code{head(details)} lists the most important problems.
#' }
#'
#' @details
#' Like \code{GetStatus()}, this reads reactive values, so it must be called inside a reactive
#' context (or \code{shiny::isolate()}). Called from a reactive it depends on the status of
#' every included id, so a displayed state-level indicator updates automatically.
#'
#' Status entries created by error-catching wrappers (\code{validationNote = "ERROR: ..."}, set by the
#' code generated with \code{useSafeShiny = TRUE}) are included, so a failed computation shows up
#' as \code{"ERROR"} here. An \code{"ERROR"} severity is a signal, not a switch: consumers should
#' degrade only the outputs that depend on the failed values.
#'
#' @examples
#' \dontrun{
#' shiny::isolate({
#'   st <- GetStateStatus(state)
#'   st$severity
#'   GetStateStatus(state, ids = c("HillIN", "invitroPRR"))$details
#' })
#' }
#' @export
GetStateStatus <- function(state, ids = NULL) {
  allIds <- names(state$status)
  ids <- if (is.null(ids)) allIds else intersect(ids, allIds)

  severityLevels <- c("OK", "WARN", "ERROR")

  rows <- lapply(ids, function(id) {
    status <- GetStatus(state, id)
    notes <- as.character(status$validationNote)
    notes <- notes[!is.na(notes) & nzchar(notes)]
    severity <- if (any(startsWith(notes, "ERROR:"))) "ERROR"
                else if (any(startsWith(notes, "WARN:"))) "WARN"
                else "OK"
    source <- status$source
    data.frame(
      id = id,
      severity = severity,
      source = if (is.null(source) || length(source) == 0) NA_character_ else as.character(source)[1],
      note = paste(trimws(sub("^(OK|WARN|ERROR):", "", notes)), collapse = "\n"),
      stringsAsFactors = FALSE)
  })
  details <- if (length(rows) > 0) {
    do.call(rbind, rows)
  } else {
    data.frame(id = character(0), severity = character(0), source = character(0),
               note = character(0), stringsAsFactors = FALSE)
  }
  details <- details[order(-match(details$severity, severityLevels)), , drop = FALSE]
  rownames(details) <- NULL

  nOf <- function(s) sum(details$severity == s)
  worst <- if (nOf("ERROR") > 0) "ERROR" else if (nOf("WARN") > 0) "WARN" else "OK"

  src <- tolower(details$source)
  hasUser <- any(startsWith(src, "user"), na.rm = TRUE)
  hasData <- any(!is.na(src) & !startsWith(src, "default") & !startsWith(src, "user"))
  sourceMix <- if (nrow(details) == 0) "none"
               else if (hasUser && hasData) "mixed"
               else if (hasUser) "user"
               else if (hasData) "data"
               else "default"

  overriddenIds <- details$id[!is.na(src) & startsWith(src, "user")]
  asChar <- function(f, id) tryCatch({
    v <- f(state, id)
    if (length(v) == 0) NA_character_ else as.character(v)[1]
  }, error = function(e) NA_character_)
  overrides <- data.frame(
    id = overriddenIds,
    value = vapply(overriddenIds, asChar, character(1), f = Get, USE.NAMES = FALSE),
    default = vapply(overriddenIds, asChar, character(1), f = GetDefault, USE.NAMES = FALSE),
    stringsAsFactors = FALSE)

  list(
    severity = worst,
    nOK = nOf("OK"), nWARN = nOf("WARN"), nERROR = nOf("ERROR"),
    sourceMix = sourceMix,
    hasUserOverride = hasUser,
    overriddenIds = overriddenIds,
    overrides = overrides,
    details = details)
}
