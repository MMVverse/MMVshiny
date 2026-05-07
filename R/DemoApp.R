#' Run the MMVshiny demo app
#'
#' Extracts the bundled demo app to a temporary directory and launches it with
#' [shiny::runApp()].  Any extra arguments are forwarded to `runApp()`.
#'
#' @param ... additional arguments passed to [shiny::runApp()] (e.g. `port`,
#'   `launch.browser`).
#'
#' @return called for its side-effect; returns the result of `shiny::runApp()`
#'   invisibly.
#' @export
DemoApp <- function(...) {
  zip_path <- system.file("extdata", "MMVshinyDemo.zip", package = "MMVshiny")
  if (!nzchar(zip_path)) {
    stop("MMVshinyDemo.zip not found in MMVshiny installation.")
  }
  app_dir <- file.path(tempdir(), "MMVshinyDemo")
  if (dir.exists(app_dir)) unlink(app_dir, recursive = TRUE)
  utils::unzip(zip_path, exdir = dirname(app_dir))
  shiny::runApp(app_dir, ...)
}
