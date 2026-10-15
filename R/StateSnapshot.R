#-------------------------------------------------------------------------#
# Seeded initial state ----
#-------------------------------------------------------------------------#
# Every new session used to recompute the same fixed-point initial state: the
# reset, default and max observers of all inputs fire once on creation, validate
# the default values and write validated/displayed/status/icon values into the
# state. The functions below settle one live state once per R process, keep a
# snapshot of it, and let InitState(seed = TRUE) write the snapshot into new
# states before their observers are created, so the observers (guarded by
# SkipSeededRun, see the `seedable` argument of GenerateScriptCreatingObservers)
# skip their first run when nothing they depend on has changed.

# Process-wide cache of snapshots: key -> snapshot.
snapshotCache <- new.env()

# Names of the reactiveValues of a state which a snapshot holds, one value per input id.
snapshotFields <- c("validated", "displayed", "status", "event", "resetCount",
                    "statusIconClass", "statusIconColor", "statusTitle", "statusText")

# Run expr in isolate(), as a SafeShiny::SafeReactive() with the given label if SafeShiny
# is installed, so that the step shows up in the SafeShiny timing output and flame chart.
labelledIsolate <- function(expr, label) {
  expr <- substitute(expr)
  env <- parent.frame()
  if(requireNamespace("SafeShiny", quietly = TRUE) &&
     utils::packageVersion("SafeShiny") >= "0.3.0") {
    r <- SafeShiny::SafeReactive(eval(expr, env), label = label,
                                 onError = function(e) stop(e))
    shiny::isolate(r())
  } else {
    shiny::isolate(eval(expr, env))
  }
}

#' Extract a snapshot of the initialised values of a state
#'
#' @param state a state object, as returned by \code{\link{InitState}}.
#' @param ids a character vector of input ids to include. Default: all numeric, text,
#' radio, select and checkbox inputs in \code{state$spec}.
#'
#' @return a list of class \code{MMVshinyStateSnapshot} with one named list element per
#' field: the validated value, displayed value, status (list with source, valid, note, ...),
#' event, resetCount, status icon class, colour, title and text of each id, together with
#' the current default, min (numeric inputs only) and max (numeric inputs only) values
#' (informational - these are re-computed live in each session, not seeded), each a named
#' list indexed by id.
#'
#' @details The values are read in \code{isolate()}.
#'
#' @examples
#' spec <- data.table::data.table(
#'   ID = "HillIN", TYPE = "numeric input", GUILABEL = "Label", STATUSICON = TRUE,
#'   VALEXPR = "1.3", DEFAULTVALUENOTE = "NA", MIN = "0.1", MINNOTE = "NA",
#'   MAX = "10", MAXNOTE = "NA", DECDIGITS = "2", OUTOFBOUNDS = "keep with error",
#'   SCFILTER = NA_character_, SCVALUE = NA_character_,
#'   ExprVAL = "1.3", ExprDEFNOTE = "NA_character_", ExprMIN = "0.1",
#'   ExprMINNOTE = "NA_character_", ExprMAX = "10", ExprMAXNOTE = "NA_character_")
#' state <- InitState(spec, stateId = "exampleSnapshot")
#' names(ExtractStateSnapshot(state))
#' @export
ExtractStateSnapshot <- function(state, ids = NULL) {
  spec <- state$spec
  inputTypes <- c("numeric input", "text input", "radio input", "select input", "checkbox input")
  if(is.null(ids)) {
    ids <- spec$ID[vapply(spec$ID, function(id) GetType(id = id, spec = spec) %in% inputTypes, logical(1))]
  }
  snapshot <- shiny::isolate({
    res <- lapply(snapshotFields, function(field) {
      lapply(stats::setNames(ids, ids), function(id) state[[field]][[id]])
    })
    names(res) <- snapshotFields
    res$default <- lapply(stats::setNames(ids, ids), function(id) GetDefault(state, id))
    numIds <- ids[vapply(ids, function(id) GetType(state, id) == "numeric input", logical(1))]
    res$min <- lapply(stats::setNames(numIds, numIds), function(id) GetMin(state, id))
    res$max <- lapply(stats::setNames(numIds, numIds), function(id) GetMax(state, id))
    res
  })
  class(snapshot) <- "MMVshinyStateSnapshot"
  snapshot
}

#' Decide whether the first run of a seeded observer can be skipped
#'
#' Called by the observers generated with \code{GenerateScriptCreatingObservers(seedable = TRUE)};
#' not meant to be called directly. See \code{\link{InitState}}, section "Seeded states". Must be
#' called in a reactive context or \code{isolate()}.
#'
#' @param state a state object.
#' @param kind character string, one of \code{"default"}, \code{"min"}, \code{"max"}.
#' @param id character string, the input id.
#' @param value the current value of the event expression of the observer.
#' @return logical; \code{TRUE} only for the first call per \code{kind} and \code{id} in a seeded
#' state when nothing changed since seeding, i.e. the handler can be skipped.
#' @keywords internal
#' @examples
#' SkipSeededRun(new.env(), "default", "A", 1)  # unseeded state: FALSE
#' @export
SkipSeededRun <- function(state, kind, id, value) {
  if(!isTRUE(state$seeded)) {
    return(FALSE)
  }
  key <- paste(kind, id, sep = ":")
  if(isTRUE(state$seedConsumed[[key]])) {
    return(FALSE)
  }
  state$seedConsumed[[key]] <- TRUE
  snapshot <- state$seedSnapshot
  # The event value must be unchanged, and so must the state of the id itself: e.g. a report
  # upload sets validated = NA with source "Default value" which the first run would repair.
  identical(value, snapshot[[kind]][[id]]) &&
    identical(state$validated[[id]], snapshot$validated[[id]]) &&
    identical(state$status[[id]]$source, snapshot$status[[id]]$source) &&
    identical(state$event[[id]], snapshot$event[[id]])
}

# Write the snapshot values into the reactiveValues of a state.
SeedState <- function(state, snapshot) {
  labelledIsolate({
    for(field in snapshotFields) {
      for(id in names(snapshot[[field]])) {
        state[[field]][[id]] <- snapshot[[field]][[id]]
      }
    }
  }, label = "seedState")
  invisible(state)
}

# TRUE for plain data: NULL, atomic vectors, and (data) frames/lists of those. FALSE for anything
# tied to a session or process (functions, environments, S4 objects such as Java references,
# external pointers, classed non-data lists such as shiny's reactivevalues), whose hash would change
# with every session/process.
IsPlainData <- function(x) {
  if(is.null(x)) {
    return(TRUE)
  }
  if(is.function(x) || is.environment(x) || isS4(x) || typeof(x) %in% c("externalptr", "weakref", "language", "symbol")) {
    return(FALSE)
  }
  if(is.atomic(x)) {
    return(TRUE)
  }
  if(is.list(x)) {
    if(!is.null(attr(x, "class")) && !inherits(x, "data.frame")) {
      return(FALSE)
    }
    return(all(vapply(x, IsPlainData, logical(1))))
  }
  FALSE
}

# The spec as it enters the cache key: sorted by ID with data.table's key attribute dropped.
# InitState() sorts and keys (a copy of) the spec, so the spec which a session passes to
# InitState(seed = TRUE) arrives sorted, whereas the one given to BuildStateSnapshot() at startup
# may not be: the key must not depend on the row order.
CanonicalSpecForKey <- function(spec) {
  spec <- data.table::as.data.table(spec)
  df <- as.data.frame(spec[order(spec$ID), ])
  attr(df, "sorted") <- NULL
  attr(df, ".internal.selfref") <- NULL
  df
}

# Cache key: spec, contents of the scripts, names of listObjects and the values of its plain-data
# non-empty elements only (empty ones - e.g. NULL or list(), usable as surrogates at build time - carry
# no data; the other elements - e.g. input, a Java parser - are supplied live to every state
# by InitState(), with values right for the new session/process, and never stored in a snapshot),
# and the versions of the packages the state computation depends on.
StateSnapshotKey <- function(spec, listObjects = NULL, scriptsToSource = NULL, keyExtra = NULL) {
  scriptText <- lapply(scriptsToSource, function(f) if(file.exists(f)) readLines(f, warn = FALSE) else NA_character_)
  plainObjects <- Filter(function(x) IsPlainData(x) && length(x) > 0, listObjects)
  pkgs <- c("MMVshiny", "shiny", "SafeShiny")
  pkgVersions <- vapply(pkgs, function(p) {
    tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_)
  }, character(1))
  digest::digest(list(CanonicalSpecForKey(spec), scriptText, names(listObjects), plainObjects,
                      pkgVersions, keyExtra))
}

# On-disk cache ----
# Snapshots are also kept as RDS files, so that a new R process (app-process restart, server
# reboot, idle respawn) loads the snapshot instead of rebuilding it. The folder belongs to the
# deployed app (see SnapshotCacheDir), so deploying a new version of the app starts with an empty
# cache; within a folder the file name is the cache key, so a changed spec, script, listObjects
# value or package version selects another file, and files not used for
# MMVshiny.snapshotMaxAgeDays days (default 30) are removed when a snapshot is written.

# Directory of the on-disk cache, or NULL if disabled. Option MMVshiny.snapshotCacheDir: a path,
# or FALSE (or "") to disable; unset: a folder ".MMVshiny-cache" in the app's home folder, i.e. the
# working directory, which Shiny sets to the app directory (the same place the apps write their
# generated files to, so writable by the user running the app).
SnapshotCacheDir <- function() {
  dir <- getOption("MMVshiny.snapshotCacheDir", file.path(getwd(), ".MMVshiny-cache"))
  if(is.null(dir) || isFALSE(dir) || !is.character(dir) || length(dir) != 1L || is.na(dir) || !nzchar(dir)) {
    return(NULL)
  }
  dir
}

SnapshotCacheFile <- function(dir, key) {
  file.path(dir, paste0("snapshot_", key, ".rds"))
}

# The snapshot stored under key, or NULL if there is none or the file is unreadable/invalid.
ReadSnapshotFromDisk <- function(key) {
  dir <- SnapshotCacheDir()
  if(is.null(dir)) {
    return(NULL)
  }
  file <- SnapshotCacheFile(dir, key)
  if(!file.exists(file)) {
    return(NULL)
  }
  snapshot <- tryCatch(readRDS(file), error = function(e) NULL)
  if(!inherits(snapshot, "MMVshinyStateSnapshot") || !is.list(snapshot)) {
    try(unlink(file), silent = TRUE)
    return(NULL)
  }
  try(Sys.setFileTime(file, Sys.time()), silent = TRUE)  # keep files in use from being pruned
  snapshot
}

# Store the snapshot under key; never fails (a cache that cannot be written is simply not used).
WriteSnapshotToDisk <- function(key, snapshot) {
  dir <- SnapshotCacheDir()
  if(is.null(dir)) {
    return(invisible(FALSE))
  }
  ok <- tryCatch({
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    # Self-ignoring folder (like renv's): apps need no .gitignore entry for it.
    if(!file.exists(file.path(dir, ".gitignore"))) writeLines("*", file.path(dir, ".gitignore"))
    tmp <- tempfile("snapshot_", tmpdir = dir, fileext = ".tmp")
    on.exit(unlink(tmp), add = TRUE)
    saveRDS(snapshot, tmp)
    file.rename(tmp, SnapshotCacheFile(dir, key))  # atomic: concurrent processes never see a partial file
  }, error = function(e) FALSE, warning = function(w) FALSE)
  PruneSnapshotCacheDir(dir)
  invisible(isTRUE(ok))
}

PruneSnapshotCacheDir <- function(dir) {
  maxAgeDays <- getOption("MMVshiny.snapshotMaxAgeDays", 30)
  try({
    files <- list.files(dir, pattern = "^snapshot_.*\\.(rds|tmp)$", full.names = TRUE)
    age <- as.numeric(difftime(Sys.time(), file.mtime(files), units = "days"))
    unlink(files[!is.na(age) & age > ifelse(grepl("\\.tmp$", files), 1, maxAgeDays)])
  }, silent = TRUE)
  invisible(NULL)
}

#' Build, cache and return the snapshot of the initialised state for a specification
#'
#' Creates a live state with \code{InitState(seed = FALSE)} inside a mock Shiny session,
#' lets all its observers settle (the Shiny reactive graph orders the default, reset and max
#' observers by their dependencies), and checks that the settled state is a fixed point,
#' i.e. flushing again changes nothing and no input has a pending event. The snapshot is
#' stored in a process-wide cache under a key made of the spec, the contents of the scripts,
#' the names of \code{listObjects}, the values of those of its elements that are plain data (NULL,
#' atomic vectors, lists and data frames of those, if non-empty - not functions, environments, S4 objects,
#' external pointers or reactivevalues, which differ between sessions and processes and are
#' supplied live to every state by \code{InitState}) and the versions of MMVshiny, shiny and
#' SafeShiny. If a snapshot with that key already exists it is returned without a rebuild.
#'
#' Snapshots are also stored on disk, one RDS file per key, so that a new R process (restart of a
#' Shiny Server app process or of the server) loads the snapshot instead of rebuilding it; a
#' deployment that changes the spec, scripts, \code{listObjects} values or package versions
#' changes the key and so builds a new snapshot. The directory is the option
#' \code{MMVshiny.snapshotCacheDir}; by default a folder \code{.MMVshiny-cache} in the working
#' directory, which Shiny sets to the deployed app's home folder, so every deployed copy of an app
#' has its own cache and a new deployment starts with an empty one (the folder contains a
#' \code{.gitignore} ignoring its content). Set the option to \code{FALSE} to disable the disk
#' cache. Files unused for \code{getOption("MMVshiny.snapshotMaxAgeDays", 30)} days are removed
#' when a snapshot is written. Corrupt files and unwritable directories are ignored.
#'
#' Call this once at process start (e.g. at the top level of \code{app.R}) to move the cost
#' out of the first session; \code{InitState(seed = TRUE)} calls it lazily otherwise.
#'
#' @param spec a data.table denoting the parameter specification, as passed to \code{\link{InitState}}.
#' @param listObjects a named list of objects, as passed to \code{\link{InitState}}.
#' @param scriptsToSource a character vector of script paths, as passed to \code{\link{InitState}}.
#' @param keyExtra any R object included in the cache key, e.g. file modification times of
#' data files read by \code{scriptsToSource}, so that changing them invalidates the snapshot.
#' @param maxFlush integer; maximum number of reactive flushes to wait for the state to settle.
#'
#' @return a \code{MMVshinyStateSnapshot} (see \code{\link{ExtractStateSnapshot}}), or \code{NULL}
#' with a warning if the state does not reach a fixed point within \code{maxFlush} flushes
#' or its construction fails - callers then fall back to the live path.
#'
#' @seealso \code{\link{InitState}}, \code{\link{ClearStateSnapshotCache}}
#' @export
BuildStateSnapshot <- function(spec, listObjects = NULL, scriptsToSource = NULL, keyExtra = NULL,
                               maxFlush = 10L) {
  spec <- as.data.table(spec)
  key <- StateSnapshotKey(spec, listObjects, scriptsToSource, keyExtra)
  if(!is.null(snapshotCache[[key]])) {
    return(snapshotCache[[key]])
  }
  fromDisk <- ReadSnapshotFromDisk(key)
  if(!is.null(fromDisk)) {
    snapshotCache[[key]] <- fromDisk
    return(fromDisk)
  }

  snapshot <- tryCatch({
    out <- new.env()
    # A mock session gives the observers a reactive domain to be flushed in; testServer() is
    # avoided since it cannot be nested in tests which themselves use it.
    session <- shiny::MockShinySession$new()
    on.exit(try(session$close(), silent = TRUE), add = TRUE)
    state <- shiny::withReactiveDomain(session, {
      InitState(spec, stateId = "snapshotBuild", listObjects = listObjects,
                scriptsToSource = scriptsToSource, seed = FALSE)
    })
    previous <- NULL
    for(i in seq_len(maxFlush)) {
      session$flushReact()
      current <- ExtractStateSnapshot(state)
      if(identical(current, previous)) {
        pending <- vapply(current$event, function(e) !is.na(e), logical(1))
        if(any(pending)) {
          stop("inputs with a pending event after settling: ", paste(names(pending)[pending], collapse = ", "))
        }
        out$result <- current
        break
      }
      previous <- current
    }
    if(is.null(out$result)) {
      stop("the state did not reach a fixed point within ", maxFlush, " flushes")
    }
    out$result
  }, error = function(e) {
    warning("BuildStateSnapshot: ", conditionMessage(e), "; falling back to the live initial state.",
            call. = FALSE)
    NULL
  })

  if(!is.null(snapshot)) {
    snapshotCache[[key]] <- snapshot
    WriteSnapshotToDisk(key, snapshot)
  }
  snapshot
}

#' Remove all cached state snapshots
#' @param disk logical; also delete the snapshot files of the on-disk cache (default \code{TRUE}).
#' @return nothing - called for its side effect.
#' @examples
#' ClearStateSnapshotCache()
#' @export
ClearStateSnapshotCache <- function(disk = TRUE) {
  rm(list = ls(snapshotCache, all.names = TRUE), envir = snapshotCache)
  dir <- SnapshotCacheDir()
  if(disk && !is.null(dir) && dir.exists(dir)) {
    unlink(list.files(dir, pattern = "^snapshot_.*\\.(rds|tmp)$", full.names = TRUE))
  }
  invisible(NULL)
}
