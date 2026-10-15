library(testthat)
library(shiny)
library(data.table)

# Spec with the dependency shapes of the real use case:
# - B: default depends on A (leaf -> dependent chain), C: max depends on B
# - X <-> Y: mutually guarded defaults, like BPratio/BP_R
make_chain_spec <- function() {
  mk <- function(id, val = "1", max = "100", dec = "2") {
    data.table(
      ID = id, TYPE = "numeric input", GUILABEL = "Label", STATUSICON = TRUE,
      VALEXPR = val, DEFAULTVALUENOTE = "NA", MIN = "0", MINNOTE = "NA",
      MAX = max, MAXNOTE = "NA", DECDIGITS = dec, OUTOFBOUNDS = "nearest",
      SCFILTER = NA_character_, SCVALUE = NA_character_,
      ExprVAL = val, ExprDEFNOTE = "NA_character_", ExprMIN = "0",
      ExprMINNOTE = "NA_character_", ExprMAX = max, ExprMAXNOTE = "NA_character_")
  }
  rbind(mk("A", "2.345"), mk("B"), mk("C", max = "50"), mk("X"), mk("Y"),
        mk("Z", "7"))
}

make_state_scripts <- function(spec) {
  extra <- tempfile(fileext = ".R")
  writeLines(c(
    'MYSTATE$default[["B"]] <- shiny::reactive(Get(MYSTATE, "A") * 3.14159)',
    'MYSTATE$max[["C"]] <- shiny::reactive(Get(MYSTATE, "B") / 2)',
    'MYSTATE$default[["X"]] <- shiny::reactive(if(GetSource(MYSTATE, "Y") == "Default value") 1.5 else Get(MYSTATE, "Y"))',
    'MYSTATE$default[["Y"]] <- shiny::reactive(if(GetSource(MYSTATE, "X") == "Default value") 1.5 else Get(MYSTATE, "X"))'
  ), extra)
  reset <- GenerateScriptCreatingObservers(spec, stateObjectName = "MYSTATE", observerTypes = "resetCount",
                                           seedable = TRUE, filename = tempfile(fileext = ".R"))
  def <- GenerateScriptCreatingObservers(spec, stateObjectName = "MYSTATE", observerTypes = "default",
                                         ids = c("B", "X", "Y"), seedable = TRUE, filename = tempfile(fileext = ".R"))
  max <- GenerateScriptCreatingObservers(spec, stateObjectName = "MYSTATE", observerTypes = "max",
                                         ids = "C", seedable = TRUE, filename = tempfile(fileext = ".R"))
  c(extra, reset, def, max)
}

test_that("seeded initial state equals the live one for all ids, including chains", {
  spec <- make_chain_spec()
  scripts <- make_state_scripts(spec)
  ClearStateSnapshotCache()
  testServer(function(input, output, session) {
    live <- InitState(spec, stateId = "live", scriptsToSource = scripts, seed = FALSE)
    seeded <- InitState(spec, stateId = "seeded", scriptsToSource = scripts, seed = TRUE)
  }, {
    session$flushReact()
    expect_false(live$seeded)
    expect_true(seeded$seeded)
    sLive <- ExtractStateSnapshot(live)
    sSeeded <- ExtractStateSnapshot(seeded)
    expect_identical(sSeeded, sLive)
    # chains actually resolved to non-initial values
    expect_equal(as.numeric(sLive$displayed$B), 7.37)
    expect_equal(sLive$max$C, as.numeric(sLive$validated$B) / 2)
    expect_equal(sLive$status$X$source, "Default value")
    expect_equal(sLive$status$Y$source, "Default value")
    expect_equal(as.numeric(sLive$validated$X), 1.5)
    expect_equal(sLive$statusIconClass$A, "glyphicon glyphicon-info-sign")
  })
})

test_that("seeded observers skip the first run but still react to later changes", {
  spec <- make_chain_spec()
  scripts <- make_state_scripts(spec)
  ClearStateSnapshotCache()
  n <- 0L
  origValidate <- Validate
  testthat::local_mocked_bindings(Validate = function(...) { n <<- n + 1L; origValidate(...) }, .package = "MMVshiny")
  testServer(function(input, output, session) {
    live <- InitState(spec, stateId = "live", scriptsToSource = scripts, seed = FALSE)
  }, {
    session$flushReact()
    nLive <- n
    expect_gt(nLive, 0L)
  })
  nBefore <- n
  testServer(function(input, output, session) {
    seeded <- InitState(spec, stateId = "seeded", scriptsToSource = scripts, seed = TRUE)
  }, {
    nAfterSeed <- n   # includes the one-off snapshot build
    session$flushReact()
    expect_equal(n, nAfterSeed)  # no Validate() calls from the seeded observers
    # Change A as a user would; default of B, then max of C, must propagate
    ProcessGuiInputEvent(seeded, input = NULL, id = "A", inputValue = 10)
    session$flushReact()
    expect_equal(as.numeric(GetDisplayed(seeded, "B")), round(10 * 3.14159, 2))
    expect_equal(GetMax(seeded, "C"), as.numeric(Get(seeded, "B")) / 2)
    expect_equal(GetStatus(seeded, "A")$source, "User input")
  })
})

test_that("a user override of X moves Y off its guarded default in seeded and live states alike", {
  spec <- make_chain_spec()
  scripts <- make_state_scripts(spec)
  ClearStateSnapshotCache()
  testServer(function(input, output, session) {
    live <- InitState(spec, stateId = "live", scriptsToSource = scripts, seed = FALSE)
    seeded <- InitState(spec, stateId = "seeded", scriptsToSource = scripts, seed = TRUE)
  }, {
    session$flushReact()
    for(s in list(live, seeded)) {
      ProcessGuiInputEvent(s, input = NULL, id = "X", inputValue = 3)
    }
    session$flushReact()
    expect_identical(ExtractStateSnapshot(seeded), ExtractStateSnapshot(live))
    expect_equal(as.numeric(Get(seeded, "Y")), 3)
  })
})

test_that("snapshot is cached by key and invalidated when the spec or scripts change", {
  spec <- make_chain_spec()
  scripts <- make_state_scripts(spec)
  ClearStateSnapshotCache()
  s1 <- BuildStateSnapshot(spec, scriptsToSource = scripts)
  expect_s3_class(s1, "MMVshinyStateSnapshot")
  expect_identical(BuildStateSnapshot(spec, scriptsToSource = scripts), s1)
  expect_length(ls(MMVshiny:::snapshotCache), 1L)

  spec2 <- data.table::copy(spec)
  spec2[ID == "A", `:=`(VALEXPR = "9", ExprVAL = "9")]
  s2 <- BuildStateSnapshot(spec2, scriptsToSource = scripts)
  expect_length(ls(MMVshiny:::snapshotCache), 2L)
  expect_equal(as.numeric(s2$validated$A), 9)
  expect_equal(as.numeric(s1$validated$A), 2.345)

  writeLines("# changed", scripts[1])
  expect_false(identical(MMVshiny:::StateSnapshotKey(spec, scriptsToSource = scripts),
                         MMVshiny:::StateSnapshotKey(spec2, scriptsToSource = scripts)))
})

test_that("a failing build warns and InitState(seed = TRUE) falls back to the live path", {
  spec <- make_chain_spec()
  bad <- tempfile(fileext = ".R")
  writeLines("stop('boom')", bad)
  ClearStateSnapshotCache()
  expect_warning(snap <- BuildStateSnapshot(spec, scriptsToSource = bad), "falling back")
  expect_null(snap)
  expect_error(suppressWarnings(InitState(spec, stateId = "bad", scriptsToSource = bad, seed = TRUE)), "boom")

  ok <- InitState(spec, stateId = "ok", scriptsToSource = make_state_scripts(spec), seed = TRUE)
  expect_true(ok$seeded)
  expect_false(InitState(spec, stateId = "ok2", seed = FALSE)$seeded)
})

test_that("a change made between InitState() and the first flush is not lost by the seeded first-run skip", {
  # Regression test for #24: e.g. the MMVSola upload sets Science Cloud data synchronously in the
  # same observer that creates the state; the observers' deferred first run must then still run.
  spec <- make_chain_spec()
  scripts <- make_state_scripts(spec)
  ClearStateSnapshotCache()
  testServer(function(input, output, session) {
    mk <- function(seed) {
      s <- InitState(spec, stateId = paste0("s", seed), scriptsToSource = scripts, seed = seed)
      ProcessGuiInputEvent(s, input = NULL, id = "A", inputValue = 10)
      s
    }
    live <- mk(FALSE)
    seeded <- mk(TRUE)
  }, {
    session$flushReact()
    expect_true(seeded$seeded)
    expect_identical(ExtractStateSnapshot(seeded), ExtractStateSnapshot(live))
    expect_equal(as.numeric(GetDisplayed(seeded, "B")), round(10 * 3.14159, 2))
  })
})

test_that("a report-style change of the validated value (default unchanged) is repaired by the first run", {
  # Regression test for #24 (Cmp3 report in MMVSola): REPORT sets validated = NA with source
  # "Default value"; the default observer's first run restores the default.
  spec <- make_chain_spec()
  scripts <- make_state_scripts(spec)
  ClearStateSnapshotCache()
  testServer(function(input, output, session) {
    mk <- function(seed) {
      s <- InitState(spec, stateId = paste0("r", seed), scriptsToSource = scripts, seed = seed)
      SetStatus(s, "B", reportSource = "Default value")
      SetEvent(s, "B", "REPORT")
      SetReportInput(s, "B", NA_real_, flagSynchronous = TRUE)
      s
    }
    live <- mk(FALSE)
    seeded <- mk(TRUE)
  }, {
    session$flushReact()
    # status fields may be added in a different order (reportSource vs validationNote)
    norm <- function(s) { s$status <- lapply(s$status, function(x) x[order(names(x))]); s }
    expect_identical(norm(ExtractStateSnapshot(seeded)), norm(ExtractStateSnapshot(live)))
    expect_equal(as.numeric(GetDisplayed(seeded, "B")), round(2.345 * 3.14159, 2))
  })
})

test_that("SkipSeededRun skips only the first run, and only if the value is unchanged", {
  st <- new.env()
  expect_false(SkipSeededRun(st, "default", "A", 1))  # unseeded
  st$seeded <- TRUE
  st$seedConsumed <- new.env()
  st$seedSnapshot <- list(default = list(A = 1, B = 2),
                          validated = list(A = 1, B = 2), status = list(A = list(source = "Default value"),
                                                                           B = list(source = "Default value")),
                          event = list(A = NA_character_, B = NA_character_))
  st$validated <- list(A = 1, B = 2)
  st$status <- list(A = list(source = "Default value"), B = list(source = "Default value"))
  st$event <- list(A = NA_character_, B = NA_character_)
  expect_true(SkipSeededRun(st, "default", "A", 1))
  expect_false(SkipSeededRun(st, "default", "A", 1))  # consumed
  expect_false(SkipSeededRun(st, "default", "B", 3))  # changed
  expect_false(SkipSeededRun(st, "default", "B", 2))  # consumed by the previous call
  st$seedConsumed <- new.env()
  st$validated$A <- NA_real_                           # validated value changed since seeding
  expect_false(SkipSeededRun(st, "default", "A", 1))
})

test_that("seedable = FALSE leaves generated observers unchanged, seedable = TRUE adds the SkipSeededRun guard", {
  spec <- make_chain_spec()
  for(safe in c(FALSE, TRUE)) {
    skip_if(safe && !requireNamespace("SafeShiny", quietly = TRUE))
    types <- c("resetCount", "default", "min", "max")
    f0 <- GenerateScriptCreatingObservers(spec, ids = "A", observerTypes = types,
                                          useSafeShiny = safe, filename = tempfile(fileext = ".R"))
    f1 <- GenerateScriptCreatingObservers(spec, ids = "A", observerTypes = types,
                                          useSafeShiny = safe, seedable = TRUE, filename = tempfile(fileext = ".R"))
    code0 <- paste(readLines(f0), collapse = "\n")
    code1 <- paste(readLines(f1), collapse = "\n")
    expect_false(grepl("Seeded", code0, fixed = TRUE))
    expect_equal(lengths(regmatches(code1, gregexpr("SkipSeededRun(state,", code1, fixed = TRUE))), 3L)
    expect_false(grepl("ignoreInit = isTRUE", code1, fixed = TRUE))
    expect_no_error(parse(text = code1))
  }
})

test_that("the snapshot key ignores session- and process-specific listObjects but not plain ones", {
  spec <- make_chain_spec()
  scripts <- make_state_scripts(spec)
  key <- function(listObjects) MMVshiny:::StateSnapshotKey(spec, listObjects = listObjects, scriptsToSource = scripts)

  # Objects whose hash differs between sessions/processes: only their names enter the key.
  input1 <- shiny::MockShinySession$new()$input
  input2 <- shiny::MockShinySession$new()$input
  expect_false(identical(digest::digest(input1), digest::digest(input2)))
  expect_identical(key(list(input = input1, parser = function() 1, env = new.env())),
                   key(list(input = input2, parser = NULL, env = list())))
  expect_identical(key(list(input = input1)), key(list(input = list())))
  setClass("MMVshinyKeyTestS4", representation(x = "numeric"))
  expect_identical(key(list(obj = new("MMVshinyKeyTestS4", x = 1))), key(list(obj = NULL)))
  removeClass("MMVshinyKeyTestS4")

  # Names still matter, and so do plain values (also nested in lists and data frames).
  expect_false(identical(key(list(input = input1)), key(list(other = input1))))
  expect_false(identical(key(list(prefixGuiId = "")), key(list(prefixGuiId = "a_"))))
  expect_false(identical(key(list(x = list(a = 1))), key(list(x = list(a = 2)))))
  expect_false(identical(key(list(df = data.frame(a = 1))), key(list(df = data.frame(a = 2)))))
  expect_identical(key(list(prefixGuiId = "")), key(list(prefixGuiId = "")))

  # A snapshot built with surrogates is found by InitState(seed = TRUE) with the live objects.
  ClearStateSnapshotCache()
  s1 <- BuildStateSnapshot(spec, listObjects = list(input = list()), scriptsToSource = scripts)
  expect_s3_class(s1, "MMVshinyStateSnapshot")
  session <- shiny::MockShinySession$new()
  state <- shiny::withReactiveDomain(session, InitState(spec, stateId = "keyTest",
                                                        listObjects = list(input = input1),
                                                        scriptsToSource = scripts, seed = TRUE))
  expect_true(state$seeded)
  expect_length(ls(MMVshiny:::snapshotCache), 1L)
})

test_that("IsPlainData classifies values", {
  expect_true(all(vapply(list(NULL, 1, "a", c(a = 1), factor("a"), list(1, list("a")), data.frame(a = 1)),
                         MMVshiny:::IsPlainData, logical(1))))
  expect_false(any(vapply(list(sum, new.env(), quote(x), shiny::reactiveValues(a = 1), list(1, new.env()),
                               structure(list(1), class = "foo")),
                          MMVshiny:::IsPlainData, logical(1))))
})

test_that("snapshots are stored on disk and loaded by a fresh process (empty memory cache)", {
  dir <- file.path(tempdir(), "diskCacheTest")
  old <- options(MMVshiny.snapshotCacheDir = dir)
  on.exit(options(old))
  spec <- make_chain_spec()
  scripts <- make_state_scripts(spec)
  ClearStateSnapshotCache()
  expect_length(list.files(dir, pattern = "\\.rds$"), 0L)

  s1 <- BuildStateSnapshot(spec, scriptsToSource = scripts)
  files <- list.files(dir, pattern = "^snapshot_.*\\.rds$", full.names = TRUE)
  expect_length(files, 1L)
  expect_length(list.files(dir, pattern = "\\.tmp$"), 0L)

  # New "process": memory cache empty, the disk file is used. Tag the file to prove that the
  # returned snapshot was read from it rather than rebuilt.
  ClearStateSnapshotCache(disk = FALSE)
  expect_length(ls(MMVshiny:::snapshotCache), 0L)
  tagged <- s1
  tagged$validated$A <- 99
  saveRDS(tagged, files)
  s2 <- BuildStateSnapshot(spec, scriptsToSource = scripts)
  expect_equal(as.numeric(s2$validated$A), 99)
  expect_length(ls(MMVshiny:::snapshotCache), 1L)

  # An unmodified file round-trips to an identical snapshot and seeds states like a live build.
  saveRDS(s1, files)
  ClearStateSnapshotCache(disk = FALSE)
  s3 <- BuildStateSnapshot(spec, scriptsToSource = scripts)
  expect_identical(s3, s1)
  session <- shiny::MockShinySession$new()
  state <- shiny::withReactiveDomain(session, InitState(spec, stateId = "diskTest",
                                                        scriptsToSource = scripts, seed = TRUE))
  expect_true(state$seeded)

  # A changed spec (a new deployment) selects another file; the old one stays until it ages out.
  spec2 <- data.table::copy(spec)
  spec2[ID == "A", `:=`(VALEXPR = "9", ExprVAL = "9")]
  s4 <- BuildStateSnapshot(spec2, scriptsToSource = scripts)
  expect_equal(as.numeric(s4$validated$A), 9)
  expect_length(list.files(dir, pattern = "\\.rds$"), 2L)

  # Clearing removes the files too.
  ClearStateSnapshotCache()
  expect_length(list.files(dir, pattern = "\\.rds$"), 0L)
})

test_that("a corrupt or foreign cache file is ignored and replaced", {
  dir <- file.path(tempdir(), "diskCacheCorrupt")
  old <- options(MMVshiny.snapshotCacheDir = dir)
  on.exit(options(old))
  spec <- make_chain_spec()
  scripts <- make_state_scripts(spec)
  ClearStateSnapshotCache()
  s1 <- BuildStateSnapshot(spec, scriptsToSource = scripts)
  file <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)

  for(bad in list("not an rds file", list(validated = 1))) {
    ClearStateSnapshotCache(disk = FALSE)
    if(is.character(bad)) writeLines(bad, file) else saveRDS(bad, file)
    expect_no_warning(s2 <- BuildStateSnapshot(spec, scriptsToSource = scripts))
    expect_identical(s2, s1)
    expect_s3_class(readRDS(file), "MMVshinyStateSnapshot")
  }
})

test_that("the disk cache can be disabled, and an unwritable directory is harmless", {
  spec <- make_chain_spec()
  scripts <- make_state_scripts(spec)
  dir <- file.path(tempdir(), "diskCacheOff")
  old <- options(MMVshiny.snapshotCacheDir = FALSE)
  on.exit(options(old))
  expect_null(MMVshiny:::SnapshotCacheDir())
  wd <- file.path(tempdir(), "diskCacheApp")
  dir.create(wd, showWarnings = FALSE)
  owd <- setwd(wd)
  on.exit(setwd(owd), add = TRUE)
  ClearStateSnapshotCache()
  expect_s3_class(BuildStateSnapshot(spec, scriptsToSource = scripts), "MMVshinyStateSnapshot")
  expect_length(list.files(wd, all.files = TRUE, no.. = TRUE), 0L)

  # Unset: the default is a folder in the app's home (working) directory, with its own .gitignore.
  options(MMVshiny.snapshotCacheDir = NULL)
  expect_equal(normalizePath(MMVshiny:::SnapshotCacheDir()), normalizePath(file.path(wd, ".MMVshiny-cache"), mustWork = FALSE))
  ClearStateSnapshotCache()
  BuildStateSnapshot(spec, scriptsToSource = scripts)
  expect_length(list.files(file.path(wd, ".MMVshiny-cache"), pattern = "^snapshot_.*\\.rds$"), 1L)
  expect_equal(readLines(file.path(wd, ".MMVshiny-cache", ".gitignore")), "*")
  options(MMVshiny.snapshotCacheDir = FALSE)

  # The "directory" is a regular file: nothing can be written, the build still succeeds.
  blocker <- tempfile("blocker")
  writeLines("x", blocker)
  options(MMVshiny.snapshotCacheDir = file.path(blocker, "sub"))
  ClearStateSnapshotCache()
  expect_no_error(s <- BuildStateSnapshot(spec, scriptsToSource = scripts))
  expect_s3_class(s, "MMVshinyStateSnapshot")
})

test_that("files not used for MMVshiny.snapshotMaxAgeDays days are pruned when a snapshot is written", {
  dir <- file.path(tempdir(), "diskCachePrune")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  old <- options(MMVshiny.snapshotCacheDir = dir)
  on.exit(options(old))
  stale <- file.path(dir, "snapshot_stale.rds")
  recent <- file.path(dir, "snapshot_recent.rds")
  other <- file.path(dir, "other.txt")
  for(f in c(stale, recent, other)) writeLines("x", f)
  Sys.setFileTime(c(stale, other), Sys.time() - 40 * 24 * 3600)
  spec <- make_chain_spec()
  ClearStateSnapshotCache(disk = FALSE)
  BuildStateSnapshot(spec, scriptsToSource = make_state_scripts(spec))
  expect_false(file.exists(stale))
  expect_true(file.exists(recent))
  expect_true(file.exists(other))  # only snapshot files are touched
})
