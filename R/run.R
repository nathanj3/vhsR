#' Run a hand-written tape script
#'
#' Escape hatch for users who want full control over the vhs script. Pass
#' either a path to a `.tape` file or the tape contents as a string.
#'
#' @param tape Path to a `.tape` file, or a string containing tape syntax.
#' @param quiet If `TRUE`, suppress vhs stdout/stderr.
#' @return Invisibly returns the [processx::run()] result.
#' @export
vhsr_run_tape <- function(tape, quiet = FALSE) {
  vhs <- vhsr_vhs_path()
  if (!nzchar(vhs)) {
    cli::cli_abort(c(
      "{.code vhs} binary not found.",
      i = "Run {.run vhsr_install()} to install it."
    ))
  }

  looks_like_path <- length(tape) == 1L && !grepl("\n", tape, fixed = TRUE)
  if (looks_like_path) {
    if (!file.exists(tape)) {
      cli::cli_abort(c(
        "Tape file not found: {.path {tape}}",
        i = "Pass a multi-line string for inline tape content."
      ))
    }
    tape_path <- tape
  } else {
    tape_path <- withr::local_tempfile(fileext = ".tape")
    writeLines(tape, tape_path)
  }

  # vhs uses go-rod under the hood, which leaves a `rod/` directory
  # under tempdir() after each browser session. Clean it up so
  # `R CMD check` doesn't flag it as detritus.
  on.exit(
    unlink(file.path(tempdir(), "rod"), recursive = TRUE, force = TRUE),
    add = TRUE
  )

  result <- processx::run(
    vhs,
    c(tape_path),
    echo = !quiet,
    env = c("current", PATH = augmented_path()),
    error_on_status = FALSE
  )

  if (result$status != 0L) {
    cli::cli_abort(c(
      "vhs exited with status {result$status}.",
      i = result$stderr
    ))
  }

  invisible(result)
}
