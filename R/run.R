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

  if (length(tape) == 1L && file.exists(tape)) {
    tape_path <- tape
  } else {
    tape_path <- withr::local_tempfile(fileext = ".tape")
    writeLines(tape, tape_path)
  }

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
