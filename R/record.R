#' Record a GIF demo of an R expression
#'
#' Captures the unevaluated R expression `expr`, types it line-by-line into
#' a freshly-spawned R session inside a `vhs` recording, and writes the
#' result to `output`. The R front-end is chosen by `backend` (see below).
#' Setup is a one-liner: if the required binaries are missing, the
#' function points to [vhsr_install()].
#'
#' @param expr An R expression (typically a `{ ... }` block). Captured
#'   unevaluated; lines are typed verbatim into the REPL.
#' @param output Path to the output file. The extension determines the
#'   format (`.gif`, `.mp4`, `.webm`).
#' @param width,height Terminal dimensions in pixels.
#' @param font_size Font size in pixels.
#' @param theme A vhs theme name (see vhs docs for the full list).
#' @param typing_speed Per-keystroke delay, e.g. `"60ms"`.
#' @param playback_speed Playback rate multiplier (1 = real time).
#' @param line_pause Delay after each line of R code, e.g. `"500ms"`.
#' @param backend Which interactive R front-end to record.
#'   `"auto"` (the default) prefers `"arf"` (a Rust-based modern R
#'   console with tree-sitter syntax highlighting and zero runtime
#'   dependencies — see <https://github.com/eitsupi/arf>), falls back to
#'   `"radian"` if found, otherwise plain `"R"`. When the resolved
#'   backend is `"R"` and the `colorout` package is installed, it is
#'   loaded automatically so the recording has highlighted output.
#' @param tape Optional. A raw `.tape` script as a string (or path to a
#'   `.tape` file). When supplied, `expr` and the styling args are ignored
#'   and the tape is run verbatim.
#' @param quiet If `TRUE`, suppress vhs stdout/stderr.
#' @return Invisibly returns the path to the output file.
#' @export
#' @examples
#' \dontrun{
#' record_demo({
#'   x <- 1:5
#'   mean(x)
#'   head(cars, 3)
#' }, output = "demo.gif")
#' }
record_demo <- function(
  expr,
  output = "demo.gif",
  width = 1200,
  height = 600,
  font_size = 22,
  theme = "Catppuccin Mocha",
  typing_speed = "60ms",
  playback_speed = 1,
  line_pause = "500ms",
  backend = c("auto", "arf", "radian", "R"),
  tape = NULL,
  quiet = FALSE
) {
  if (!vhsr_check()) {
    cli::cli_abort(c(
      "Required binaries not found.",
      i = "Run {.run vhsr_doctor()} to see what's missing.",
      i = "Run {.run vhsr_install()} to install them."
    ))
  }

  if (is.null(tape)) {
    chosen <- resolve_backend(backend)

    rprofile <- NULL
    if (chosen == "R" && has_colorout()) {
      rprofile <- tempfile(fileext = ".R")
      writeLines(
        "suppressMessages(library(colorout))",
        rprofile
      )
      on.exit(unlink(rprofile), add = TRUE)
    }

    e <- substitute(expr)
    r_lines <- deparse_block(e)
    tape_str <- build_tape(
      r_lines,
      output = output,
      width = width,
      height = height,
      font_size = font_size,
      theme = theme,
      typing_speed = typing_speed,
      playback_speed = playback_speed,
      line_pause = line_pause,
      backend = chosen,
      rprofile = rprofile
    )
  } else {
    tape_str <- paste(tape, collapse = "\n")
  }

  vhsr_run_tape(tape_str, quiet = quiet)
  invisible(output)
}
