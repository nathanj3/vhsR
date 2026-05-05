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
#' @param start_pause Delay between R startup and the first typed line
#'   (e.g. `"800ms"`).
#' @param end_pause Delay between the last typed line and the (hidden)
#'   `q()` exit (e.g. `"500ms"`).
#' @param paragraph_pause If set, blank lines in `expr` produce a sleep
#'   of this duration instead of being typed.
#' @param typing_speed_jitter Numeric in `[0, 1]`; when positive, each
#'   typed line uses a speed sampled uniformly within `±jitter` of
#'   `typing_speed`. Set [base::set.seed()] before calling for
#'   reproducible jitter.
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
  start_pause = "800ms",
  end_pause = "500ms",
  paragraph_pause = NULL,
  typing_speed_jitter = 0,
  tape = NULL,
  quiet = FALSE
) {
  if (!is.null(tape)) {
    assert_vhs_available()
    vhsr_run_tape(paste(tape, collapse = "\n"), quiet = quiet)
    return(invisible(output))
  }

  e <- substitute(expr)
  lines <- deparse_block(e)

  record_demo_lines(
    lines               = lines,
    output              = output,
    width               = width,
    height              = height,
    font_size           = font_size,
    theme               = theme,
    typing_speed        = typing_speed,
    playback_speed      = playback_speed,
    line_pause          = line_pause,
    backend             = backend,
    start_pause         = start_pause,
    end_pause           = end_pause,
    paragraph_pause     = paragraph_pause,
    typing_speed_jitter = typing_speed_jitter,
    quiet               = quiet
  )
}

# Internal helper. Runs the post-substitute() portion of the recording
# pipeline so the knitr engine and any future record_demo_*() entry points
# share one path through vhsr_check / resolve_backend / build_tape /
# vhsr_run_tape.
record_demo_lines <- function(
  lines,
  output = "demo.gif",
  width = 1200,
  height = 600,
  font_size = 22,
  theme = "Catppuccin Mocha",
  typing_speed = "60ms",
  playback_speed = 1,
  line_pause = "500ms",
  backend = c("auto", "arf", "radian", "R"),
  quiet = FALSE,
  screenshots = NULL,
  start_pause = "800ms",
  end_pause = "500ms",
  paragraph_pause = NULL,
  typing_speed_jitter = 0
) {
  assert_vhs_available()

  chosen <- resolve_backend(backend)

  rprofile <- NULL
  if (chosen == "R" && has_colorout()) {
    rprofile <- tempfile(fileext = ".R")
    writeLines("suppressMessages(library(colorout))", rprofile)
    on.exit(unlink(rprofile), add = TRUE)
  }

  tape_str <- build_tape(
    r_lines             = lines,
    output              = output,
    width               = width,
    height              = height,
    font_size           = font_size,
    theme               = theme,
    typing_speed        = typing_speed,
    playback_speed      = playback_speed,
    line_pause          = line_pause,
    backend             = chosen,
    rprofile            = rprofile,
    screenshots         = screenshots,
    start_pause         = start_pause,
    end_pause           = end_pause,
    paragraph_pause     = paragraph_pause,
    typing_speed_jitter = typing_speed_jitter
  )

  vhsr_run_tape(tape_str, quiet = quiet)
  invisible(output)
}

#' Record a GIF demo from a script file
#'
#' Reads `path` as R source, validates it parses, and records each line
#' typed into the recorded REPL. Equivalent to inlining the file
#' contents in [record_demo()] but accepts a path instead of an
#' expression.
#'
#' @param path Path to an R script file.
#' @inheritParams record_demo
#' @return Invisibly returns `output`.
#' @export
#' @examples
#' \dontrun{
#' record_demo_file("script.R", output = "demo.gif")
#' }
record_demo_file <- function(
  path,
  output = "demo.gif",
  width = 1200,
  height = 600,
  font_size = 22,
  theme = "Catppuccin Mocha",
  typing_speed = "60ms",
  playback_speed = 1,
  line_pause = "500ms",
  backend = c("auto", "arf", "radian", "R"),
  start_pause = "800ms",
  end_pause = "500ms",
  paragraph_pause = NULL,
  typing_speed_jitter = 0,
  quiet = FALSE
) {
  if (!file.exists(path)) {
    cli::cli_abort("Script file not found: {.path {path}}")
  }

  lines <- readLines(path, warn = FALSE)

  tryCatch(
    parse(text = lines),
    error = function(e) {
      cli::cli_abort(c(
        "Syntax error in {.path {path}}",
        i = conditionMessage(e)
      ))
    }
  )

  record_demo_lines(
    lines               = lines,
    output              = output,
    width               = width,
    height              = height,
    font_size           = font_size,
    theme               = theme,
    typing_speed        = typing_speed,
    playback_speed      = playback_speed,
    line_pause          = line_pause,
    backend             = backend,
    start_pause         = start_pause,
    end_pause           = end_pause,
    paragraph_pause     = paragraph_pause,
    typing_speed_jitter = typing_speed_jitter,
    quiet               = quiet
  )
}

#' Capture a single PNG frame from a recorded R session
#'
#' Records the same R expression that [record_demo()] would, but
#' instead of (or in addition to) the animated output, writes a single
#' PNG frame using vhs's `Screenshot` directive.
#'
#' @param expr An R expression. See [record_demo()].
#' @param output Path to the output PNG. Must end in `.png`.
#' @param at Where to take the screenshot. `NULL` (default) → after the
#'   last typed line. A string `"after:N"` (1-indexed) → after the Nth
#'   line.
#' @inheritParams record_demo
#' @return Invisibly returns `output`.
#' @export
#' @examples
#' \dontrun{
#' record_demo_screenshot(
#'   { x <- 1:5; mean(x) },
#'   output = "demo.png"
#' )
#' }
record_demo_screenshot <- function(
  expr,
  output = "demo.png",
  at = NULL,
  width = 1200,
  height = 600,
  font_size = 22,
  theme = "Catppuccin Mocha",
  typing_speed = "60ms",
  playback_speed = 1,
  line_pause = "500ms",
  backend = c("auto", "arf", "radian", "R"),
  start_pause = "800ms",
  end_pause = "500ms",
  paragraph_pause = NULL,
  typing_speed_jitter = 0,
  quiet = FALSE
) {
  if (!grepl("\\.png$", output, ignore.case = TRUE)) {
    cli::cli_abort(c(
      "{.arg output} for a screenshot must end in {.val .png}.",
      x = "Got: {.path {output}}"
    ))
  }

  e <- substitute(expr)
  lines <- deparse_block(e)
  n <- length(lines)

  after_line <- if (is.null(at)) {
    n
  } else if (is.character(at) && length(at) == 1L && grepl("^after:\\d+$", at)) {
    as.integer(sub("^after:", "", at))
  } else {
    cli::cli_abort(
      "{.arg at} must be {.code NULL} or a string of the form {.val after:N}."
    )
  }
  if (after_line < 1L || after_line > n) {
    cli::cli_abort(
      "{.arg at} = {.val {at}} is out of range; the expression has {n} line{?s}."
    )
  }

  throwaway_gif <- withr::local_tempfile(fileext = ".gif")
  record_demo_lines(
    lines               = lines,
    output              = throwaway_gif,
    width               = width,
    height              = height,
    font_size           = font_size,
    theme               = theme,
    typing_speed        = typing_speed,
    playback_speed      = playback_speed,
    line_pause          = line_pause,
    backend             = backend,
    quiet               = quiet,
    screenshots         = list(list(after_line = after_line, path = output)),
    start_pause         = start_pause,
    end_pause           = end_pause,
    paragraph_pause     = paragraph_pause,
    typing_speed_jitter = typing_speed_jitter
  )
  invisible(output)
}

assert_vhs_available <- function() {
  if (!vhsr_check()) {
    cli::cli_abort(c(
      "Required binaries not found.",
      i = "Run {.run vhsr_doctor()} to see what's missing.",
      i = "Run {.run vhsr_install()} to install them."
    ))
  }
}
