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
#' @param ... Pacing and styling arguments forwarded to [tape_options()].
#'   Pass any of `width`, `height`, `font_size`, `theme`, `typing_speed`,
#'   `playback_speed`, `line_pause`, `start_pause`, `end_pause`,
#'   `paragraph_pause`, `typing_speed_jitter` by name. Unknown names error.
#' @param backend Which interactive R front-end to record.
#'   `"auto"` (the default) prefers `"arf"` (a Rust-based modern R
#'   console with tree-sitter syntax highlighting and zero runtime
#'   dependencies — see <https://github.com/eitsupi/arf>), falls back to
#'   `"radian"` if found, otherwise plain `"R"`.
#' @param tape Optional. A raw `.tape` script as a string (or path to a
#'   `.tape` file). When supplied, `expr` and the styling args are ignored
#'   and the tape is run verbatim.
#' @param quiet If `TRUE`, suppress vhs stdout/stderr.
#' @return Invisibly returns the path to the output file.
#' @seealso [tape_options()] for the full pacing/styling argument list.
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
  ...,
  backend = c("auto", "arf", "radian", "R"),
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
    lines = lines,
    output = output,
    opts = tape_options(...),
    backend = backend,
    quiet = quiet
  )
}

# Internal helper. Runs the post-substitute() portion of the recording
# pipeline so the knitr engine and any future record_demo_*() entry points
# share one path through vhsr_check / resolve_backend / build_tape /
# vhsr_run_tape.
record_demo_lines <- function(
  lines,
  output = "demo.gif",
  opts = tape_options(),
  backend = c("auto", "arf", "radian", "R"),
  quiet = FALSE,
  screenshots = NULL
) {
  assert_vhs_available()

  chosen <- resolve_backend(backend)

  tape_str <- do.call(
    build_tape,
    c(
      list(
        r_lines = lines,
        output = output,
        backend = chosen,
        screenshots = screenshots
      ),
      opts
    )
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
  ...,
  backend = c("auto", "arf", "radian", "R"),
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
    lines = lines,
    output = output,
    opts = tape_options(...),
    backend = backend,
    quiet = quiet
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
  ...,
  backend = c("auto", "arf", "radian", "R"),
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
  after_line <- parse_at_arg(at, length(lines))

  throwaway_gif <- withr::local_tempfile(fileext = ".gif")
  record_demo_lines(
    lines = lines,
    output = throwaway_gif,
    opts = tape_options(...),
    backend = backend,
    quiet = quiet,
    screenshots = list(list(after_line = after_line, path = output))
  )
  invisible(output)
}

parse_at_arg <- function(at, n) {
  line <- if (is.null(at)) {
    n
  } else if (is.character(at) && length(at) == 1L && grepl("^after:\\d+$", at)) {
    as.integer(sub("^after:", "", at))
  } else {
    cli::cli_abort(
      "{.arg at} must be {.code NULL} or a string of the form {.val after:N}."
    )
  }
  if (line < 1L || line > n) {
    cli::cli_abort(
      "{.arg at} = {.val {at}} is out of range; the expression has {n} line{?s}."
    )
  }
  line
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
