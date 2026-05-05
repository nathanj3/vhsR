quote_for_tape <- function(line) {
  has_dq <- grepl('"', line, fixed = TRUE)
  has_bt <- grepl("`", line, fixed = TRUE)
  if (has_dq && has_bt) {
    cli::cli_abort(c(
      "Line contains both double quotes and backticks.",
      i = "vhs {.code Type} can't quote a string with both. Pass a hand-written tape via {.arg tape} to {.fn record_demo} to bypass this.",
      x = "Offending line: {.val {line}}"
    ))
  }
  if (has_dq) {
    paste0("`", line, "`")
  } else {
    paste0("\"", line, "\"")
  }
}

backend_command <- function(backend) {
  switch(
    backend,
    R = "R --quiet --no-save",
    radian = "radian --no-save",
    arf = "arf"
  )
}

escape_tape_string <- function(s) {
  gsub('"', '\\"', s, fixed = TRUE)
}

parse_duration_ms <- function(s) {
  m <- regmatches(s, regexec("^([0-9.]+)\\s*(ms|s)$", s))[[1]]
  if (length(m) != 3) {
    cli::cli_abort(c(
      "Cannot parse duration {.val {s}}.",
      i = "Expected something like {.val 60ms} or {.val 1s}."
    ))
  }
  as.numeric(m[2]) * if (m[3] == "s") 1000 else 1
}

#' Pacing and styling options for a vhsR recording
#'
#' Constructor for the option list passed to internal recording helpers.
#' Public entry points like [record_demo()] forward `...` here, so any
#' argument documented below can be passed by name to those functions.
#' Calling `tape_options()` directly with an unknown argument errors out,
#' which gives the public functions typo safety on pacing/styling args.
#'
#' @param width,height Terminal dimensions in pixels.
#' @param font_size Font size in pixels.
#' @param theme A vhs theme name.
#' @param typing_speed Per-keystroke delay, e.g. `"60ms"`.
#' @param playback_speed Playback rate multiplier (1 = real time).
#' @param line_pause Delay after each line of R code, e.g. `"500ms"`.
#' @param start_pause Delay between R startup and the first typed line.
#' @param end_pause Delay between the last typed line and the (hidden)
#'   `q()` exit.
#' @param paragraph_pause If set, blank lines produce a sleep of this
#'   duration instead of being typed.
#' @param typing_speed_jitter Numeric in `[0, 1]`; when positive, each
#'   typed line uses a speed sampled uniformly within `±jitter` of
#'   `typing_speed`.
#' @return A named list of options.
#' @keywords internal
tape_options <- function(
  width = 1200,
  height = 600,
  font_size = 22,
  theme = "Catppuccin Mocha",
  typing_speed = "60ms",
  playback_speed = 1,
  line_pause = "500ms",
  start_pause = "800ms",
  end_pause = "500ms",
  paragraph_pause = NULL,
  typing_speed_jitter = 0
) {
  list(
    width = width,
    height = height,
    font_size = font_size,
    theme = theme,
    typing_speed = typing_speed,
    playback_speed = playback_speed,
    line_pause = line_pause,
    start_pause = start_pause,
    end_pause = end_pause,
    paragraph_pause = paragraph_pause,
    typing_speed_jitter = typing_speed_jitter
  )
}

build_tape <- function(
  r_lines,
  output,
  ...,
  backend = "R",
  screenshots = NULL
) {
  if (length(r_lines) == 0L) {
    cli::cli_abort("No R code to record.")
  }
  opts <- tape_options(...)

  paste(
    c(
      tape_header(output, opts, backend_command(backend)),
      tape_body(r_lines, opts, parse_duration_ms(opts$typing_speed), screenshots),
      tape_footer(opts)
    ),
    collapse = "\n"
  )
}

tape_header <- function(output, opts, startup_cmd) {
  c(
    sprintf('Output "%s"', escape_tape_string(output)),
    'Set Shell "bash"',
    sprintf('Set FontSize %d', as.integer(opts$font_size)),
    sprintf('Set Width %d', as.integer(opts$width)),
    sprintf('Set Height %d', as.integer(opts$height)),
    sprintf('Set Theme "%s"', escape_tape_string(opts$theme)),
    sprintf('Set TypingSpeed %s', opts$typing_speed),
    sprintf('Set PlaybackSpeed %s', format(opts$playback_speed)),
    'Hide',
    sprintf('Type "%s"', startup_cmd),
    'Enter',
    sprintf('Sleep %s', opts$start_pause),
    'Ctrl+L',
    'Sleep 200ms',
    'Show'
  )
}

tape_body <- function(r_lines, opts, base_ms, screenshots) {
  unlist(lapply(seq_along(r_lines), function(i) {
    line <- r_lines[[i]]
    if (!nzchar(trimws(line)) && !is.null(opts$paragraph_pause)) {
      return(sprintf('Sleep %s', opts$paragraph_pause))
    }
    c(
      type_directive_for(line, base_ms, opts$typing_speed_jitter),
      "Enter",
      sprintf("Sleep %s", opts$line_pause),
      screenshots_after(i, screenshots)
    )
  }))
}

tape_footer <- function(opts) {
  c(
    'Hide',
    'Type "q()"',
    "Enter",
    sprintf("Sleep %s", opts$end_pause)
  )
}

type_directive_for <- function(line, base_ms, jitter) {
  if (jitter > 0) {
    sampled_ms <- max(1L, round(base_ms * stats::runif(1L, 1 - jitter, 1 + jitter)))
    sprintf("Type@%dms %s", as.integer(sampled_ms), quote_for_tape(line))
  } else {
    paste("Type", quote_for_tape(line))
  }
}

screenshots_after <- function(i, screenshots) {
  if (length(screenshots) == 0L) {
    return(character())
  }
  hits <- vapply(screenshots, function(sc) isTRUE(sc$after_line == i), logical(1))
  if (!any(hits)) {
    return(character())
  }
  vapply(
    screenshots[hits],
    function(sc) sprintf('Screenshot "%s"', escape_tape_string(sc$path)),
    character(1)
  )
}

deparse_block <- function(expr) {
  if (is.call(expr) && identical(expr[[1L]], as.name("{"))) {
    stmts <- as.list(expr)[-1L]
  } else {
    stmts <- list(expr)
  }
  vapply(
    stmts,
    function(x) paste(deparse(x, width.cutoff = 500L), collapse = " "),
    character(1)
  )
}
