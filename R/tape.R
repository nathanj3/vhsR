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

build_tape <- function(
  r_lines,
  output,
  width = 1200,
  height = 600,
  font_size = 22,
  theme = "Catppuccin Mocha",
  typing_speed = "60ms",
  playback_speed = 1,
  line_pause = "500ms",
  backend = "R",
  screenshots = NULL,
  start_pause = "800ms",
  end_pause = "500ms",
  paragraph_pause = NULL,
  typing_speed_jitter = 0
) {
  if (length(r_lines) == 0L) {
    cli::cli_abort("No R code to record.")
  }

  startup <- backend_command(backend)

  header <- c(
    sprintf('Output "%s"', escape_tape_string(output)),
    'Set Shell "bash"',
    sprintf('Set FontSize %d', as.integer(font_size)),
    sprintf('Set Width %d', as.integer(width)),
    sprintf('Set Height %d', as.integer(height)),
    sprintf('Set Theme "%s"', escape_tape_string(theme)),
    sprintf('Set TypingSpeed %s', typing_speed),
    sprintf('Set PlaybackSpeed %s', format(playback_speed)),
    'Hide',
    sprintf('Type "%s"', startup),
    'Enter',
    sprintf('Sleep %s', start_pause),
    'Ctrl+L',
    'Sleep 200ms',
    'Show'
  )

  base_ms <- parse_duration_ms(typing_speed)

  body <- unlist(lapply(seq_along(r_lines), function(i) {
    line <- r_lines[[i]]
    if (!nzchar(trimws(line)) && !is.null(paragraph_pause)) {
      return(sprintf('Sleep %s', paragraph_pause))
    }
    type_directive <- if (typing_speed_jitter > 0) {
      sampled_ms <- max(1L, round(base_ms * stats::runif(
        1L,
        1 - typing_speed_jitter,
        1 + typing_speed_jitter
      )))
      sprintf("Type@%dms %s", as.integer(sampled_ms), quote_for_tape(line))
    } else {
      paste("Type", quote_for_tape(line))
    }
    out <- c(
      type_directive,
      "Enter",
      sprintf("Sleep %s", line_pause)
    )
    for (sc in screenshots) {
      if (isTRUE(sc$after_line == i)) {
        out <- c(out, sprintf('Screenshot "%s"', escape_tape_string(sc$path)))
      }
    }
    out
  }))

  footer <- c(
    'Hide',
    'Type "q()"',
    "Enter",
    sprintf("Sleep %s", end_pause)
  )

  paste(c(header, body, footer), collapse = "\n")
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
