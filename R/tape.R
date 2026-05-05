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
  rprofile = NULL
) {
  if (length(r_lines) == 0L) {
    cli::cli_abort("No R code to record.")
  }

  startup <- backend_command(backend)

  env_lines <- character()
  if (!is.null(rprofile)) {
    env_lines <- sprintf('Env R_PROFILE_USER "%s"', rprofile)
  }

  header <- c(
    sprintf('Output "%s"', output),
    'Set Shell "bash"',
    sprintf('Set FontSize %d', as.integer(font_size)),
    sprintf('Set Width %d', as.integer(width)),
    sprintf('Set Height %d', as.integer(height)),
    sprintf('Set Theme "%s"', theme),
    sprintf('Set TypingSpeed %s', typing_speed),
    sprintf('Set PlaybackSpeed %s', format(playback_speed)),
    env_lines,
    'Hide',
    sprintf('Type "%s"', startup),
    'Enter',
    'Sleep 1s',
    'Ctrl+L',
    'Sleep 200ms',
    'Show'
  )

  body <- unlist(lapply(r_lines, function(line) {
    c(
      paste("Type", quote_for_tape(line)),
      "Enter",
      sprintf("Sleep %s", line_pause)
    )
  }))

  footer <- c('Hide', 'Type "q()"', "Enter", "Sleep 500ms")

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
