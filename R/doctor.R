#' Check whether all required binaries are available
#'
#' Silent variant: returns `TRUE` when `vhs`, `ttyd`, and `ffmpeg` can all
#' be located via [vhsr_vhs_path()] / [vhsr_ttyd_path()] /
#' [vhsr_ffmpeg_path()], `FALSE` otherwise. Used at the top of
#' [record_demo()] to bail with an actionable error.
#'
#' @return `TRUE` or `FALSE`.
#' @export
vhsr_check <- function() {
  all(nzchar(c(vhsr_vhs_path(), vhsr_ttyd_path(), vhsr_ffmpeg_path())))
}

#' Report on the state of vhs and its dependencies
#'
#' Prints a one-line report per binary: name, version, where it was found
#' (cache or system), and full path. When something is missing, prints a
#' one-line `vhsr_install()` call to fix it.
#'
#' @return Invisibly returns the result of [vhsr_check()].
#' @export
vhsr_doctor <- function() {
  cli::cli_h2("vhsR diagnostic")
  report_one("vhs", vhsr_vhs_path())
  report_one("ttyd", vhsr_ttyd_path())
  report_one("ffmpeg", vhsr_ffmpeg_path())

  cli::cli_h3("Optional (syntax highlighting)")
  arf <- vhsr_arf_path()
  if (nzchar(arf)) {
    report_one("arf", arf)
  } else {
    cli::cli_alert_warning(
      "arf: not found ({.url https://github.com/eitsupi/arf} — modern Rust-based R console, preferred backend)"
    )
  }
  radian <- vhsr_radian_path()
  if (nzchar(radian)) {
    report_one("radian", radian)
  } else {
    cli::cli_alert_warning(
      "radian: not found ({.code pip install radian} — fallback when arf is absent)"
    )
  }
  if (has_colorout()) {
    cli::cli_alert_success(
      "colorout: installed (output coloring under plain R)"
    )
  } else {
    cli::cli_alert_warning(
      "colorout: not installed ({.run vhsr_install_colorout()} for output coloring under plain R)"
    )
  }

  ok <- vhsr_check()
  if (!ok) {
    cli::cli_alert_info(
      "Run {.run vhsr_install()} to install missing required binaries."
    )
  }
  invisible(ok)
}

report_one <- function(name, path) {
  if (!nzchar(path)) {
    cli::cli_alert_danger("{name}: not found")
    return(invisible())
  }
  cache <- vhsr_cache_dir()
  loc <- if (
    startsWith(path, normalizePath(cache, winslash = "/", mustWork = FALSE))
  ) {
    "cache"
  } else {
    "system"
  }
  v <- tryCatch(get_version(path), error = function(e) "?")
  cli::cli_alert_success("{name} {v} ({loc}): {.path {path}}")
}

get_version <- function(path) {
  out <- processx::run(
    path,
    "--version",
    error_on_status = FALSE,
    timeout = 5
  )
  txt <- if (nzchar(out$stdout)) out$stdout else out$stderr
  trimws(strsplit(txt, "\n", fixed = TRUE)[[1L]][1L])
}
