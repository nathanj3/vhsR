resolve_backend <- function(backend = c("auto", "arf", "radian", "R")) {
  backend <- match.arg(backend)
  if (backend == "auto") {
    if (nzchar(vhsr_arf_path())) {
      "arf"
    } else if (nzchar(vhsr_radian_path())) {
      "radian"
    } else {
      "R"
    }
  } else if (backend == "arf" && !nzchar(vhsr_arf_path())) {
    cli::cli_abort(c(
      "{.code arf} not found.",
      i = "Install on macOS/Linux with {.code curl --proto '=https' --tlsv1.2 -LsSf https://github.com/eitsupi/arf/releases/latest/download/arf-console-installer.sh | sh}",
      i = "Pass {.code backend = \"R\"} or {.code backend = \"radian\"} to fall back."
    ))
  } else if (backend == "radian" && !nzchar(vhsr_radian_path())) {
    cli::cli_abort(c(
      "{.code radian} not found.",
      i = "Install with {.code pip install radian} (or {.code pipx install radian}).",
      i = "Pass {.code backend = \"R\"} to fall back to plain R."
    ))
  } else {
    backend
  }
}

has_colorout <- function() {
  isTRUE(requireNamespace("colorout", quietly = TRUE))
}

#' Install the colorout package for highlighted R output
#'
#' Convenience wrapper around [utils::install.packages()] for the
#' [colorout](https://github.com/jalvesaq/colorout) package, which colors R
#' output (results, warnings, errors) inside the REPL. Once installed,
#' [record_demo()] auto-injects `library(colorout)` whenever it falls back
#' to the plain R backend.
#'
#' @param ... Passed on to [utils::install.packages()].
#' @return Invisibly `TRUE` if `colorout` is available afterwards.
#' @export
vhsr_install_colorout <- function(...) {
  if (has_colorout()) {
    cli::cli_alert_info("{.pkg colorout} already installed.")
    return(invisible(TRUE))
  }
  utils::install.packages(
    'colorout',
    repos = 'https://community.r-multiverse.org',
    ...
  )
  ok <- has_colorout()
  if (ok) {
    cli::cli_alert_success("{.pkg colorout} installed.")
  } else {
    cli::cli_alert_warning("{.pkg colorout} install did not succeed.")
  }
  invisible(ok)
}
