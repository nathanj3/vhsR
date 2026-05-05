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
