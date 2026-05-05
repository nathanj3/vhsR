# Single source of truth for the optional R-frontend backends. Doctor and
# resolve_backend both consume this so the priority order, install hints,
# and path lookups don't drift apart. `path_fn` is a closure that resolves
# `vhsr_<name>_path` at call time so `local_mocked_bindings()` works.
backends <- function() {
  list(
    arf = list(
      path_fn = function() vhsr_arf_path(),
      doctor_hint = "{.url https://github.com/eitsupi/arf} - modern Rust-based R console, preferred backend",
      install_hint = c(
        i = "Install on macOS/Linux with {.code curl --proto '=https' --tlsv1.2 -LsSf https://github.com/eitsupi/arf/releases/latest/download/arf-console-installer.sh | sh}",
        i = "Pass {.code backend = \"R\"} or {.code backend = \"radian\"} to fall back."
      )
    ),
    radian = list(
      path_fn = function() vhsr_radian_path(),
      doctor_hint = "{.code pip install radian} - fallback when arf is absent",
      install_hint = c(
        i = "Install with {.code pip install radian} (or {.code pipx install radian}).",
        i = "Pass {.code backend = \"R\"} to fall back to plain R."
      )
    )
  )
}

resolve_backend <- function(backend = c("auto", "arf", "radian", "R")) {
  backend <- match.arg(backend)
  if (backend == "R") {
    return("R")
  }
  bs <- backends()
  if (backend == "auto") {
    for (name in names(bs)) {
      if (nzchar(bs[[name]]$path_fn())) {
        return(name)
      }
    }
    return("R")
  }
  if (!nzchar(bs[[backend]]$path_fn())) {
    cli::cli_abort(c(
      "{.code {backend}} not found.",
      bs[[backend]]$install_hint
    ))
  }
  backend
}
