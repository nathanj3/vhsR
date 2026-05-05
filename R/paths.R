#' Locate the per-user vhsR binary cache
#'
#' The directory under [tools::R_user_dir()] where `vhsr_install()` places
#' downloaded `vhs`, `ttyd`, and `ffmpeg` binaries. The path is
#' platform-and-architecture-specific so a single `$HOME` can serve multiple
#' machines (e.g. via NFS).
#'
#' @return A length-1 character path. The directory is not created on read.
#' @export
vhsr_cache_dir <- function() {
  root <- tools::R_user_dir("vhsR", "cache")
  file.path(root, "bin", paste(vhsr_os(), vhsr_arch(), sep = "_"))
}

find_binary <- function(name, opt_key, env_key) {
  o <- getOption(opt_key, default = "")
  if (nzchar(o) && file.exists(o)) {
    return(normalizePath(o, winslash = "/", mustWork = FALSE))
  }
  e <- Sys.getenv(env_key, unset = "")
  if (nzchar(e) && file.exists(e)) {
    return(normalizePath(e, winslash = "/", mustWork = FALSE))
  }
  cached <- file.path(vhsr_cache_dir(), vhsr_exe(name))
  if (file.exists(cached)) {
    return(normalizePath(cached, winslash = "/", mustWork = FALSE))
  }
  sys <- unname(Sys.which(name))
  if (nzchar(sys)) {
    return(normalizePath(sys, winslash = "/", mustWork = FALSE))
  }
  ""
}

#' Resolve paths to vhs and its dependencies
#'
#' Each function returns the first match in this order:
#' option (`vhsR.<tool>_path`) -> env var (`VHSR_<TOOL>`) -> per-user cache
#' (see [vhsr_cache_dir()]) -> system `PATH`. Returns `""` when nothing
#' matches.
#'
#' @return A length-1 character path, or `""` if not found.
#' @name vhsr_paths
NULL

#' @rdname vhsr_paths
#' @export
vhsr_vhs_path <- function() {
  find_binary("vhs", "vhsR.vhs_path", "VHSR_VHS")
}

#' @rdname vhsr_paths
#' @export
vhsr_ttyd_path <- function() {
  find_binary("ttyd", "vhsR.ttyd_path", "VHSR_TTYD")
}

#' @rdname vhsr_paths
#' @export
vhsr_ffmpeg_path <- function() {
  find_binary("ffmpeg", "vhsR.ffmpeg_path", "VHSR_FFMPEG")
}

#' @rdname vhsr_paths
#' @export
vhsr_radian_path <- function() {
  find_binary("radian", "vhsR.radian_path", "VHSR_RADIAN")
}

#' @rdname vhsr_paths
#' @export
vhsr_arf_path <- function() {
  find_binary("arf", "vhsR.arf_path", "VHSR_ARF")
}
