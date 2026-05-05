skip_if_no_vhs <- function() {
  if (!vhsr_check()) {
    testthat::skip("vhs / ttyd / ffmpeg not installed (see vhsr_install())")
  }
}
