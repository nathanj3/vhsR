.onLoad <- function(libname, pkgname) {
  if (requireNamespace("knitr", quietly = TRUE)) {
    knitr::knit_engines$set(vhsr = knit_engine_vhsr)
  }
  invisible()
}
