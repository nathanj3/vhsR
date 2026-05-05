# knitr chunk engine for vhsR.
#
# Registered in .onLoad() (see R/zzz.R) when the knitr namespace is
# available. Authors write a chunk like:
#
#   ```{vhsr, output="man/figures/demo.gif", height=400}
#   x <- 1:5
#   mean(x)
#   ```
#
# and get a recorded GIF inserted into the rendered document.

knit_engine_vhsr <- function(options) {
  if (isFALSE(options$eval)) {
    return(knitr::engine_output(options, code = options$code, out = NULL))
  }
  if (is.null(options$output) || !nzchar(options$output)) {
    cli::cli_abort(c(
      "{.code vhsr} chunk requires an {.arg output} option.",
      i = "Set it like {.code {{vhsr, output=\"man/figures/demo.gif\"}}}."
    ))
  }

  lines <- options$code
  opt_keys <- intersect(names(options), names(formals(tape_options)))
  opts <- do.call(tape_options, options[opt_keys])
  args <- list(lines = lines, output = options$output, opts = opts, quiet = TRUE)
  if (!is.null(options$backend)) args$backend <- options$backend

  do.call(record_demo_lines, args)

  knitr::engine_output(
    options,
    code = lines,
    out  = sprintf("![](%s)", options$output)
  )
}
