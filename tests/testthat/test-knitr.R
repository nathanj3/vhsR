chunk_options <- function(...) {
  # Layer synthetic chunk options over knitr's full defaults so
  # `knitr::engine_output()` finds every option it expects (echo, eval,
  # results, ...). Real knit() does this for us; tests have to do it
  # themselves.
  knitr::opts_chunk$merge(list(...))
}

test_that("knit_engine_vhsr errors when output= is missing", {
  expect_error(
    knit_engine_vhsr(list(code = "1 + 1")),
    "requires.*output"
  )
})

test_that("knit_engine_vhsr errors when output= is empty", {
  expect_error(
    knit_engine_vhsr(list(code = "1 + 1", output = "")),
    "requires.*output"
  )
})

test_that("knit_engine_vhsr forwards chunk options to record_demo_lines", {
  skip_if_not_installed("knitr")
  captured <- NULL
  testthat::local_mocked_bindings(
    record_demo_lines = function(...) {
      captured <<- list(...)
      invisible("captured")
    }
  )
  knit_engine_vhsr(chunk_options(
    code   = c("x <- 1", "x"),
    output = "out.gif",
    width  = 1000,
    theme  = "Dracula"
  ))
  expect_equal(captured$lines, c("x <- 1", "x"))
  expect_equal(captured$output, "out.gif")
  expect_equal(captured$width, 1000)
  expect_equal(captured$theme, "Dracula")
  expect_true(captured$quiet)
})

test_that("knit_engine_vhsr only forwards recognised options", {
  skip_if_not_installed("knitr")
  captured <- NULL
  testthat::local_mocked_bindings(
    record_demo_lines = function(...) {
      captured <<- list(...)
      invisible("captured")
    }
  )
  knit_engine_vhsr(chunk_options(
    code   = "1 + 1",
    output = "out.gif",
    width  = 800
  ))
  # `width` is recognised; `eval`/`echo`/etc. (filled in by chunk_options)
  # are not in the forwarded set.
  expect_equal(captured$width, 800)
  expect_null(captured$eval)
  expect_null(captured$echo)
})

test_that("knit_engine_vhsr returns chunk output containing image-include", {
  skip_if_not_installed("knitr")
  testthat::local_mocked_bindings(
    record_demo_lines = function(...) invisible(NULL)
  )
  out <- knit_engine_vhsr(chunk_options(
    code   = "1 + 1",
    output = "demo.gif"
  ))
  rendered <- paste(as.character(out), collapse = "\n")
  expect_match(rendered, "![](demo.gif)", fixed = TRUE)
})

test_that("vhsr engine is registered with knitr after package load", {
  skip_if_not_installed("knitr")
  expect_true("vhsr" %in% names(knitr::knit_engines$get()))
})
