test_that("record_demo_screenshot rejects non-png output", {
  expect_error(
    record_demo_screenshot(1 + 1, output = "demo.gif"),
    "must end in.*png"
  )
})

test_that("record_demo_screenshot rejects out-of-range at=", {
  testthat::local_mocked_bindings(
    vhsr_check    = function() TRUE,
    vhsr_run_tape = function(tape, quiet = FALSE) invisible(NULL)
  )
  expect_error(
    record_demo_screenshot({ x <- 1 }, output = "f.png", at = "after:5"),
    "out of range"
  )
})

test_that("record_demo_screenshot rejects malformed at=", {
  testthat::local_mocked_bindings(
    vhsr_check    = function() TRUE,
    vhsr_run_tape = function(tape, quiet = FALSE) invisible(NULL)
  )
  expect_error(
    record_demo_screenshot({ x <- 1 }, output = "f.png", at = "halfway"),
    "after:N"
  )
})

test_that("record_demo_screenshot at = NULL targets the last line", {
  captured <- NULL
  testthat::local_mocked_bindings(
    record_demo_lines = function(...) {
      captured <<- list(...)
      invisible("throwaway.gif")
    }
  )
  record_demo_screenshot({ a <- 1; b <- 2; c <- 3 }, output = "shot.png")
  expect_equal(captured$screenshots[[1L]]$after_line, 3L)
  expect_equal(captured$screenshots[[1L]]$path, "shot.png")
})

test_that("record_demo_screenshot at = 'after:N' targets line N", {
  captured <- NULL
  testthat::local_mocked_bindings(
    record_demo_lines = function(...) {
      captured <<- list(...)
      invisible("throwaway.gif")
    }
  )
  record_demo_screenshot(
    { a <- 1; b <- 2; c <- 3 },
    output = "shot.png",
    at     = "after:2"
  )
  expect_equal(captured$screenshots[[1L]]$after_line, 2L)
})

test_that("record_demo_screenshot end-to-end produces a non-zero PNG", {
  skip_on_cran()
  skip_if_no_vhs()

  # vhs's Screenshot directive is occasionally racy — the headless
  # browser sometimes hasn't rendered the prompt by the time the
  # screenshot fires, in which case no PNG is written. Retry up to 3x
  # so this smoke test isn't flaky.
  out <- withr::local_tempfile(fileext = ".png")
  ok <- FALSE
  for (attempt in seq_len(3L)) {
    unlink(out)
    record_demo_screenshot(
      { x <- 1:3; sum(x) },
      output       = out,
      typing_speed = "20ms",
      line_pause   = "500ms",
      quiet        = TRUE
    )
    if (file.exists(out) && file.info(out)$size > 0) {
      ok <- TRUE
      break
    }
  }
  expect_true(ok, info = "vhs Screenshot did not produce a PNG in 3 attempts")
})
