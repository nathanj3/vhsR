test_that("record_demo errors with actionable message when binaries missing", {
  testthat::local_mocked_bindings(vhsr_check = function() FALSE)
  expect_error(record_demo(1 + 1), "binaries not found")
})

test_that("record_demo_lines runs the pipeline without binaries when stubbed", {
  captured_tape <- NULL
  testthat::local_mocked_bindings(
    vhsr_check    = function() TRUE,
    vhsr_run_tape = function(tape, quiet = FALSE) {
      captured_tape <<- tape
      invisible(NULL)
    }
  )
  res <- record_demo_lines(c("x <- 1", "x"), output = "out.gif")
  expect_identical(res, "out.gif")
  expect_match(captured_tape, 'Output "out.gif"')
  expect_match(captured_tape, 'Type "x <- 1"')
})

test_that("record_demo_file errors when the file does not exist", {
  expect_error(
    record_demo_file("/no/such/file.R"),
    "Script file not found"
  )
})

test_that("record_demo_file errors with cli when the file has bad syntax", {
  bad <- withr::local_tempfile(fileext = ".R")
  writeLines("x <-", bad)
  expect_error(record_demo_file(bad), "Syntax error")
})

test_that("record_demo_file forwards the script lines and styling args", {
  good <- withr::local_tempfile(fileext = ".R")
  writeLines(c("x <- 1:5", "mean(x)"), good)

  captured <- NULL
  testthat::local_mocked_bindings(
    record_demo_lines = function(...) {
      captured <<- list(...)
      invisible("demo.gif")
    }
  )
  record_demo_file(good, output = "out.gif", width = 800)
  expect_equal(captured$lines, c("x <- 1:5", "mean(x)"))
  expect_equal(captured$output, "out.gif")
  expect_equal(captured$width, 800)
})

test_that("record_demo_file end-to-end produces a non-empty GIF", {
  skip_on_cran()
  skip_if_no_vhs()

  good <- withr::local_tempfile(fileext = ".R")
  writeLines(c("x <- 1:3", "sum(x)"), good)
  out <- withr::local_tempfile(fileext = ".gif")
  record_demo_file(
    good,
    output       = out,
    typing_speed = "20ms",
    line_pause   = "200ms",
    quiet        = TRUE
  )
  expect_true(file.exists(out))
  expect_gt(file.info(out)$size, 0)
})

test_that("record_demo end-to-end produces a non-empty output file", {
  skip_on_cran()
  skip_if_no_vhs()

  out <- withr::local_tempfile(fileext = ".gif")
  res <- record_demo(
    {
      x <- 1:3
      sum(x)
    },
    output = out,
    typing_speed = "20ms",
    line_pause = "200ms",
    quiet = TRUE
  )
  expect_identical(res, out)
  expect_true(file.exists(out))
  expect_gt(file.info(out)$size, 0)
})
