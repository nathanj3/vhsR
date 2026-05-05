test_that("record_demo errors with actionable message when binaries missing", {
  testthat::local_mocked_bindings(vhsr_check = function() FALSE)
  expect_error(record_demo(1 + 1), "binaries not found")
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
