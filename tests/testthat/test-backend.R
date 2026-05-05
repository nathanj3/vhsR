test_that("resolve_backend honours explicit choices", {
  expect_equal(resolve_backend("R"), "R")
})

test_that("resolve_backend('auto') returns 'R' when arf and radian both missing", {
  testthat::local_mocked_bindings(
    vhsr_arf_path    = function() "",
    vhsr_radian_path = function() ""
  )
  expect_equal(resolve_backend("auto"), "R")
})

test_that("resolve_backend('auto') prefers radian over R when arf missing", {
  testthat::local_mocked_bindings(
    vhsr_arf_path    = function() "",
    vhsr_radian_path = function() "/fake/radian"
  )
  expect_equal(resolve_backend("auto"), "radian")
})

test_that("resolve_backend('auto') prefers arf over radian when both available", {
  testthat::local_mocked_bindings(
    vhsr_arf_path    = function() "/fake/arf",
    vhsr_radian_path = function() "/fake/radian"
  )
  expect_equal(resolve_backend("auto"), "arf")
})

test_that("resolve_backend('arf') errors with install hint when missing", {
  testthat::local_mocked_bindings(vhsr_arf_path = function() "")
  expect_error(resolve_backend("arf"), "arf.*not found")
})

test_that("resolve_backend('radian') errors with install hint when missing", {
  testthat::local_mocked_bindings(vhsr_radian_path = function() "")
  expect_error(resolve_backend("radian"), "radian.*not found")
})
