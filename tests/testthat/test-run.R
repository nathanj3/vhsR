test_that("vhsr_run_tape errors when vhs binary is not available", {
  testthat::local_mocked_bindings(vhsr_vhs_path = function() "")
  expect_error(vhsr_run_tape("Output \"x.gif\""), "vhs.*not found")
})

test_that("vhsr_run_tape errors clearly for a missing tape file path", {
  testthat::local_mocked_bindings(vhsr_vhs_path = function() "/fake/vhs")
  expect_error(
    vhsr_run_tape("/no/such/tape/file.tape"),
    "Tape file not found"
  )
})

test_that("vhsr_run_tape removes the rod/ detritus from tempdir() after running", {
  rod_dir <- file.path(tempdir(), "rod")

  testthat::local_mocked_bindings(
    vhsr_vhs_path = function() "/fake/vhs",
    .package = "vhsR"
  )
  # Stub processx::run: simulate vhs leaving a `rod/` directory behind.
  testthat::local_mocked_bindings(
    run = function(...) {
      dir.create(rod_dir, showWarnings = FALSE)
      file.create(file.path(rod_dir, "browser-cache"))
      list(status = 0L, stdout = "", stderr = "")
    },
    .package = "processx"
  )

  vhsr_run_tape("Output \"x.gif\"\nType \"hi\"\nEnter")
  expect_false(dir.exists(rod_dir))
})

test_that("vhsr_run_tape still cleans up rod/ when vhs exits non-zero", {
  rod_dir <- file.path(tempdir(), "rod")

  testthat::local_mocked_bindings(
    vhsr_vhs_path = function() "/fake/vhs",
    .package = "vhsR"
  )
  testthat::local_mocked_bindings(
    run = function(...) {
      dir.create(rod_dir, showWarnings = FALSE)
      list(status = 1L, stdout = "", stderr = "boom")
    },
    .package = "processx"
  )

  expect_error(
    vhsr_run_tape("Output \"x.gif\"\nType \"hi\"\nEnter"),
    "vhs exited with status"
  )
  expect_false(dir.exists(rod_dir))
})
