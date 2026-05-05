test_that("install_one skips when tool is already on PATH (regression: macOS bug)", {
  testthat::local_mocked_bindings(
    vhsr_ttyd_path = function() "/opt/homebrew/bin/ttyd",
    install_ttyd   = function(cache, version) {
      stop("install_ttyd should not have been called")
    }
  )
  expect_message(
    res <- install_one("ttyd", cache = tempdir(), force = FALSE, version = "x"),
    "already available"
  )
  expect_identical(res, "/opt/homebrew/bin/ttyd")
})

test_that("install_one re-installs when force = TRUE even if already available", {
  called <- FALSE
  testthat::local_mocked_bindings(
    vhsr_vhs_path = function() "/opt/homebrew/bin/vhs",
    install_vhs   = function(cache, version) {
      called <<- TRUE
      "/cache/vhs"
    }
  )
  install_one("vhs", cache = tempdir(), force = TRUE, version = "x")
  expect_true(called)
})

test_that("install_one delegates to per-tool installer when not yet available", {
  called <- character()
  testthat::local_mocked_bindings(
    vhsr_ffmpeg_path = function() "",
    install_ffmpeg   = function(cache, version) {
      called <<- c(called, "ffmpeg")
      "/cache/ffmpeg"
    }
  )
  install_one("ffmpeg", cache = tempdir(), force = FALSE, version = "latest")
  expect_equal(called, "ffmpeg")
})

test_that("install_vhs constructs the correct GitHub release URL per platform", {
  captured <- NULL
  testthat::local_mocked_bindings(
    vhsr_os = function() "linux",
    vhsr_arch = function() "x86_64",
    download_and_extract = function(url, dest_dir, exe_name) {
      captured <<- url
      file.path(dest_dir, exe_name)
    }
  )
  install_vhs(cache = tempdir(), version = "0.11.0")
  expect_equal(
    captured,
    "https://github.com/charmbracelet/vhs/releases/download/v0.11.0/vhs_0.11.0_Linux_x86_64.tar.gz"
  )
})

test_that("install_ttyd aborts on darwin with brew instructions", {
  testthat::local_mocked_bindings(vhsr_os = function() "darwin")
  expect_error(install_ttyd(tempdir(), "1.7.7"), "brew install ttyd")
})

test_that("install_ffmpeg aborts on darwin with brew instructions", {
  testthat::local_mocked_bindings(vhsr_os = function() "darwin")
  expect_error(install_ffmpeg(tempdir(), "latest"), "brew install ffmpeg")
})
