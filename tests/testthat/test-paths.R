test_that("vhsr_cache_dir is platform-and-arch suffixed", {
  d <- vhsr_cache_dir()
  expect_true(grepl(paste(vhsR:::vhsr_os(), vhsR:::vhsr_arch(), sep = "_"), d))
  expect_match(d, "/bin/")
})

test_that("find_binary honours the option ahead of cache and PATH", {
  fake <- withr::local_tempfile()
  file.create(fake)

  withr::local_options(list(vhsR.vhs_path = fake))
  expect_equal(
    vhsr_vhs_path(),
    normalizePath(fake, winslash = "/", mustWork = FALSE)
  )
})

test_that("find_binary honours the env var when no option is set", {
  fake <- withr::local_tempfile()
  file.create(fake)

  withr::local_options(list(vhsR.vhs_path = NULL))
  withr::local_envvar(c(VHSR_VHS = fake))
  expect_equal(
    vhsr_vhs_path(),
    normalizePath(fake, winslash = "/", mustWork = FALSE)
  )
})

test_that("find_binary returns empty string when none of option/env/cache/PATH match", {
  empty_cache <- withr::local_tempdir()
  testthat::local_mocked_bindings(vhsr_cache_dir = function() empty_cache)
  withr::local_options(list(
    vhsR.vhs_path    = NULL,
    vhsR.ttyd_path   = NULL,
    vhsR.ffmpeg_path = NULL
  ))
  withr::local_envvar(c(
    VHSR_VHS    = "",
    VHSR_TTYD   = "",
    VHSR_FFMPEG = "",
    PATH        = ""
  ))

  expect_identical(vhsr_vhs_path(),    "")
  expect_identical(vhsr_ttyd_path(),   "")
  expect_identical(vhsr_ffmpeg_path(), "")
})

test_that("find_binary picks the cache when option/env are unset", {
  cache <- withr::local_tempdir()
  bin <- file.path(cache, "vhs")
  file.create(bin)

  testthat::local_mocked_bindings(vhsr_cache_dir = function() cache)
  withr::local_options(list(vhsR.vhs_path = NULL))
  withr::local_envvar(c(VHSR_VHS = ""))

  expect_equal(
    vhsr_vhs_path(),
    normalizePath(bin, winslash = "/", mustWork = FALSE)
  )
})
