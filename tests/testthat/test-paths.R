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

test_that("find_binary returns empty string when nothing is set", {
  withr::local_options(list(
    vhsR.vhs_path = NULL,
    vhsR.ttyd_path = NULL,
    vhsR.ffmpeg_path = NULL
  ))
  withr::local_envvar(c(VHSR_VHS = "", VHSR_TTYD = "", VHSR_FFMPEG = ""))

  cache <- vhsr_cache_dir()
  for (name in c("vhs", "ttyd", "ffmpeg")) {
    cached <- file.path(cache, vhsR:::vhsr_exe(name))
    if (file.exists(cached)) {
      skip(sprintf("%s already cached, can't test empty case", name))
    }
  }
  if (any(nzchar(unname(Sys.which(c("vhs", "ttyd", "ffmpeg")))))) {
    skip("a system binary is on PATH, can't test empty case")
  }

  expect_identical(vhsr_vhs_path(), "")
  expect_identical(vhsr_ttyd_path(), "")
  expect_identical(vhsr_ffmpeg_path(), "")
})
