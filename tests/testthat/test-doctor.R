test_that("vhsr_doctor reports all-missing state cleanly", {
  testthat::local_mocked_bindings(
    vhsr_vhs_path    = function() "",
    vhsr_ttyd_path   = function() "",
    vhsr_ffmpeg_path = function() "",
    vhsr_arf_path    = function() "",
    vhsr_radian_path = function() ""
  )
  expect_snapshot(out <- vhsr_doctor())
  expect_false(out)
})

test_that("vhsr_doctor reports a fully-equipped system", {
  fake_cache <- withr::local_tempdir()
  fake_vhs <- file.path(fake_cache, "vhs")
  file.create(fake_vhs)

  testthat::local_mocked_bindings(
    vhsr_cache_dir   = function() fake_cache,
    vhsr_vhs_path    = function() fake_vhs,
    vhsr_ttyd_path   = function() "/system/bin/ttyd",
    vhsr_ffmpeg_path = function() "/system/bin/ffmpeg",
    vhsr_arf_path    = function() "/system/bin/arf",
    vhsr_radian_path = function() "/system/bin/radian",
    get_version      = function(path) "v1.0.0"
  )
  expect_snapshot(
    out <- vhsr_doctor(),
    transform = function(x) {
      x <- gsub(fake_cache, "<cache>", x, fixed = TRUE)
      gsub("'<cache>/", "'<cache>/", x, fixed = TRUE)
    }
  )
  expect_true(out)
})
