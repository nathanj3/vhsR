test_that("vhsr_widget errors for missing file", {
  skip_if_not_installed("htmltools")
  expect_error(
    vhsr_widget("/no/such/file.gif"),
    "File not found"
  )
})

test_that("vhsr_widget rejects unsupported extensions", {
  skip_if_not_installed("htmltools")
  f <- withr::local_tempfile(fileext = ".svg")
  file.create(f)
  expect_error(vhsr_widget(f), "Unsupported widget format")
})

test_that("vhsr_widget produces an <img> tag for .gif", {
  skip_if_not_installed("htmltools")
  f <- withr::local_tempfile(fileext = ".gif")
  file.create(f)
  out <- vhsr_widget(f)
  rendered <- as.character(out)
  expect_match(rendered, "<img", fixed = TRUE)
  expect_match(rendered, basename(f), fixed = TRUE)
})

test_that("vhsr_widget produces a <video> tag with mp4 source for .mp4", {
  skip_if_not_installed("htmltools")
  f <- withr::local_tempfile(fileext = ".mp4")
  file.create(f)
  out <- vhsr_widget(f)
  rendered <- as.character(out)
  expect_match(rendered, "<video", fixed = TRUE)
  expect_match(rendered, 'type="video/mp4"', fixed = TRUE)
  expect_match(rendered, "controls", fixed = TRUE)
  expect_match(rendered, "loop", fixed = TRUE)
  expect_match(rendered, "muted", fixed = TRUE)
  expect_match(rendered, "playsinline", fixed = TRUE)
})

test_that("vhsr_widget produces a <video> tag with webm source for .webm", {
  skip_if_not_installed("htmltools")
  f <- withr::local_tempfile(fileext = ".webm")
  file.create(f)
  out <- vhsr_widget(f)
  rendered <- as.character(out)
  expect_match(rendered, "<video", fixed = TRUE)
  expect_match(rendered, 'type="video/webm"', fixed = TRUE)
})

test_that("numeric width is converted to px", {
  skip_if_not_installed("htmltools")
  f <- withr::local_tempfile(fileext = ".gif")
  file.create(f)
  out <- vhsr_widget(f, width = 800)
  expect_match(as.character(out), "width:800px", fixed = TRUE)
})

test_that("string width passes through verbatim", {
  skip_if_not_installed("htmltools")
  f <- withr::local_tempfile(fileext = ".gif")
  file.create(f)
  out <- vhsr_widget(f, width = "100%")
  expect_match(as.character(out), "width:100%", fixed = TRUE)
})

test_that("height is applied alongside width", {
  skip_if_not_installed("htmltools")
  f <- withr::local_tempfile(fileext = ".gif")
  file.create(f)
  out <- vhsr_widget(f, width = 800, height = 400)
  rendered <- as.character(out)
  expect_match(rendered, "width:800px", fixed = TRUE)
  expect_match(rendered, "height:400px", fixed = TRUE)
})

test_that("css_dimensions returns empty string when both args are NULL", {
  expect_equal(css_dimensions(NULL, NULL), "")
})
