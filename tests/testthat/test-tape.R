test_that("quote_for_tape uses double quotes for plain lines", {
  expect_equal(quote_for_tape("x <- 1"), "\"x <- 1\"")
})

test_that("quote_for_tape switches to backticks when line has double quotes", {
  expect_equal(
    quote_for_tape('x <- "hello"'),
    '`x <- "hello"`'
  )
})

test_that("quote_for_tape errors when line has both quote styles", {
  expect_error(
    quote_for_tape('paste("a", `b`)'),
    "double quotes and backticks"
  )
})

test_that("build_tape writes header, body, footer in order", {
  out <- build_tape(c("x <- 1", "x + 1"), output = "demo.gif")

  expect_match(out, '^Output "demo.gif"')
  expect_match(out, "Set FontSize 22")
  expect_match(out, 'Type "R --quiet --no-save"')
  expect_match(out, "Ctrl\\+L")
  expect_match(out, "Type \"x <- 1\"\nEnter\nSleep 500ms")
  expect_match(out, "Type \"x \\+ 1\"\nEnter\nSleep 500ms")
  expect_match(out, "Hide\nType \"q\\(\\)\"\nEnter\nSleep 500ms$")
})

test_that("build_tape rejects empty input", {
  expect_error(build_tape(character(), output = "x.gif"), "No R code")
})

test_that("build_tape passes styling options through", {
  out <- build_tape(
    "x <- 1",
    output = "demo.gif",
    width = 800,
    height = 400,
    font_size = 18,
    theme = "Dracula"
  )
  expect_match(out, "Set Width 800")
  expect_match(out, "Set Height 400")
  expect_match(out, "Set FontSize 18")
  expect_match(out, 'Set Theme "Dracula"')
})

test_that("build_tape switches launch command for radian backend", {
  out <- build_tape("x <- 1", output = "demo.gif", backend = "radian")
  expect_match(out, 'Type "radian --no-save"')
  expect_false(grepl('Type "R --quiet --no-save"', out))
})

test_that("build_tape switches launch command for arf backend", {
  out <- build_tape("x <- 1", output = "demo.gif", backend = "arf")
  expect_match(out, 'Type "arf"')
  expect_false(grepl('Type "R --quiet --no-save"', out))
  expect_false(grepl('Type "radian --no-save"', out))
})

test_that("build_tape emits Screenshot directive after the requested line", {
  out <- build_tape(
    c("x <- 1", "x + 1", "x * 2"),
    output = "demo.gif",
    screenshots = list(list(after_line = 2, path = "frame.png"))
  )
  expect_match(
    out,
    'Type "x \\+ 1"\nEnter\nSleep 500ms\nScreenshot "frame.png"\nType "x \\* 2"'
  )
})

test_that("build_tape with screenshots = NULL is byte-identical to omitted", {
  expect_identical(
    build_tape("x <- 1", output = "demo.gif", screenshots = NULL),
    build_tape("x <- 1", output = "demo.gif")
  )
})

test_that("build_tape supports multiple screenshots in a single recording", {
  out <- build_tape(
    c("a", "b", "c"),
    output = "demo.gif",
    screenshots = list(
      list(after_line = 1, path = "f1.png"),
      list(after_line = 3, path = "f2.png")
    )
  )
  expect_match(out, 'Screenshot "f1.png"')
  expect_match(out, 'Screenshot "f2.png"')
})

test_that("build_tape default pacing args produce unchanged tape (regression)", {
  before <- build_tape(c("x <- 1", "y <- 2"), output = "demo.gif")
  after <- build_tape(
    c("x <- 1", "y <- 2"),
    output              = "demo.gif",
    start_pause         = "800ms",
    end_pause           = "500ms",
    paragraph_pause     = NULL,
    typing_speed_jitter = 0
  )
  expect_identical(before, after)
})

test_that("start_pause replaces the hardcoded startup sleep", {
  out <- build_tape("x <- 1", output = "demo.gif", start_pause = "2s")
  expect_match(out, "Enter\nSleep 2s\nCtrl\\+L")
  expect_false(grepl("Enter\nSleep 800ms\nCtrl\\+L", out))
})

test_that("end_pause replaces the hardcoded final sleep", {
  out <- build_tape("x <- 1", output = "demo.gif", end_pause = "1s")
  expect_match(out, 'Hide\nType "q\\(\\)"\nEnter\nSleep 1s$')
})

test_that("paragraph_pause turns blank lines into sleeps", {
  out <- build_tape(
    c("x <- 1", "", "y <- 2"),
    output = "demo.gif",
    paragraph_pause = "1s"
  )
  expect_match(out, "Sleep 1s\nType \"y <- 2\"")
  expect_false(grepl('Type ""', out, fixed = TRUE))
})

test_that("paragraph_pause = NULL preserves current blank-line behaviour", {
  out <- build_tape(c("x <- 1", "", "y <- 2"), output = "demo.gif")
  expect_match(out, 'Type ""\nEnter')
})

test_that("typing_speed_jitter emits per-line Type@<ms> directives", {
  withr::local_seed(1)
  out <- build_tape(
    c("x <- 1", "y <- 2"),
    output              = "demo.gif",
    typing_speed        = "100ms",
    typing_speed_jitter = 0.3
  )
  expect_equal(
    length(regmatches(out, gregexpr("Type@\\d+ms", out))[[1L]]),
    2L
  )
})

test_that("typing_speed_jitter = 0 emits plain Type directives (regression)", {
  out <- build_tape("x <- 1", output = "demo.gif", typing_speed_jitter = 0)
  expect_false(grepl("Type@", out, fixed = TRUE))
})

test_that("parse_duration_ms handles ms and s", {
  expect_equal(parse_duration_ms("60ms"), 60)
  expect_equal(parse_duration_ms("1s"), 1000)
  expect_equal(parse_duration_ms("0.5s"), 500)
  expect_error(parse_duration_ms("60"), "Cannot parse duration")
  expect_error(parse_duration_ms("forever"), "Cannot parse duration")
})

test_that("deparse_block splits a brace block into one line per statement", {
  e <- quote({
    x <- 1
    y <- 2
    x + y
  })
  expect_equal(deparse_block(e), c("x <- 1", "y <- 2", "x + y"))
})

test_that("deparse_block treats a non-brace expression as a single line", {
  e <- quote(mean(1:5))
  expect_equal(deparse_block(e), "mean(1:5)")
})
