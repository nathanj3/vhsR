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

test_that("build_tape emits Env R_PROFILE_USER when rprofile supplied", {
  out <- build_tape(
    "x <- 1",
    output = "demo.gif",
    rprofile = "/tmp/colorout.R"
  )
  expect_match(out, 'Env R_PROFILE_USER "/tmp/colorout.R"')
})

test_that("build_tape omits Env line when rprofile is NULL", {
  out <- build_tape("x <- 1", output = "demo.gif")
  expect_false(grepl("Env R_PROFILE_USER", out))
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
