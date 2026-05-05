# Re-record the GIFs embedded in the vignettes.
#
# Vignette chunks that produce GIFs are eval = FALSE so the vignettes
# build on CRAN (which has no vhs binary). The committed
# vignettes/figures/*.gif files are produced by this script. Run from
# the package root whenever the example code changes:
#
#   Rscript data-raw/precompute-vignette-figures.R
#
# Requires vhs / ttyd / ffmpeg to be installed (see vhsr_doctor()).

devtools::load_all(".")

if (!vhsr_check()) {
  stop(
    "vhs / ttyd / ffmpeg not all available. Run vhsr_install() first.",
    call. = FALSE
  )
}

fig_dir <- "vignettes/figures"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# Pacing demo: same code, three different pacing recipes.
pacing_lines <- c(
  "fit <- lm(mpg ~ wt, data = mtcars)",
  "",
  "summary(fit)$coefficients",
  "",
  "predict(fit, newdata = data.frame(wt = c(2, 3, 4)))"
)

set.seed(1L)

# 1. Mechanical: every line at exactly typing_speed, no jitter, no
#    paragraph beat.
vhsR:::record_demo_lines(
  lines               = pacing_lines,
  output              = file.path(fig_dir, "pacing-mechanical.gif"),
  typing_speed        = "60ms",
  typing_speed_jitter = 0,
  paragraph_pause     = NULL,
  line_pause          = "500ms",
  quiet               = TRUE
)

set.seed(1L)

# 2. Jittered: same code, ±30% per-line typing speed.
vhsR:::record_demo_lines(
  lines               = pacing_lines,
  output              = file.path(fig_dir, "pacing-jittered.gif"),
  typing_speed        = "60ms",
  typing_speed_jitter = 0.3,
  paragraph_pause     = NULL,
  line_pause          = "500ms",
  quiet               = TRUE
)

set.seed(1L)

# 3. Jittered + paragraph beat: blank lines become a 1s pause.
vhsR:::record_demo_lines(
  lines               = pacing_lines,
  output              = file.path(fig_dir, "pacing-paragraph-pause.gif"),
  typing_speed        = "60ms",
  typing_speed_jitter = 0.3,
  paragraph_pause     = "1s",
  line_pause          = "600ms",
  quiet               = TRUE
)

# Tape-scripts vignette: a small non-R example using vhsr_run_tape()
# to show the escape hatch.
vhsr_run_tape(
  sprintf(
    'Output "%s"
Set Shell "bash"
Set FontSize 22
Set Width 1000
Set Height 400
Type "echo Hello from a hand-written tape!"
Enter
Sleep 800ms
Type `ls -1 *.R | head -3`
Enter
Sleep 1s',
    file.path(fig_dir, "tape-hello.gif")
  ),
  quiet = TRUE
)

cat(
  "Wrote:\n",
  paste(" -", list.files(fig_dir, full.names = TRUE), collapse = "\n"),
  "\n",
  sep = ""
)
