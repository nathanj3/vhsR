#' Embed a vhsR recording as an HTML widget
#'
#' Returns an [htmltools::tagList()] containing the recorded GIF /
#' video, suitable for embedding in an Rmarkdown chunk
#' (`results = "asis"`) or a Shiny UI. Auto-detects the format from
#' `file`'s extension: `.gif` becomes a styled `<img>`,
#' `.mp4` / `.webm` become a `<video controls loop muted playsinline>`.
#'
#' @param file Path to a `.gif`, `.mp4`, or `.webm` file produced by
#'   [record_demo()] / [vhsr_run_tape()].
#' @param width,height Optional CSS dimensions. Numeric values are
#'   treated as pixels (e.g. `800` → `"800px"`); strings pass through
#'   verbatim (e.g. `"100%"`).
#' @return An `htmltools::tagList`.
#' @export
#' @examples
#' \dontrun{
#' record_demo({ x <- 1:5; mean(x) }, output = "demo.gif")
#' vhsr_widget("demo.gif")
#' }
vhsr_widget <- function(file, width = NULL, height = NULL) {
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    cli::cli_abort(c(
      "{.fn vhsr_widget} needs the {.pkg htmltools} package.",
      i = "Run {.run install.packages(\"htmltools\")}."
    ))
  }
  if (!file.exists(file)) {
    cli::cli_abort("File not found: {.path {file}}")
  }

  ext <- tolower(tools::file_ext(file))
  style <- css_dimensions(width, height)

  switch(
    ext,
    gif = htmltools::tagList(
      htmltools::div(
        style = sprintf("max-width:100%%; %s", style),
        htmltools::tags$img(
          src   = file,
          style = "max-width:100%; height:auto;"
        )
      )
    ),
    mp4  = video_widget(file, "video/mp4",  style),
    webm = video_widget(file, "video/webm", style),
    cli::cli_abort(c(
      "Unsupported widget format: {.val {ext}}",
      i = "Use one of {.val gif}, {.val mp4}, {.val webm}."
    ))
  )
}

css_dimensions <- function(width, height) {
  to_css <- function(x) {
    if (is.null(x)) NULL
    else if (is.numeric(x)) sprintf("%spx", x)
    else as.character(x)
  }
  paste(c(
    if (!is.null(width))  sprintf("width:%s;",  to_css(width)),
    if (!is.null(height)) sprintf("height:%s;", to_css(height))
  ), collapse = "")
}

video_widget <- function(file, mime, style) {
  htmltools::tagList(
    htmltools::tags$video(
      controls    = NA,
      loop        = NA,
      muted       = NA,
      playsinline = NA,
      style       = sprintf("max-width:100%%; %s", style),
      htmltools::tags$source(src = file, type = mime)
    )
  )
}
