library(vhsR)
record_demo(
  {
    library(dplyr)

    mtcars |> head(3)

    mtcars |>
      filter(cyl == 6) |>
      arrange(desc(mpg))

    mtcars |>
      group_by(cyl) |>
      summarise(avg_mpg = mean(mpg), n = n())
    
  },
  typing_speed = "70ms",
  typing_speed_jitter = 0.25,
  line_pause = "600ms",
  paragraph_pause = "1000ms",
  height = 800,
  output = "man/figures/hero.gif"
)
