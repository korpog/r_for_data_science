library(tidyverse)
library(nycflights13)

flights |>
  filter(dest == "IAH") |>
  group_by(year, month, day) |>
  summarize(
    n =
      n(), delay = mean(arr_delay, na.rm = TRUE)
  ) |>
  filter(n > 10)

# code style --------------------------------------------------------------


flights |>
  filter(
    carrier == "UA",
    dest %in% c("IAH", "HOU"),
    sched_dep_time >
      0900,
    sched_arr_time < 2000
  ) |>
  group_by(flight) |>
  summarize(
    delay = mean(arr_delay, na.rm = TRUE),
    cancelled = sum(is.na(arr_delay)),
    n = n()
  ) |>
  filter(n > 10)
