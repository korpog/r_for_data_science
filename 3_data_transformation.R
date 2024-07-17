library(nycflights13)
library(tidyverse)

# 3.1
?flights
View(flights)

flights |>
  filter(dest == "IAH") |>
  group_by(year, month, day) |>
  summarize(
    arr_delay = mean(arr_delay, na.rm = TRUE)
  )

# 3.2
flights |>
  filter(dep_delay > 120)

flights |>
  filter(month == 1 & day == 1)

flights |>
  filter(month == 1 | month == 2)

flights |>
  filter(month %in% c(1, 2))

jan1 <- flights |>
  filter(month == 1 & day == 1)

flights |>
  arrange(year, month, day, dep_time)

flights |>
  arrange(desc(dep_delay))

flights |>
  distinct()

flights |>
  distinct(origin, dest)

flights |>
  distinct(origin, dest, .keep_all = TRUE)

flights |>
  count(origin, dest, sort = TRUE)

# exercises
# 1
flights |>
  filter(arr_delay >= 120)

flights |>
  filter(dest %in% c("IAH", "HOU")) -> houston

flights |>
  filter(carrier %in% c("UA", "DL", "AA"))

flights |>
  filter(month %in% c(7, 8, 9))

flights |>
  filter(arr_delay > 120 & dep_delay <= 0)

flights |>
  filter(dep_delay >= 60 & arr_delay <= -30)

# 2
flights |>
  arrange(desc(dep_delay), dep_time) -> long_delay
