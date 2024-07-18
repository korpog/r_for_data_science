library(nycflights13)
library(tidyverse)

# 3.1
? flights
View(flights)

flights |>
  filter(dest == "IAH") |>
  group_by(year, month, day) |>
  summarize(arr_delay = mean(arr_delay, na.rm = TRUE))

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

# 3
flights |>
  arrange(desc(distance / air_time)) -> fast_flights

# 4
flights |>
  filter(year == 2013) |>
  distinct(day, month) -> every_day

# 5
flights |>
  arrange(desc(distance)) -> farthest

head(farthest$distance, 1)
tail(farthest$distance, 1)

# 3.3
flights |>
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    .before = 1
  )

flights |>
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time / 60,
    gain_per_hour = gain / hours,
    .keep = "used"
  )

flights |>
  select(year, month, day)

flights |>
  select(year:day)

flights |>
  select(!year:day)

flights |>
  select(where(is.character))

flights |>
  select(tail_num = tailnum)

flights |>
  rename(tail_num = tailnum)

flights |>
  relocate(time_hour, air_time)

flights |>
  relocate(starts_with("arr"), .before = dep_time)

# exercises

# 1
flights |>
  select(dep_time, sched_dep_time, dep_delay)

# 2
flights |>
  select(dep_time, dep_delay, arr_time, arr_delay)

flights |>
  select(starts_with("dep") | starts_with("arr"))

# 3
flights |>
  select(dep_time, dep_time)

# 4
variables <- c("year", "month", "day", "dep_delay", "arr_delay")
flights |>
  select(any_of(variables))

# 5
flights |> select(contains("TIME", ignore.case = FALSE))

# 6
flights |>
  rename(air_time_min = air_time) |>
  relocate(air_time_min)

# 3.4
flights |>
  filter(dest == "IAH") |>
  mutate(speed = distance / air_time * 60) |>
  select(year:day, dep_time, carrier, flight, speed) |>
  arrange(desc(speed))

# 3.5
flights |>
  group_by(month)

flights |>
  group_by(month) |>
  summarize(avg_delay = mean(dep_delay, na.rm = TRUE), n = n())

flights |>
  group_by(dest) |>
  slice_max(arr_delay, n = 1, with_ties = FALSE) |>
  relocate(dest)

daily <- flights |>
  group_by(year, month, day)
daily

daily_flights <- daily |>
  summarize(n = n(), .groups = "drop_last")

daily |>
  ungroup()

daily |>
  ungroup() |>
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    flights = n()
  )

flights |>
  summarize(
    delay = mean(dep_delay, na.rm = TRUE),
    n = n(),
    .by = month
  )

flights |>
  summarize(
    delay = mean(dep_delay, na.rm = TRUE),
    n = n(),
    .by = c(origin, dest)
  )

# exercises
# 1
flights |>
  group_by(carrier, dest) |>
  summarize(n())

flights |>
  group_by(carrier) |>
  summarize(
    avg_dep_delay = mean(dep_delay, na.rm = T),
    avg_arr_delay = mean(arr_delay, na.rm = T),
    total_flights = n()
  ) |>
  arrange(desc(avg_dep_delay), desc(avg_arr_delay))

flights |>
  group_by(carrier, dest) |>
  summarize(
    avg_arr_delay = mean(arr_delay, na.rm = TRUE),
    total_flights = n()
  ) |>
  filter(total_flights > 30) |>
  arrange(desc(avg_arr_delay))

# 2
flights |>
  group_by(dest) |>
  slice_max(dep_delay, n = 1, na_rm = T) |>
  relocate(dest) |>
  select(dest, year:day, dep_delay)

# 3
flights |>
  group_by(hour = hour(time_hour)) |>
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE)) -> delays

ggplot(delays, aes(x = hour)) +
  geom_line(aes(y = avg_delay, color = "Arrival delay"))

# 4
flights |>
  group_by(dest) |>
  slice_min(arr_delay, n = -1)

# 6
df <- tibble(
  x = 1:5,
  y = c("a", "b", "a", "a", "b"),
  z = c("K", "K", "L", "L", "K")
)

df |>
  group_by(y)

df |>
  arrange(y)

df |>
  group_by(y) |>
  summarize(mean_x = mean(x))

df |>
  group_by(y, z) |>
  summarize(mean_x = mean(x))

df |>
  group_by(y, z) |>
  summarize(mean_x = mean(x), .groups = "drop")

df |>
  group_by(y, z) |>
  summarize(mean_x = mean(x))

df |>
  group_by(y, z) |>
  mutate(mean_x = mean(x))

# 3.6
batters <- Lahman::Batting |>
  group_by(playerID) |>
  summarize(
    performance = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    n = sum(AB, na.rm = TRUE)
  )
batters

batters |>
  filter(n > 100) |>
  ggplot(aes(x = n, y = performance)) +
  geom_point(alpha = 1 / 10) +
  geom_smooth(se = FALSE)
