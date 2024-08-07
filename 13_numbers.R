library(tidyverse)
library(nycflights13)

# 13.2
x <- c("1.2", "5.6", "1e3")
parse_double(x)

x <- c("$1,234", "USD 3,513", "59%")
parse_number(x)

# 13.3
flights |> count(dest)
flights |> count(dest, sort = TRUE)

flights |>
  group_by(dest) |>
  summarize(n = n(), delay = mean(arr_delay, na.rm = TRUE))

flights |>
  group_by(dest) |>
  summarize(carriers = n_distinct(carrier)) |>
  arrange(desc(carriers))

flights |>
  group_by(tailnum) |>
  summarize(miles = sum(distance))

flights |> count(tailnum, wt = distance, sort = TRUE)

flights |>
  group_by(dest) |>
  summarize(n_cancelled = sum(is.na(dep_time)))

# exercises

# 1
flights |>
  count(is.na(dep_time))

# 2
flights |> count(dest, sort = TRUE)
flights |>
  group_by(dest) |>
  summarise(n = n()) |>
  arrange(desc(n))

flights |> count(tailnum, wt = distance)
flights |>
  group_by(dest) |>
  summarise(total_distance = sum(distance))


# 13.4
x <- c(1, 2, 10, 20)
x / 5

x * c(1, 2)
x * c(1, 2, 3)

flights |>
  filter(month == c(1, 2))

df <- tribble(~ x, ~ y, 1, 3, 5, 2, 7, NA, )

df |>
  mutate(min = pmin(x, y, na.rm = TRUE),
         max = pmax(x, y, na.rm = TRUE))

df |>
  mutate(min = min(x, y, na.rm = TRUE),
         max = max(x, y, na.rm = TRUE))

1:10 %/% 3
1:10 %% 3

flights |>
  mutate(hour = sched_dep_time %/% 100,
         minute = sched_dep_time %% 100,
         .keep = "used")

flights |>
  group_by(hour = sched_dep_time %/% 100) |>
  summarize(prop_cancelled = mean(is.na(dep_time)), n = n()) |>
  filter(hour > 1) |>
  ggplot(aes(x = hour, y = prop_cancelled)) +
  geom_line(color = "grey50") +
  geom_point(aes(size = n))

round(123.456, 2)
round(123.456, -1)
round(c(1.5, 2.5))

x <- 123.456
floor(x)
ceiling(x)

floor(x / 0.01) * 0.01

# Round to nearest multiple of 4
round(x / 4) * 4

x <- c(1, 2, 5, 10, 15, 20)
cut(x, breaks = c(0, 5, 10, 15, 20))

cut(x,
    breaks = c(0, 5, 10, 15, 20),
    labels = c("sm", "md", "lg", "xl"))

y <- c(NA, -10, 5, 10, 30)
cut(y, breaks = c(0, 5, 10, 15, 20))

x <- 1:10
cumsum(x)

# exercises

# 2
sin(pi / 2)

# 3
flights |>
  filter(month == 1, day == 1) |>
  ggplot(aes(x = sched_dep_time, y = dep_delay)) +
  geom_point()

flights |>
  mutate(
    new_sched_dep_time = (sched_dep_time %/% 100) * 60 + sched_dep_time %% 100,
    .keep = "used"
  ) -> new_time

# 4
new_time |>
  mutate(new_sched_dep_time = round(new_sched_dep_time / 5) * 5)

# 13.5
x <- c(1, 2, 2, 3, 4, NA)
min_rank(x)
min_rank(desc(x))

df <- tibble(x = x)
df |>
  mutate(
    row_number = row_number(x),
    dense_rank = dense_rank(x),
    percent_rank = percent_rank(x),
    cume_dist = cume_dist(x)
  )

df <- tibble(id = 1:10)

df |>
  mutate(
    row0 = row_number() - 1,
    three_groups = row0 %% 3,
    three_in_each_group = row0 %/% 3
  )

x <- c(2, 5, 11, 11, 19, 35)
lag(x)
lead(x)

events <- tibble(time = c(0, 1, 2, 3, 5, 10, 12, 15, 17, 19, 20, 27, 28, 30))

events <- events |>
  mutate(diff = time - lag(time, default = first(time)),
         has_gap = diff >= 5)
events

events |> mutate(group = cumsum(has_gap))

df <- tibble(
  x = c("a", "a", "a", "b", "c", "c", "d", "e", "a", "a", "b", "b"),
  y = c(1, 2, 3, 2, 4, 1, 3, 9, 4, 8, 10, 199)
)

df |>
  group_by(id = consecutive_id(x)) |>
  slice_head(n = 1)

# exercises

# 1
flights |>
  mutate(
    rank = min_rank(dep_delay),
    rank2 = row_number(dep_delay),
    .keep = "used"
  ) |>
  arrange(rank)

# 2
flights |>
  mutate(rank = min_rank(dep_delay)) |>
  group_by(tailnum) |>
  summarise(mean_rank = mean(rank)) |>
  arrange(desc(mean_rank))

# 3
flights |>
  mutate(rank = min_rank(dep_delay)) |>
  group_by(hour) |>
  summarise(mean_rank = mean(rank, na.rm = TRUE)) |>
  arrange(mean_rank)

# 4
flights |> group_by(dest) |> filter(row_number() < 4)
flights |> group_by(dest) |> filter(row_number(dep_delay) < 4)

# 5
flights |>
  group_by(dest) |>
  summarise(total_delay = sum(dep_delay, na.rm = TRUE)) |>
  arrange(desc(total_delay))

flights |>
  group_by(dest) |>
  filter(n() > 100) |>
  mutate(
    total_delay = sum(dep_delay, na.rm = TRUE),
    prop_of_delay = abs(dep_delay / total_delay)
  ) |>
  select(dest, dep_delay, total_delay, prop_of_delay) |>
  arrange(desc(prop_of_delay))

# 6
fl <- flights |>
  mutate(hour = dep_time %/% 100) |>
  group_by(year, month, day, hour) |>
  summarize(
    dep_delay = mean(dep_delay, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  filter(n > 5)

fl |>
  group_by(year, month, day) |>
  mutate(lag = lag(dep_delay), diff = dep_delay - lag) |>
  ungroup() -> fl2

fl2 |>
  group_by(hour) |>
  summarise(mean_diff = mean(diff, na.rm = TRUE)) |>
  ggplot(aes(x = hour, y = mean_diff)) +
  geom_col()

# 7
fast <- flights |>
  filter(air_time < 22) |>
  arrange(air_time)

air_delayed <- flights |>
  filter(!is.na(arr_time), !is.na(dep_time)) |>
  mutate(air_delay = arr_time - dep_time) |>
  arrange(desc(air_delay)) |>
  slice_head(n = 10)

# 8
fl3 <- flights |>
  group_by(dest) |>
  filter(n_distinct(carrier) >= 2) |>
  ungroup()

carriers <- fl3 |>
  group_by(carrier, dest) |>
  summarise(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

car <- carriers |>
  group_by(dest) |>
  mutate(ranking = min_rank(avg_delay))

final <- car |>
  group_by(carrier) |>
  summarise(avg_rank = mean(ranking, na.rm = TRUE))

# 13.6
flights |>
  group_by(year, month, day) |>
  summarize(
    mean = mean(dep_delay, na.rm = TRUE),
    median = median(dep_delay, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  ggplot(aes(x = mean, y = median)) +
  geom_abline(
    slope = 1,
    intercept = 0,
    color = "white",
    linewidth = 2
  ) +
  geom_point()

flights |>
  group_by(year, month, day) |>
  summarize(
    max = max(dep_delay, na.rm = TRUE),
    q95 = quantile(dep_delay, 0.95, na.rm = TRUE),
    .groups = "drop"
  )

flights |>
  group_by(origin, dest) |>
  summarize(distance_iqr = IQR(distance),
            n = n(),
            .groups = "drop") |>
  filter(distance_iqr > 0)

flights |>
  filter(dep_delay < 120) |>
  ggplot(aes(x = dep_delay, group = interaction(day, month))) +
  geom_freqpoly(binwidth = 5, alpha = 1 / 5)

flights |>
  group_by(year, month, day) |>
  summarize(
    first_dep = first(dep_time, na_rm = TRUE),
    fifth_dep = nth(dep_time, 5, na_rm = TRUE),
    last_dep = last(dep_time, na_rm = TRUE)
  )

flights |>
  group_by(year, month, day) |>
  mutate(r = min_rank(sched_dep_time)) |>
  filter(r %in% c(1, max(r)))

# exercises

# 1
flights |>
  group_by(dest) |>
  summarise(
    mean = mean(dep_delay, na.rm = TRUE),
    median = median(dep_delay, na.rm = TRUE),
    q01 = quantile(dep_delay, 0.01, na.rm = TRUE),
    q99 = quantile(dep_delay, 0.99, na.rm = TRUE),
  ) |>
  ggplot(aes(x = mean)) +
  geom_boxplot()

# 2
flights |>
  group_by(dest) |>
  summarise(velocity_sd = sd(distance / air_time, na.rm = TRUE)) |>
  arrange(desc(velocity_sd))

# 3
flights |>
  group_by(origin, year, month) |>
  filter(dest == "EGE") |>
  summarise(avg_dist = mean(distance))
