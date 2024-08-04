library(tidyverse)
library(nycflights13)

# 12.2
flights |>
  filter(dep_time > 600 & dep_time < 2000 & abs(arr_delay) < 20)

flights |>
  mutate(
    daytime = dep_time > 600 & dep_time < 2000,
    approx_ontime = abs(arr_delay) < 20,
    .keep = "used"
  )

flights |>
  mutate(daytime = dep_time > 600 & dep_time < 2000,
         approx_ontime = abs(arr_delay) < 20,) |>
  filter(daytime & approx_ontime)

x <- c(1 / 49 * 49, sqrt(2) ^ 2)
x
x == c(1, 2)

print(x, digits = 16)
near(x, c(1, 2))

NA > 5
10 == NA
NA == NA

# We don't know how old Mary is
age_mary <- NA

# We don't know how old John is
age_john <- NA

# Are Mary and John the same age?
age_mary == age_john

flights |>
  filter(dep_time == NA)

is.na(c(TRUE, NA, FALSE))

flights |>
  filter(is.na(dep_time))

flights |>
  filter(month == 1, day == 1) |>
  arrange(dep_time)

flights |>
  filter(month == 1, day == 1) |>
  arrange(desc(is.na(dep_time)), dep_time)

# exercises

# 1
near
near(sqrt(2) ^ 2, 2)

# 2
miss <- flights |>
  mutate(
    na_dep_time = is.na(dep_time),
    na_sched_dep_time = is.na(sched_dep_time),
    na_dep_delay = is.na(dep_delay),
  ) |>
  count(na_dep_time, na_sched_dep_time, na_dep_delay)

# 12.3
df <- tibble(x = c(TRUE, FALSE, NA))

df |>
  mutate(and = x & NA, or = x | NA)

flights |>
  mutate(nov = month == 11,
         final = nov | 12,
         .keep = "used")

flights |>
  filter(month %in% c(11, 12))

c(1, 2, NA) == NA

c(1, 2, NA) %in% NA

flights |>
  filter(dep_time %in% c(NA, 0800))

# exercises

# 1
flights |>
  filter(is.na(arr_delay) & !is.na(dep_delay))

flights |>
  filter(!is.na(arr_time) &
           !is.na(sched_arr_time) & is.na(arr_delay))

# 2
flights |>
  filter(is.na(dep_time))

# 3
fl <- flights |>
  filter(is.na(dep_time)) |>
  group_by(day) |>
  summarise(num = n())

fl2 <- flights |>
  group_by(day) |>
  summarise(
    prop = sum(is.na(dep_time)) / n(),
    avg_del_noncancelled = mean(dep_delay[!is.na(dep_time)], na.rm = TRUE)
  )

ggplot(fl2, aes(x = prop, y = avg_del_noncancelled)) +
  geom_point() +
  geom_smooth(method = "lm")

# 12.4
flights |>
  group_by(year, month, day) |>
  summarize(
    all_delayed = all(dep_delay <= 60, na.rm = TRUE),
    any_long_delay = any(arr_delay >= 300, na.rm = TRUE),
    .groups = "drop"
  )

flights |>
  group_by(year, month, day) |>
  summarize(
    proportion_delayed = mean(dep_delay <= 60, na.rm = TRUE),
    count_long_delay = sum(arr_delay >= 300, na.rm = TRUE),
    .groups = "drop"
  )

flights |>
  filter(arr_delay > 0) |>
  group_by(year, month, day) |>
  summarize(behind = mean(arr_delay),
            n = n(),
            .groups = "drop")

flights |>
  group_by(year, month, day) |>
  summarize(
    behind = mean(arr_delay[arr_delay > 0], na.rm = TRUE),
    ahead = mean(arr_delay[arr_delay < 0], na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# exercises

# 1
ccc <- c(TRUE, NA, FALSE)
sum(is.na(ccc))
mean(is.na(ccc))

# 2
prod(ccc, na.rm = TRUE)
min(ccc, na.rm = T)

# 12.5
x <- c(-3:3, NA)
if_else(x > 0, "+ve", "-ve")
if_else(x > 0, "+ve", "-ve", "???")
if_else(x < 0, -x, x)

x1 <- c(NA, 1, 2, NA)
y1 <- c(3, NA, 4, 6)
if_else(is.na(x1), y1, x1)

if_else(x == 0, "0", if_else(x < 0, "-ve", "+ve"), "???")

x <- c(-3:3, NA)
case_when(x == 0 ~ "0", x < 0 ~ "-ve", x > 0 ~ "+ve", is.na(x) ~ "???")

case_when(x < 0 ~ "-ve", x > 0 ~ "+ve", .default = "???")

flights |>
  mutate(
    status = case_when(
      is.na(arr_delay) ~ "cancelled",
      arr_delay < -30 ~ "very early",
      arr_delay < -15 ~ "early",
      abs(arr_delay) <= 15 ~ "on time",
      arr_delay < 60 ~ "late",
      arr_delay < Inf ~ "very late",
    ),
    .keep = "used"
  )

# exercises

# 1
nums <- c(0:20)
if_else(nums %% 2 == 0, "even", "odd")

# 2
x <- c("Monday", "Saturday", "Wednesday")
ifelse(x %in% c("Saturday", "Sunday"), "weekend", "weekday")

# 3
x <- c(-2, 3, 0, -5)
if_else(x < 0, -x, x)

# 4
flights |>
  mutate(
    holiday = case_when(
      month == 1 & day == 1 ~ "New Year's Day",
      month == 7 & day == 4 ~ "4th of July",
    ),
    .keep = "used"
  ) -> hol
