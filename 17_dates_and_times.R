library(tidyverse)
library(nycflights13)

# 17.2
today()
now()

csv <- "
  date,datetime
  2022-01-02,2022-01-02 05:12
"
read_csv(csv)

csv <- "
  date
  01/02/15
"
read_csv(csv, col_types = cols(date = col_date("%m/%d/%y")))
read_csv(csv, col_types = cols(date = col_date("%d/%m/%y")))
read_csv(csv, col_types = cols(date = col_date("%y/%m/%d")))

ymd("2017-01-31")
mdy("January 31st, 2017")
dmy("31-Jan-2017")

ymd_hms("2017-01-31 20:11:59")
mdy_hm("01/31/2017 08:01")

ymd("2017-01-31", tz = "UTC")

flights |>
  select(year, month, day, hour, minute)

flights |>
  select(year, month, day, hour, minute) |>
  mutate(departure = make_datetime(year, month, day, hour, minute))

make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights |>
  filter(!is.na(dep_time), !is.na(arr_time)) |>
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) |>
  select(origin, dest, ends_with("delay"), ends_with("time"))

flights_dt |>
  ggplot(aes(x = dep_time)) +
  geom_freqpoly(binwidth = 86400) # 86400 seconds = 1 day

flights_dt |>
  filter(dep_time < ymd(20130102)) |>
  ggplot(aes(x = dep_time)) +
  geom_freqpoly(binwidth = 600) # 600 s = 10 minutes

as_datetime(today())
as_date(now())

as_datetime(60 * 60 * 10)
as_date(365 * 10 + 2)

# exercises

# 1
ymd(c("2010-10-10", "bananas"))

# 2
today(tzone = "UTC")

# 3
d1 <- "January 1, 2010"
# %B %d, %Y
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14" # Dec 30, 2014
t1 <- "1705"
t2 <- "11:15:10.12 PM"

# 17.3
datetime <- ymd_hms("2026-07-08 12:34:56")

year(datetime)
month(datetime)

mday(datetime)
yday(datetime)
wday(datetime)

month(datetime, label = TRUE)
wday(datetime, label = TRUE, abbr = FALSE)

make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights |>
  filter(!is.na(dep_time), !is.na(arr_time)) |>
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) |>
  select(origin, dest, ends_with("delay"), ends_with("time"))

flights_dt |>
  mutate(wday = wday(dep_time, label = TRUE)) |>
  ggplot(aes(x = wday)) +
  geom_bar()

flights_dt |>
  mutate(minute = minute(dep_time)) |>
  group_by(minute) |>
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    n = n()
  ) |>
  ggplot(aes(x = minute, y = avg_delay)) +
  geom_line()

sched_dep <- flights_dt |>
  mutate(minute = minute(sched_dep_time)) |>
  group_by(minute) |>
  summarize(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )

ggplot(sched_dep, aes(x = minute, y = avg_delay)) +
  geom_line()

ggplot(sched_dep, aes(x = minute, y = n)) +
  geom_line()

flights_dt |>
  count(week = floor_date(dep_time, "week")) |>
  ggplot(aes(x = week, y = n)) +
  geom_line() +
  geom_point()

flights_dt |>
  mutate(dep_hour = dep_time - floor_date(dep_time, "day")) |>
  ggplot(aes(x = dep_hour)) +
  geom_freqpoly(binwidth = 60 * 30)

flights_dt |>
  mutate(dep_hour = hms::as_hms(dep_time - floor_date(dep_time, "day"))) |>
  ggplot(aes(x = dep_hour)) +
  geom_freqpoly(binwidth = 60 * 30)

datetime <- ymd_hms("2026-07-08 12:34:56")

year(datetime) <- 2030
month(datetime) <- 01
hour(datetime) <- hour(datetime) + 1
datetime

update(datetime, year = 2030, month = 2, mday = 2, hour = 2)

update(ymd("2023-02-01"), mday = 30)
update(ymd("2023-02-01"), hour = 400)

# exercises

# 1
flights_dt |>
  mutate(
    dep_hour = hms::as_hms(dep_time - floor_date(dep_time, "day")),
    month = month(dep_time)
  ) |>
  ggplot(aes(x = dep_hour)) +
  geom_freqpoly(binwidth = 60 * 30) +
  facet_wrap(~month)

# 2
flights_comp <- flights_dt |>
  mutate(
    diff_delay = dep_time - sched_dep_time,
    consistent = if_else(dep_delay == diff_delay, TRUE, FALSE)
  )

# 3
air_time <- flights_dt |>
  mutate(
    diff_time = arr_time - dep_time,
    consistent = if_else(dep_delay == diff_time, TRUE, FALSE)
  )

# 5
flights_dt |>
  group_by(wday(dep_time, label = TRUE)) |>
  summarise(
    avg_delay = mean(dep_delay, na.rm = TRUE)
  )

# 17.4
h_age <- today() - ymd("1979-10-14")
as.duration(h_age)

dseconds(15)
dminutes(10)
dhours(c(12, 24))
ddays(0:5)
dweeks(3)
dyears(1)

2 * dyears(1)
dyears(1) + dweeks(12) + dhours(15)

tomorrow <- today() + ddays(1)
last_year <- today() - dyears(1)

one_am <- ymd_hms("2026-03-08 01:00:00", tz = "America/New_York")
one_am + ddays(1)
one_am + days(1)

hours(c(12, 24))
days(7)
months(1:6)

10 * (months(6) + days(1))
days(50) + hours(25) + minutes(2)

# A leap year
ymd("2024-01-01") + dyears(1)
ymd("2024-01-01") + years(1)

# Daylight saving time
one_am + ddays(1)
one_am + days(1)

flights_dt |>
  filter(arr_time < dep_time)

flights_dt <- flights_dt |>
  mutate(
    overnight = arr_time < dep_time,
    arr_time = arr_time + days(overnight),
    sched_arr_time = sched_arr_time + days(overnight)
  )

flights_dt |>
  filter(arr_time < dep_time)

years(1) / days(1)

y2023 <- ymd("2023-01-01") %--% ymd("2024-01-01")
y2024 <- ymd("2024-01-01") %--% ymd("2025-01-01")

y2023 / days(1)
y2024 / days(1)

# exercises

# 2
dates_2015 <- ymd("2015-01-01") + months(0:11)

dates_current <- ymd("2024-01-01") + months(0:11)

# 3
my_bday <- ymd("1992-01-18")
age <- today() - my_bday
as.duration(age)

# 4
(today() %--% (today() + years(1))) / months(1)

# 17.5
Sys.timezone()
length(OlsonNames())

x1 <- ymd_hms("2024-06-01 12:00:00", tz = "America/New_York")
x2 <- ymd_hms("2024-06-01 18:00:00", tz = "Europe/Copenhagen")
x3 <- ymd_hms("2024-06-02 04:00:00", tz = "Pacific/Auckland")
x1 - x2

x4 <- c(x1, x2, x3)
x4

x4a <- with_tz(x4, tzone = "Australia/Lord_Howe")
x4a

x4b <- force_tz(x4, tzone = "Australia/Lord_Howe")
x4b
