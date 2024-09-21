library(tidyverse)
library(nycflights13)

# 19.2
planes |> 
  count(tailnum) |> 
  filter(n > 1)

weather |> 
  count(time_hour, origin) |> 
  filter(n > 1)

planes |> 
  filter(is.na(tailnum))

weather |> 
  filter(is.na(time_hour) | is.na(origin))

flights2 <- flights |> 
  mutate(id = row_number(), .before = 1)

# exercises

# 1
weather
airports

# 3
weather |> 
  count(year, month, day, hour, origin) |> 
  filter(n > 1)

# 19.3
flights2 <- flights |> 
  select(year, time_hour, origin, dest, tailnum, carrier)

flights2 |>
  left_join(airlines)

flights2 |> 
  left_join(weather |> select(origin, time_hour, temp, wind_speed))

flights2 |> 
  left_join(planes |> select(tailnum, type, engines, seats))

flights2 |> 
  left_join(planes, join_by(tailnum))

flights2 |> 
  left_join(airports, join_by(dest == faa))


airports |> 
  semi_join(flights2, join_by(faa == origin))

airports |> 
  semi_join(flights2, join_by(faa == dest))

flights2 |> 
  anti_join(airports, join_by(dest == faa)) |> 
  distinct(dest)

flights2 |>
  anti_join(planes, join_by(tailnum)) |> 
  distinct(tailnum)
