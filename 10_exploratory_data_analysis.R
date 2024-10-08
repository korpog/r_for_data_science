library(tidyverse)

# 10.3
ggplot(diamonds, aes(x = carat)) +
  geom_histogram(binwidth = 0.5)

smaller <- diamonds |>
  filter(carat < 3)

ggplot(smaller, aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

ggplot(diamonds, aes(x = y)) +
  geom_histogram(binwidth = 0.5)

ggplot(diamonds, aes(x = y)) +
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

unusual <- diamonds |>
  filter(y < 3 | y > 20) |>
  select(price, x, y, z) |>
  arrange(y)
unusual

# exercises
ggplot(diamonds, aes(x = x)) +
  geom_histogram(binwidth = 0.5)

ggplot(diamonds, aes(x = y)) +
  geom_histogram(binwidth = 0.5)

ggplot(diamonds, aes(x = z)) +
  geom_histogram(binwidth = 0.5)

ggplot(diamonds, aes(x = price)) +
  geom_histogram(binwidth = 200)

carat99 <- diamonds |>
  filter(carat == 0.99)

carat1 <- diamonds |>
  filter(carat == 1)

ggplot(diamonds, aes(x = y)) +
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

ggplot(diamonds, aes(x = y)) +
  ylim(0, 25) +
  geom_histogram(binwidth = 0.5)

# 10.4
diamonds2 <- diamonds |>
  filter(between(y, 3, 20))

diamonds2 <- diamonds |>
  mutate(y = if_else(y < 3 | y > 20, NA, y))

ggplot(diamonds2, aes(x = x, y = y)) +
  geom_point(na.rm = TRUE)

nycflights13::flights |>
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + (sched_min / 60)
  ) |>
  ggplot(aes(x = sched_dep_time)) +
  geom_freqpoly(aes(color = cancelled), binwidth = 1 / 4)

# exercises
ggplot(diamonds, aes(x = price)) +
  geom_histogram(binwidth = 200, na.rm = FALSE)

mean(diamonds$carat, na.rm = FALSE)

nycflights13::flights |>
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + (sched_min / 60)
  ) |>
  ggplot(aes(x = sched_dep_time)) +
  geom_freqpoly(aes(color = cancelled), binwidth = 1 / 4) +
  facet_wrap(~cancelled, scales = "free")

# 10.5.1
ggplot(diamonds, aes(x = price)) +
  geom_freqpoly(aes(color = cut), binwidth = 500, linewidth = 0.75)

ggplot(diamonds, aes(x = price, y = after_stat(density))) +
  geom_freqpoly(aes(color = cut), binwidth = 500, linewidth = 0.75)

ggplot(diamonds, aes(x = cut, y = price)) +
  geom_boxplot()

ggplot(mpg, aes(x = class, y = hwy)) +
  geom_boxplot()

ggplot(mpg, aes(x = fct_reorder(class, hwy, median), y = hwy)) +
  geom_boxplot()

ggplot(mpg, aes(x = hwy, y = fct_reorder(class, hwy, median))) +
  geom_boxplot()

# exercises

# 1
nycflights13::flights |>
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + (sched_min / 60)
  ) |>
  ggplot(aes(x = sched_dep_time, y = after_stat(density))) +
  geom_freqpoly(aes(color = cancelled))

# 2
library(corrr)
cor(diamonds, x = )

ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

ggplot(diamonds, aes(x = carat, y = price, color = cut)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm")

corr_df <- diamonds |>
  select(where(is.numeric)) |>
  correlate()

# 3
ggplot(diamonds, aes(x = cut, y = carat)) +
  geom_boxplot() +
  coord_flip()

# 4
library(lvplot)
ggplot(diamonds, aes(x = cut, y = price)) +
  geom_lv()

# 5
ggplot(diamonds, aes(x = color, y = price)) +
  geom_violin()

ggplot(diamonds, aes(x = price)) +
  geom_histogram() +
  facet_wrap(~color)

ggplot(diamonds, aes(x = price, )) +
  geom_freqpoly(aes(color = color), linewidth = 0.75)

ggplot(diamonds, aes(price, fill = color)) +
  geom_density()

# 6
library(ggbeeswarm)

ggplot(diamonds, aes(x = price, y = cut)) +
  geom_quasirandom(width = .1)

# 10.5.2
ggplot(diamonds, aes(x = cut, y = color)) +
  geom_count()

diamonds |>
  count(color, cut) |>
  ggplot(aes(x = color, y = cut)) +
  geom_tile(aes(fill = n))

# exercises

# 1
diamonds |>
  count(color, cut) |>
  group_by(color) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(x = color, y = cut)) +
  geom_tile(aes(fill = prop))

# 2
diamonds |>
  count(color, cut) |>
  ggplot(aes(x = color, y = n, fill = cut)) +
  geom_bar(stat = "identity")

# 3
library(nycflights13)

flights |>
  group_by(dest, month) |>
  summarise(avg_delay = mean(dep_delay, na.rm = TRUE)) |>
  ggplot(aes(x = month, y = dest, fill = avg_delay)) +
  geom_tile()

flights |>
  group_by(dest) |>
  filter(n() > 365) |>
  ungroup() |>
  mutate(month = factor(month.abb[month], levels = month.abb)) |>
  group_by(dest, month) |>
  summarise(avg_delay = mean(dep_delay, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = month, y = reorder(dest, avg_delay), fill = avg_delay)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(
    title = "Average Departure Delays by Destination and Month",
    x = "Month", y = "Destination", fill = "Avg Delay (min)"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))

# 10.5.3
smaller <- diamonds |>
  filter(carat < 3)

ggplot(smaller, aes(x = carat, y = price)) +
  geom_point()

ggplot(smaller, aes(x = carat, y = price)) +
  geom_point(alpha = .1)

library(hexbin)

ggplot(smaller, aes(x = carat, y = price)) +
  geom_bin2d()

ggplot(smaller, aes(x = carat, y = price)) +
  geom_hex()

ggplot(smaller, aes(x = carat, y = price)) +
  geom_boxplot(aes(group = cut_width(carat, 0.1, varwidth = TRUE)))

# exercises

# 1
ggplot(smaller, aes(x = carat, y = price)) +
  geom_boxplot(aes(group = cut_number(carat, n = 20)))

# 2
ggplot(smaller, aes(x = price, y = carat)) +
  geom_boxplot(aes(group = cut_width(price, 500)))

# 3
bigger <- diamonds |>
  filter(carat >= 3)

ggplot(smaller, aes(x = carat, y = price)) +
  geom_bin2d()

ggplot(bigger, aes(x = carat, y = price)) +
  geom_bin2d()

ggplot(bigger, aes(x = carat, y = price)) +
  geom_boxplot(aes(group = cut_width(carat, 0.1, varwidth = TRUE)))

# 4
ggplot(smaller, aes(x = carat, y = price)) +
  geom_bin2d() +
  facet_wrap(~cut) +
  labs(
    title = "Distribution of Carat, Price, and Cut",
    x = "Carat",
    y = "Price",
    color = "Cut"
  ) +
  theme_minimal()

# 5
diamonds |>
  filter(x >= 4) |>
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))

diamonds |>
  filter(x >= 4) |>
  ggplot(aes(x = x, y = y)) +
  geom_bin2d() +
  coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))

# 6
ggplot(smaller, aes(x = carat, y = price)) +
  geom_boxplot(aes(group = cut_number(carat, 20)))

# 10.6
library(tidymodels)

diamonds <- diamonds |>
  mutate(
    log_price = log(price),
    log_carat = log(carat)
  )

diamonds_fit <- linear_reg() |>
  fit(log_price ~ log_carat, data = diamonds)

diamonds_aug <- augment(diamonds_fit, new_data = diamonds) |>
  mutate(.resid = exp(.resid))

ggplot(diamonds_aug, aes(x = carat, y = .resid)) +
  geom_point()

ggplot(diamonds_aug, aes(x = cut, y = .resid)) +
  geom_boxplot()
