library(tidyverse)

# 9.2
mpg
ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy, shape = class)) +
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy, size = class)) +
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy, alpha = class)) +
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(color = "blue", shape = 9)

# exercises
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(color = "pink", shape = 24)

ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy), color = "blue")

ggplot(mpg, aes(x = displ, y = hwy, color = displ < 5)) +
  geom_point()

# 9.3
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_smooth()

ggplot(mpg, aes(x = displ, y = hwy, shape = drv)) +
  geom_smooth()

ggplot(mpg, aes(x = displ, y = hwy, linetype = drv)) +
  geom_smooth()

ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  geom_smooth(aes(linetype = drv))

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_smooth()

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_smooth(aes(group = drv))

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_smooth(aes(color = drv), show.legend = TRUE)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth()

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_point(data = mpg |> filter(class == "2seater"),
             color = "red") +
  geom_point(
    data = mpg |> filter(class == "2seater"),
    shape = "circle open",
    size = 3,
    color = "red"
  )

ggplot(mpg, aes(x = hwy)) +
  geom_histogram(binwidth = 2)

ggplot(mpg, aes(x = hwy)) +
  geom_density()

ggplot(mpg, aes(x = hwy)) +
  geom_boxplot()

library(ggridges)
ggplot(mpg, aes(
  x = hwy,
  y = drv,
  fill = drv,
  color = drv
)) +
  geom_density_ridges(alpha = 0.5, show.legend = FALSE)

# exercises
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_area(aes(color = drv))

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(aes(group = drv), method = "loess", se = F)

ggplot(mpg, aes(x = displ, y = hwy), ) +
  geom_point(aes(color = drv)) +
  geom_smooth(
    aes(group = drv, color = drv),
    method = "loess",
    se = F,
    show.legend = T
  )

# 9.4
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_wrap(~ cyl)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_grid(drv ~ cyl)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_grid(drv ~ cyl, scales = "free")

# exercises
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_wrap(~ cty)

ggplot(mpg) +
  geom_point(aes(x = drv, y = cyl))

ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)

ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy)) +
  facet_wrap(~ cyl, nrow = 2)

ggplot(mpg, aes(x = displ)) +
  geom_histogram() +
  facet_grid(drv ~ .)

ggplot(mpg, aes(x = displ)) +
  geom_histogram() +
  facet_grid(. ~ drv)

ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy)) +
  facet_wrap(~ drv, nrow = 3)

# 9.5
ggplot(diamonds, aes(x = cut)) +
  geom_bar()

diamonds |>
  count(cut) |>
  ggplot(aes(x = cut, y = n)) +
  geom_bar(stat = "identity")

ggplot(diamonds, aes(x = cut, y = after_stat(prop), group = 1)) +
  geom_bar()

ggplot(diamonds) +
  stat_summary(
    aes(x = cut, y = depth),
    fun.min = min,
    fun.max = max,
    fun = median
  )

# exercises
ggplot(diamonds) +
  geom_pointrange(
    aes(x = cut, y = depth),
    stat = "summary",
    fun.min = min,
    fun.max = max,
    fun = median
  )

ggplot(diamonds, aes(x = cut)) +
  geom_bar()

ggplot(diamonds, aes(x = cut, y = carat)) +
  geom_col()

ggplot(diamonds) +
  stat_smooth(aes(x = carat, y = price), method = "loess", se = F)

ggplot(diamonds, aes(x = cut, y = after_stat(prop), group = 1)) +
  geom_bar()
ggplot(diamonds, aes(
  x = cut,
  fill = color,
  y = after_stat(prop),
  group = 1
)) +
  geom_bar()

# 9.6
ggplot(mpg, aes(x = drv, color = drv)) +
  geom_bar()

ggplot(mpg, aes(x = drv, fill = drv)) +
  geom_bar()

ggplot(mpg, aes(x = drv, fill = class)) +
  geom_bar()

ggplot(mpg, aes(x = drv, fill = class)) +
  geom_bar(alpha = 1 / 5, position = "identity")

ggplot(mpg, aes(x = drv, color = class)) +
  geom_bar(fill = NA, position = "identity")

ggplot(mpg, aes(x = drv, fill = class)) +
  geom_bar(position = "fill")

ggplot(mpg, aes(x = drv, fill = class)) +
  geom_bar(position = "dodge")

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(position = "jitter")

# exercises
ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_jitter(width = 0.3, height = 0.3)

ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_count()

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point()
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(position = "identity")

mpg |>
  ggplot(aes(x = hwy)) +
  geom_boxplot(position = "dodge")

# 9.7
nz <- map_data("nz")

ggplot(nz, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", color = "black")

ggplot(nz, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", color = "black") +
  coord_quickmap()

bar <- ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = clarity, fill = clarity),
    show.legend = FALSE,
    width = 1
  ) +
  theme(aspect.ratio = 1)

bar + coord_flip()
bar + coord_polar()

# exercises
bar2 <- ggplot(mpg, aes(x = drv, fill = drv)) +
  geom_bar()

bar2 + coord_polar()

ggplot(nz, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", color = "black") +
  coord_map()

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() +
  geom_abline() +
  coord_fixed()
