library(tidyverse)
library(palmerpenguins)
library(ggthemes)

# 2.1
x <- 3 * 4
primes <- c(2, 3, 5, 7, 11, 13)
primes * 2

# 2.2
# create vector of primes
primes <- c(2, 3, 5, 7, 11, 13)

# multiply primes by 2
primes * 2

# 2.5
seq(1, 10)

# exercises
library(tidyverse)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(method = "lm")

my_bar_plot <- ggplot(mpg, aes(x = class)) +
  geom_bar()
my_scatter_plot <- ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point()
ggsave(filename = "mpg-plot.png", plot = my_bar_plot)
