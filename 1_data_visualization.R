library(tidyverse)
library(palmerpenguins)
library(ggthemes)

View(penguins)
?penguins

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
)