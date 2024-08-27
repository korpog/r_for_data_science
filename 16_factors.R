library(tidyverse)

# 16.2
x1 <- c("Dec", "Apr", "Jan", "Mar")
x2 <- c("Dec", "Apr", "Jam", "Mar")

month_levels <- c(
  "Jan",
  "Feb",
  "Mar",
  "Apr",
  "May",
  "Jun",
  "Jul",
  "Aug",
  "Sep",
  "Oct",
  "Nov",
  "Dec"
)

y1 <- factor(x1, levels = month_levels)
sort(y1)

y2 <- factor(x2, levels = month_levels)
y2 <- fct(x2, levels = month_levels)

factor(x1)
fct(x1)

levels(y2)

csv <- "
month,value
Jan,12
Feb,56
Mar,12"

df <- read_csv(csv, col_types = cols(month = col_factor(month_levels)))
df$month

# 16.3
gss_cat

gss_cat |>
  count(race)

# exercises

# 1
inc <- gss_cat |>
  count(rincome)

gss_cat |>
  ggplot(aes(y = rincome)) +
  geom_bar()

# 2
gss_cat |>
  count(relig) |>
  arrange(desc(n))

gss_cat |>
  count(partyid) |>
  arrange(desc(n))

# 3
gss_cat |>
  count(denom)

gss_cat |>
  group_by(relig, denom) |>
  summarise(n = n()) |>
  arrange(desc(n)) |>
  ggplot(aes(x = relig, y = denom, fill = n)) +
  geom_tile() +
  scale_fill_viridis_b()

# 16.4
relig_summary <- gss_cat |>
  group_by(relig) |>
  summarize(tvhours = mean(tvhours, na.rm = TRUE), n = n())

ggplot(relig_summary, aes(x = tvhours, y = relig)) +
  geom_point()

ggplot(relig_summary, aes(x = tvhours, y = fct_reorder(relig, tvhours))) +
  geom_point()

relig_summary |>
  mutate(relig = fct_reorder(relig, tvhours)) |>
  ggplot(aes(x = tvhours, y = relig)) +
  geom_point()

rincome_summary <- gss_cat |>
  group_by(rincome) |>
  summarize(age = mean(age, na.rm = TRUE), n = n())

ggplot(rincome_summary, aes(x = age, y = fct_reorder(rincome, age))) +
  geom_point()

ggplot(rincome_summary, aes(x = age, y = fct_relevel(rincome, "Not applicable"))) +
  geom_point()

by_age <- gss_cat |>
  filter(!is.na(age)) |>
  count(age, marital) |>
  group_by(age) |>
  mutate(prop = n / sum(n))

ggplot(by_age, aes(x = age, y = prop, color = marital)) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1")

ggplot(by_age, aes(
  x = age,
  y = prop,
  color = fct_reorder2(marital, age, prop)
)) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(color = "marital")

gss_cat |>
  mutate(marital = marital |> fct_infreq() |> fct_rev()) |>
  ggplot(aes(x = marital)) +
  geom_bar()

# exercises

# 1
relig_summary <- gss_cat |>
  group_by(relig) |>
  summarize(tvhours = median(tvhours, na.rm = TRUE), n = n())

ggplot(relig_summary, aes(x = tvhours, y = relig)) +
  geom_point()

# 2
levels(gss_cat$partyid)

# 16.5
gss_cat |> count(partyid)

gss_cat |>
  mutate(
    partyid = fct_recode(
      partyid,
      "Republican, strong"    = "Strong republican",
      "Republican, weak"      = "Not str republican",
      "Independent, near rep" = "Ind,near rep",
      "Independent, near dem" = "Ind,near dem",
      "Democrat, weak"        = "Not str democrat",
      "Democrat, strong"      = "Strong democrat"
    )
  ) |>
  count(partyid)

gss_cat |>
  mutate(partyid = fct_collapse(
    partyid,
    "other" = c("No answer", "Don't know", "Other party"),
    "rep" = c("Strong republican", "Not str republican"),
    "ind" = c("Ind,near rep", "Independent", "Ind,near dem"),
    "dem" = c("Not str democrat", "Strong democrat")
  )) |>
  count(partyid)

gss_cat |>
  mutate(relig = fct_lump_n(relig, n = 10)) |>
  count(relig, sort = TRUE)

# exercises

# 1
gss_cat |>
  mutate(partyid = fct_collapse(
    partyid,
    "other" = c("No answer", "Don't know", "Other party"),
    "rep" = c("Strong republican", "Not str republican"),
    "ind" = c("Ind,near rep", "Independent", "Ind,near dem"),
    "dem" = c("Not str democrat", "Strong democrat")
  )) |>
  filter(partyid != "other") |>
  group_by(year) |>
  count(partyid) |>
  ggplot(aes(x = year, y = partyid, fill = n)) +
  geom_tile()

# 16.6
ordered(c("a", "b", "c"))
