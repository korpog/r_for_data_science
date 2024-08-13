library(tidyverse)
library(babynames)

# 14.2
string1 <- "This is a string"
string2 <- 'If I want to include a "quote" inside a string, I use single quotes'

double_quote <- "\"" # or '"'
single_quote <- '\'' # or "'"
backslash <- "\\"

tricky <- "double_quote <- \"\\\"\" # or '\"'
single_quote <- '\\'' # or \"'\""
str_view(tricky)

tricky <- r"(double_quote <- "\"" # or '"'
single_quote <- '\'' # or "'")"
str_view(tricky)

x <- c("one\ntwo", "one\ttwo", "\u00b5", "\U0001f604")
x

# exercises

# 1
str1 <- 'He said "That\'s amazing!"'
str_view(str1)

str2 <- "\\a\\b\\c\\d"
str_view(str2)

str3 <- r"(\\\\\\)"
str_view(str3)

# 2
x <- "This\u00a0is\u00a0tricky"
str_view(x)

# 14.3
str_c("x", "y", "z")

str_c("Hello ", c("John", "Susan"))

df <- tibble(name = c("Flora", "David", "Terra", NA))
df |> mutate(greeting = str_c("Hi ", name, "!"))

df |>
  mutate(greeting1 = str_c("Hi ", coalesce(name, "you"), "!"),
         greeting2 = coalesce(str_c("Hi ", name, "!"), "Hi!"))

df |> mutate(greeting = str_glue("Hi {name}!"))
df |> mutate(greeting = str_glue("{{Hi {name}!}}"))

str_flatten(c("x", "y", "z"))
str_flatten(c("x", "y", "z"), ", ", last = ", and ")

df <- tribble(
  ~ name,
  ~ fruit,
  "Carmen",
  "banana",
  "Carmen",
  "apple",
  "Marvin",
  "nectarine",
  "Terence",
  "cantaloupe",
  "Terence",
  "papaya",
  "Terence",
  "mandarin"
)
df |>
  group_by(name) |>
  summarize(fruits = str_flatten(fruit, ", "))

# exercises

# 1
str_c("hi ", NA)
str_c(letters[1:2], letters[1:3])

paste0("hi ", NA)
paste0(letters[1:2], letters[1:3])

# 2
str_c("x", "Y", "ź", sep = " ")
paste("x", "y", "z")

# 3
str_c("The price of ", food, " is ", price)
str_glue("The price of {food} is {price}")

# 14.4
df1 <- tibble(x = c("a,b,c", "d,e", "f"))
df1 |>
  separate_longer_delim(x, delim = ",")

df2 <- tibble(x = c("1211", "131", "21"))
df2 |>
  separate_longer_position(x, width = 1)

df3 <- tibble(x = c("a10.1.2022", "b10.2.2011", "e15.1.2015"))
df3 |>
  separate_wider_delim(x,
                       delim = ".",
                       names = c("code", "edition", "year"))

df3 |>
  separate_wider_delim(x, delim = ".", names = c("code", NA, "year"))

df4 <- tibble(x = c("202215TX", "202122LA", "202325CA"))
df4 |>
  separate_wider_position(x, widths = c(year = 4, age = 2, state = 2))

df <- tibble(x = c("1-1-1", "1-1-2", "1-3", "1-3-2", "1"))

df |>
  separate_wider_delim(x, delim = "-", names = c("x", "y", "z"))

debug <- df |>
  separate_wider_delim(x,
                       delim = "-",
                       names = c("x", "y", "z"),
                       too_few = "debug")

df |>
  separate_wider_delim(x,
                       delim = "-",
                       names = c("x", "y", "z"),
                       too_few = "align_start")

df <- tibble(x = c("1-1-1", "1-1-2", "1-3-5-6", "1-3-2", "1-3-5-7-9"))

df |>
  separate_wider_delim(x, delim = "-", names = c("x", "y", "z"))

df |>
  separate_wider_delim(x,
                       delim = "-",
                       names = c("x", "y", "z"),
                       too_many = "drop")

# 14.5
str_length(c("a", "R for data science", NA))

babynames |>
  count(length = str_length(name), wt = n)

babynames |>
  filter(str_length(name) == 15) |>
  count(name, wt = n, sort = TRUE)

x <- c("Apple", "Banana", "Pear")
str_sub(x, 1, 3)

str_sub(x, -3, -1)

str_sub("a", 1, 5)

babynames |>
  mutate(first = str_sub(name, 1, 1), last = str_sub(name, -1, -1))

# exercises

# 2
babynames |>
  mutate(middle = str_sub(name, (str_length(name) %/% 2) + 1, (str_length(name) %/% 2) + 1))

# 3
babynames |>
  group_by(year, sex) |>
  summarise(avg_name_length = mean(str_length(name))) |>
  ggplot(aes(x = year, y = avg_name_length, colour = sex)) +
  geom_line()

# 14.6
charToRaw("Hadley")

x1 <- "text\nEl Ni\xf1o was particularly bad this year"
read_csv(x1)$text
read_csv(x1, locale = locale(encoding = "Latin1"))$text

x2 <- "text\n\x82\xb1\x82\xf1\x82\xc9\x82\xbf\x82\xcd"
read_csv(x2)$text
read_csv(x2, locale = locale(encoding = "Shift-JIS"))$text

u <- c("\u00fc", "u\u0308")
str_view(u)

str_length(u)
str_sub(u, 1, 1)

u[[1]] == u[[2]]

str_equal(u[[1]], u[[2]])

str_to_upper(c("i", "ı"))
str_to_upper(c("i", "ı"), locale = "tr")

str_sort(c("a", "c", "ch", "h", "z"))
str_sort(c("a", "c", "ch", "h", "z"), locale = "cs")
