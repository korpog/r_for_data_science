library(tidyverse)
library(babynames)

# 15.2
str_view(fruit, "berry")
str_view(c("a", "ab", "ae", "bd", "ea", "eab"), "a.")
str_view(fruit, "a...e")
str_view(c("a", "ab", "abb"), "ab?")
str_view(c("a", "ab", "abb"), "ab+")
str_view(c("a", "ab", "abb"), "ab*")

str_view(words, "[aeiou]x[aeiou]")
str_view(words, "[^aeiou]y[^aeiou]")
str_view(fruit, "apple|melon|nut")

# 15.3
str_detect(c("a", "b", "c"), "[aeiou]")

babynames |>
  filter(str_detect(name, "x")) |>
  count(name, wt = n, sort = TRUE)

babynames |>
  group_by(year) |>
  summarize(prop_x = mean(str_detect(name, "x"))) |>
  ggplot(aes(x = year, y = prop_x)) +
  geom_line()

x <- c("apple", "banana", "pear")
str_count(x, "p")

str_count("abababa", "aba")
str_view("abababa", "aba")

babynames |>
  count(name) |>
  mutate(
    vowels = str_count(name, regex("[aeiou]", ignore_case = TRUE)),
    consonants = str_count(name, regex("[^aeiou]", ignore_case = TRUE))
  )

babynames |>
  count(name) |>
  mutate(
    name = str_to_lower(name),
    vowels = str_count(name, "[aeiou]"),
    consonants = str_count(name, "[^aeiou]")
  )

x <- c("apple", "pear", "banana")
str_replace_all(x, "[aeiou]", "-")

x <- c("apple", "pear", "banana")
str_remove_all(x, "[aeiou]")

df <- tribble(
  ~str,
  "<Sheryl>-F_34",
  "<Kisha>-F_45",
  "<Brandon>-N_33",
  "<Sharon>-F_38",
  "<Penny>-F_58",
  "<Justin>-M_41",
  "<Patricia>-F_84",
)

df |>
  separate_wider_regex(str,
    patterns = c(
      "<",
      name = "[A-Za-z]+",
      ">-",
      gender = ".",
      "_",
      age = "[0-9]+"
    )
  )

# exercises

# 1
babynames |>
  count(name) |>
  mutate(
    name = str_to_lower(name),
    vowels = str_count(name, "[aeiou]"),
    vowel_prop = vowels / str_length(name)
  ) |>
  arrange(desc(vowel_prop))

# 2
str <- "a/b/c/d/e"
str_replace_all(str, "/", "\\")

str <- "a/b/c/d/e"
str_replace_all(str, "/", "\\\\")

# 3
str2 <- "AbDzxy"
str_replace_all(str2, "[A-Z]", function(x) {
  tolower(x)
})

# 4
tel <- "724-525-610"
str_match(tel, "[0-9]+-[0-9]+-[0-9]+")

# 15.4
dot <- "\\."
str_view(dot)
str_view(c("abc", "a.c", "bef"), "a\\.c")

x <- "a\\b"
str_view(x)
str_view(x, "\\\\")
str_view(x, r"{\\}")

str_view(c("abc", "a.c", "a*c", "a c"), "a[.]c")
str_view(c("abc", "a.c", "a*c", "a c"), ".[*]c")

str_view(fruit, "^a")
str_view(fruit, "a$")

str_view(fruit, "apple")
str_view(fruit, "^apple$")

x <- c("summary(x)", "summarize(df)", "rowsum(x)", "sum(x)")
str_view(x, "sum")
str_view(x, "\\bsum\\b")

str_view("abc", c("$", "^", "\\b"))
str_replace_all("abc", c("$", "^", "\\b"), "--")

x <- "abcd ABCD 12345 -!@#%."
str_view(x, "[abc]+")
str_view(x, "[a-z]+")
str_view(x, "[^a-z0-9]+")

str_view("a-b-c", "[a-c]")
str_view("a-b-c", "[a\\-c]")

x <- "abcd ABCD 12345 -!@#%."
str_view(x, "\\d+")
str_view(x, "\\D+")
str_view(x, "\\s+")
str_view(x, "\\S+")
str_view(x, "\\w+")
str_view(x, "\\W+")

str_view(fruit, "(..)\\1")
str_view(words, "^(..).*\\1$")

sentences |>
  str_replace("(\\w+) (\\w+) (\\w+)", "\\1 \\3 \\2") |>
  str_view()

sentences |>
  str_match("the (\\w+) (\\w+)") |>
  head()

sentences |>
  str_match("the (\\w+) (\\w+)") |>
  as_tibble(.name_repair = "minimal") |>
  set_names("match", "word1", "word2")

x <- c("a gray cat", "a grey dog")
str_match(x, "gr(e|a)y")
str_match(x, "gr(?:e|a)y")

# exercises

# 1
s1 <- "\"\'\\"
str_view(s1)
s2 <- "$^$"
str_view(s2)

str_view(s1, "\"\'\\\\")
str_view(s2, "\\W+")

# 3
str_view(words, "^y")
str_view(words, "^[^y]")
str_view(words, "x$")
str_view(words, "^\\w{3}$")
str_view(words, "^\\w{7,}$")
str_view(words, "[aeiou][^aeiou]")
str_view(words, "([aeiou][^aeiou])([aeiou][^aeiou])")
str_view(words, "^([aeiou][^aeiou])([aeiou][^aeiou])$")

# 4
str_view(c("airplane", "aeroplane"), "(air|aero)plane")
str_view(c("aluminum", "aluminium"), "alumini?um")
str_view(c("ass", "arse"), "a(ss|rs)e*")

# 5
str_replace(words, "^(.)(\\w*)(.)$", "\\3\\2\\1")

# 15.5
bananas <- c("banana", "Banana", "BANANA")
str_view(bananas, "banana")
str_view(bananas, regex("banana", ignore_case = TRUE))

x <- "Line 1\nLine 2\nLine 3"
str_view(x, ".Line")
str_view(x, regex(".Line", dotall = TRUE))

x <- "Line 1\nLine 2\nLine 3"
str_view(x, "^Line")
str_view(x, regex("^Line", multiline = TRUE))

phone <- regex(
  r"(
    \(?     # optional opening parens
    (\d{3}) # area code
    [)\-]?  # optional closing parens or dash
    \ ?     # optional space
    (\d{3}) # another three numbers
    [\ -]?  # optional space or dash
    (\d{4}) # four more numbers
  )",
  comments = TRUE
)

str_extract(c("514-791-8141", "(123) 456 7890", "123456"), phone)

str_view(c("", "a", "."), fixed("."))

str_view("x X", "X")
str_view("x X", fixed("X", ignore_case = TRUE))

str_view("i İ ı I", fixed("İ", ignore_case = TRUE))
str_view("i İ ı I", coll("İ", ignore_case = TRUE, locale = "tr"))

# 15.6
str_view(sentences, "^The")
str_view(sentences, "^The\\b")

str_view(sentences, "^(She|He|It|They)\\b")

pos <- c("He is a boy", "She had a good time")
neg <- c("Shells come from the sea", "Hadley said 'It's a great day'")

pattern <- "^(She|He|It|They)\\b"
str_detect(pos, pattern)
str_detect(neg, pattern)

str_view(words, "^[^aeiou]+$")
str_view(words[!str_detect(words, "[aeiou]")])

str_view(words, "a.*b|b.*a")
words[str_detect(words, "a") & str_detect(words, "b")]

words[str_detect(words, "a") &
  str_detect(words, "e") &
  str_detect(words, "i") &
  str_detect(words, "o") &
  str_detect(words, "u")]

str_view(sentences, "\\b(red|green|blue)\\b")

rgb <- c("red", "green", "blue")
str_c("\\b(", str_flatten(rgb, "|"), ")\\b")

str_view(colors())

cols <- colors()
cols <- cols[!str_detect(cols, "\\d")]
str_view(cols)

pattern <- str_c("\\b(", str_flatten(cols, "|"), ")\\b")
str_view(sentences, pattern)

# exercises

# 1
words[str_detect(words, "^x") | str_detect(words, "x$")]

words[str_detect(words, "^[aeiou]") &
  !str_detect(words, "[aeiou]$")]

# 2
str_view(words, "ie")
str_view(words, "cei")

# 3
cols <- colors()
str_view(cols, "^dark")

str_replace(cols, "^dark", "")

# 15.7
apropos("replace")
head(list.files(pattern = "\\.Rmd$"))
