#
#
#
#
#
#
#
#
#
#
#| message: false
library(tidyverse)
library(primer.data)
#
#
#
shaming
#
#
#
#
truth <- mean(shaming$age, na.rm = TRUE)
truth
#
#
#
#
set.seed(10)
my_sample <- shaming |> slice_sample(n = 50)
sample_mean <- mean(my_sample$age, na.rm = TRUE)
sample_mean
sample_se <- sd(my_sample$age, na.rm = TRUE) / sqrt(50)
sample_se
#
#
#
#
set.seed(10)
samples <- tibble(sample = 1:1000) |> 
  mutate(
    draw = map(sample, \(i) slice_sample(shaming, n = 50)),
    sample_mean = map_dbl(draw, \(d) mean(d$age, na.rm = TRUE)),
    sample_se = map_dbl(draw, \(d) sd(d$age, na.rm = TRUE) / sqrt(50)),
    lower = sample_mean - 1.96 * sample_se,
    upper = sample_mean + 1.96 * sample_se
  ) |> 
  select(-draw)
#
#
#
#
