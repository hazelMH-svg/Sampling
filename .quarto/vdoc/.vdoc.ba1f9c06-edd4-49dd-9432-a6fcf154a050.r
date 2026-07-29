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
#| cache: true
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
samples |> 
  slice_head(n = 100) |> 
  ggplot(aes(y = sample, x = sample_mean, xmin = lower, xmax = upper)) +
  geom_errorbarh(height = 0.3) +
  geom_point() +
  geom_vline(xintercept = truth, color = "red", linetype = "dashed") +
  scale_y_continuous(breaks = NULL) +
  labs(
    x = "Sample mean age (years)",
    y = "Sample draw",
    title = "Confidence intervals from 100 simulated samples",
    subtitle = "About 95% of 95% intervals should contain the true mean age.",
    caption = "Data from the primer.data package"
  ) +
  theme_minimal()
#
#
#
coverage_frac <- samples |> 
  summarize(frac = mean(lower < truth & upper > truth)) |> 
  pull(frac)

coverage_frac
#
#
#
#
