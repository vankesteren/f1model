
library(tidyverse)
library(firatheme)
x <- seq(0, 5, 0.01)
a <- 0.3 # sill / intercept
b <- 1   # slope
c <- 0.7 # range / asymptote - intercept

halflogistic <- function(x, a, b, c) {
  e <- exp(-x*b)
  a + (1 - e) / (1 + e) * c
}

plot(x, halflogistic(x, a, b, c), type = "l")

# spaghetti plot prior sims
iter <- 500
a <- rnorm(iter)
b <- abs(rnorm(iter, mean = 2))
c <- abs(rnorm(iter))

spag <-
  sapply(1:iter, function(i) halflogistic(x, a[i], b[i], c[i])) %>%
  as_tibble() %>%
  set_names(1:iter) %>%
  mutate(x = x) %>%
  pivot_longer(-x, names_to = "sample")

spag %>%
  ggplot(aes(x = x, y = value, group = sample)) +
  geom_line(alpha = 0.1) +
  theme_fira()


# learning parameters from data
x <- 0:10
y <- halflogistic(x, a = -.5, b = 1, c = 1.5) + rnorm(length(x), 0, 0.1)
df <- tibble(x, y)
plot(x, y)

library(brms)

frm <- brmsformula(y ~ a + (1 - exp(-x*b)) / (1 + exp(-x*b)) * c,
                   a ~ 1, b ~ 1, c ~ 1, family = gaussian(), nl = TRUE)
pri <-
  prior(normal(0, 1), nlpar = "a") +
  prior(gamma(2, 1), nlpar = "b", lb = 0) +
  prior(gamma(2, 1), nlpar = "c", lb = 0)

res <- brm(frm, df, prior = pri, backend = "cmdstanr", cores = 4)

summary(res)

# spaghetti plot
pp <- posterior_epred(res, ndraws = 200, newdata = tibble(x = seq(0, 10, 0.1)))
pp %>%
  t() %>%
  as_tibble() %>%
  mutate(x = seq(0, 10, 0.1)) %>%
  pivot_longer(-x, names_to = "sample") %>%
  ggplot() +
  geom_line(alpha = 0.1, aes(x = x, y = value, group = sample)) +
  geom_point(data = df, aes(x = x, y = y), pch = 21, fill = "seagreen", col = "white", size = 2) +
  theme_fira() +
  labs(y = "skill", x = "season")

# tryout in multilevel setting
obs <- 50
a <- rnorm(iter)
b <- abs(rnorm(iter, mean = 2))
c <- abs(rnorm(iter))
x <- 0:10

ml_df <-
  sapply(1:obs, function(i) halflogistic(x, a[i], b[i], c[i]) + rnorm(length(x), 0, 0.1)) %>%
  as_tibble() %>%
  set_names(1:iter) %>%
  mutate(x = x) %>%
  pivot_longer(-x, names_to = "sample")

frm <- brmsformula(value ~ int + (1 - exp(-x*slo)) / (1 + exp(-x*slo)) * pot,
                   int ~ (1 | cor_int_slo | sample),
                   slo ~ (1 | cor_int_slo | sample),
                   pot ~ (1 | sample),
                   family = gaussian(),
                   nl = TRUE)
pri <-
  prior(normal(0, 1), nlpar = "int") +
  prior(gamma(2, 1), nlpar = "slo", lb = 0) +
  prior(gamma(2, 1), nlpar = "pot", lb = 0)

res_ml <- brm(frm, ml_df, prior = pri, backend = "cmdstanr", cores = 4, threads = 3,
              adapt_delta = 0.95)

mcmc_plot(res_ml, type = "trace")
summary(res_ml)
