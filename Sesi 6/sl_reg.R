# Simple linear regression
# 23 July 2026


# Packages ---------------------------------------------------------------

library(ggplot2)
library(dplyr)


# Data -------------------------------------------------------------------

dat <- iris


# Data exploration -------------------------------------------------------

skimr::skim(dat)


# Run linear regression --------------------------------------------------

lr <- lm(Sepal.Length ~ Sepal.Width, data = dat)


# Assumptions cehecking --------------------------------------------------

# Get fitted and residual values
dat$pred <- fitted.values(lr)
dat$res <- resid(lr)

# 1) Normality
ggplot(dat, aes(res)) +
    geom_histogram() +
    theme_bw()

# 2) Overall linearity
# 3) Equal variance
ggplot(dat, aes(pred, res)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = 2, colour = "red") +
  theme_bw()

# 4) Linearity of cont IVs 
ggplot(dat, aes(Sepal.Width, res)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = 2, colour = "red") +
  theme_bw()


# Model fit ---------------------------------------------------------------

# Adjusted R^2
summary(lr)
# The model explain 0.7% of the variance of the outcome


# Final model -------------------------------------------------------------

summary(lr)
confint(lr)


