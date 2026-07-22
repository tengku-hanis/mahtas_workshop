# Multiple linear regression
# 23 July 2026


# Packages ---------------------------------------------------------------

library(ggplot2)
library(dplyr)


# Data -------------------------------------------------------------------

dat <- iris


# Data exploration -------------------------------------------------------

skimr::skim(dat)


# Data -------------------------------------------------------------------

dat <- iris


# Run linear regression --------------------------------------------------

mlr <- lm(Sepal.Length ~ Sepal.Width + Species, data = dat)


# Interaction -------------------------------------------------------------

int_mod <- update(mlr, .~. + Sepal.Width*Species)
summary(int_mod) 

# All are not significant


# Assumptions -------------------------------------------------------------

dat$res <- resid(mlr)
dat$pred <- fitted.values(mlr)

# 1) Normality
dat %>% 
  ggplot(aes(res)) +
  geom_histogram() 

# 2) Overall linearity
# 3) Equal variance
ggplot(dat, aes(pred, res)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = 2, colour = "red")

# 4) Linearity of cont IVs 
ggplot(dat, aes(Sepal.Width, res)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = 2, colour = "red")


# Outliers and influential observations -----------------------------------

out_data <- cooks.distance(mlr)  
sum(out_data > 1)


# Multicollinearity -------------------------------------------------------

car::vif(mlr) #all GVIF^(1/(2*Df)) should be < 3.16 


# Model fit ---------------------------------------------------------------

# Adjusted R^2
summary(mlr)
# The model explain 72.0% of the variance of the outcome


# Final model -------------------------------------------------------------

summary(mlr)
confint(mlr) %>% round(digits = 2)
