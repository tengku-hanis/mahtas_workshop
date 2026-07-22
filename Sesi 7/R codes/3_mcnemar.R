# McNemar's test
# 23 July 2026

# Packages ----------------------------------------------------------------

library(tidyverse) #data manipulation
library(summarytools) #descriptive


# Data --------------------------------------------------------------------

mn_data <- read.csv("Sesi 7/Data/mcnemar_self_esteem.csv")


# Edit variable type ------------------------------------------------------

mn_data <- 
  mn_data %>% 
  mutate(Pre = as.factor(Pre),
         Post = as.factor(Post),
         Pre = fct_recode(Pre, 
                          low_self_esteem = "0",
                          high_self_esteem = "1"),
         Post = fct_recode(Post, 
                           low_self_esteem = "0",
                           high_self_esteem = "1"))


# Explore -----------------------------------------------------------------

summary(mn_data)
freq(mn_data)

ctable(mn_data$Pre, mn_data$Post)


# McNemar's test ----------------------------------------------------------

mcnemar.test(x = mn_data$Pre, y = mn_data$Post, correct = FALSE)
