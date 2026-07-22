# Pearson's chi-square test
# 23 July 2026

# Packages ----------------------------------------------------------------

install.packages("tidyverse")
install.packages("summarytools")

library(tidyverse) #data manipulation
library(summarytools) #descriptive


# Data --------------------------------------------------------------------

pc_data <- read.csv("Sesi 7/Data/chisquare_smoking.csv")


# Edit variable type ------------------------------------------------------

pc_data <- 
  pc_data %>% 
  mutate(Smoking = as.factor(Smoking),
         Lung_cancer = as.factor(Lung_cancer),
         Smoking = fct_recode(Smoking, 
                              nonsmoker = "0",
                              smoker = "1"),
         Lung_cancer = fct_recode(Lung_cancer, 
                                  noncancer = "0",
                                  cancer = "1"))


# Explore -----------------------------------------------------------------

summary(pc_data)
freq(pc_data)
ctable(pc_data$Smoking, pc_data$Lung_cancer)


# Pearson's chi-square test -----------------------------------------------

pc_mod <- chisq.test(x = pc_data$Smoking, y = pc_data$Lung_cancer, correct = FALSE)
pc_mod$expected 
#Assumption - expected count < 5 is 0 or <20%, so Pearson's chi-square is used

pc_mod #result