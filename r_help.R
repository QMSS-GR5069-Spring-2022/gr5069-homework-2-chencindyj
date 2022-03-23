library(car)
library(dplyr)
library(GPArotation)
library(psych)

mortgage <- read.csv("C:/Users/Cindy C/Documents/CINDY CHEN/2021 Prep/SCHOOL WORK/QMSS GR5015 - Data Analysis/Final Project/mortgage_data.csv")
mortgage <- as.data.frame(mortgage)
mortgage <- mortgage %>% select(-activity_year, -county_code)
mortgage <- mortgage %>% select(-action_taken)

lreg <- glm(action_taken ~ debt_to_income_ratio + applicant_age + loan_amount_log + income_log + property_value_log + combined_loan_to_value_ratio + applicant_sex + loan_term + ffiec_msa_md_median_family_income + tract_to_msa_income_percentage, data=mortgage, family = binomial(link="logit"))
logodds <- lreg$linear.predictors

boxTidwell(logodds ~ income_log + property_value_log + combined_loan_to_value_ratio + loan_term + ffiec_msa_md_median_family_income + tract_to_msa_income_percentage + debt_to_income_ratio + applicant_age, ~ applicant_sex, data = mortgage)

#install.packages("GPArotation")

fa <- fa(mortgage, nfactors = 3, fm = "pa", rotate = "none", max.iter = 100)
fa.rotate <- fa(mortgage, nfactors = 3, fm = "pa", rotate = "varimax", max.iter = 100)
fa.rotate$loadings

factors <- fa.rotate$scores[,1:3]
colnames(factors) <- c("Financial", "Race+Fin","Other")

# data descrpition
describe(factors)
helo <- glm(action_taken ~ debt_to_income_ratio + applicant_age + loan_amount_log + income_log + property_value_log + combined_loan_to_value_ratio + loan_term + ffiec_msa_md_median_family_income + tract_to_msa_income_percentage, data=mortgage, family = binomial(link="logit"))


reset_ramsey(helo)

