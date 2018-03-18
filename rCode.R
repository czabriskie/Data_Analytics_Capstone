rm(list=ls())
require(gam)
require(ggplot2)
require(dplyr)
require(MASS)

# Read in Data
# data from https://www.kaggle.com/mirichoi0218/insurance
insurance <- read.csv(file = file.path("insurance.csv"))

# General Summary of Insurance Data
summary(insurance)
# Age Mean and Median look similar, so probably not skewed
# Sex - about the same males and females
# BMI - mean and median about the same so probably not skewed
# children max is 5 children min is 0 
# Charges - Min 1122 Max - 63770

# Charges definitely skewed
qplot(x = charges, data = insurance)

# Fix skewness with log transformation 
# Why do this beforehand? Skewness shouldn't be a probelm unless the residuals are non-normal as a result
insurance$charges_log <- log(insurance$charges) 
qplot(x = charges_log, data = insurance)

insurance_predictors <- c('age', 'bmi', 'charges', 'log_charges')
# Look at distribution of numerical predictors
par(mfrow = c(2, 2))
for (pred in c(1, 3, 7, 8)){
  hist(insurance[, pred],
       main = names(insurance)[pred])
  
}

# Means of charges grouped by region, smoker, children, and sex
insurance %>% group_by(region) %>% summarize(region.means = mean(charges, na.rm = TRUE))

insurance %>% group_by(smoker) %>% summarize(region.means = mean(charges, na.rm = TRUE))

insurance %>% group_by(children) %>% summarize(region.means = mean(charges, na.rm = TRUE))

insurance %>% group_by(sex) %>% summarize(region.means = mean(charges, na.rm = TRUE))

# Means of BMI by bin

# BMI Cutoffs https://www.cdc.gov/healthyweight/assessing/bmi/adult_bmi/index.html
# Below 18.5 = Underweight
# 18.5 - 24.9 - NOmral or Healthy Weight
# 25.0 - 29.9 - Overweight
# > 30 - Obese

insurance.bmi.groups <-  insurance %>% arrange(bmi) %>% mutate(BMI_Categories = cut(bmi, 
                                                                                    breaks = c(-Inf, 18.5, 24.9, 29.9, Inf), 
                                                                                    labels = c('Underweight', 'Normal', 'Overweight', 'Obese')))

insurance.bmi.groups %>% group_by(BMI_Categories) %>% summarize(BMI_Group_Means = mean(charges, na.rm = TRUE))


# Means of ages by group
# groups http://www.widener.edu/about/campus_resources/wolfgram_library/documents/life_span_chart_final.pdf

# Development Through life Grouping
insurance.age.groups <- insurance %>% arrange(age) %>% mutate(AGE_Categories_Life = cut(age,
                                                                                        breaks = c(17, 24, 34, 60, 75),
                                                                                        labels = c('Later Adolescence', 'Early Adulthood', 'Middle Adulthood' ,'Later Adulthood'))) %>% 
  mutate(AGE_Categories_Academic = cut(age,
                                       breaks = c(17, 25, 40, 60, 'INF'),
                                       labels = c('Young Adult', 'Adulthood', 'Middle Age' ,'Older People')))

# mean ages for bins by life_categories
insurance.age.groups %>% group_by(AGE_Categories_Life) %>% summarize(Life_Age_Means = mean(charges, na.rm = TRUE))

# mean ages for bins by Academic categories
insurance.age.groups %>% group_by(AGE_Categories_Academic) %>% summarize(Academic_Age = mean(charges))

# Cross tab of region and children
table(insurance$region, insurance$children)

# Cross tab of sex and children
table(insurance$sex, insurance$children)

#################### Tests of Independence #############################
sex.region <- table(insurance$sex, insurance$region)
chisq.test(sex.region) # non-significant therefore independent

smoker.region <- table(insurance$smoker, insurance$region)
chisq.test(smoker.region) # marginially non-significant (suprising!)



# FIt a Generalized Additive Model
gam1 <- gam(charges_log ~ ., data = insurance)
summary(gam1)
