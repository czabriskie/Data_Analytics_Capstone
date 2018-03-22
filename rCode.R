rm(list=ls())
require(gam)
require(ggplot2)
require(dplyr)
require(MASS)
require(glmnet)
require(glinternet)

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
hist(insurance$charges)

# Fix skewness with log transformation 
# Why do this beforehand? Skewness shouldn't be a probelm unless the residuals are non-normal as a result
insurance$charges_log <- log(insurance$charges) 

par(mfrow =c(2, 2))
hist(insurance$charges_log)
qqnorm(insurance$charges_log)
qqline(insurance$charges_log, col = 'red')
boxplot(insurance$charges_log)

# Look at distribution of numerical predictors
par(mfrow = c(2, 2))
for (pred in c(1, 3, 8)){
  hist(insurance[, pred],
       main = names(insurance)[pred])
  
}
# No real relationships can be seen from scatterplots of data
pairs(insurance[, -7], labels = names(insurance[, -7]))

# Means of charges grouped by region, smoker, children, and sex
insurance %>% group_by(region) %>% summarize(region.means = mean(charges_log, na.rm = TRUE))

insurance %>% group_by(smoker) %>% summarize(region.means = mean(charges_log, na.rm = TRUE))

insurance %>% group_by(children) %>% summarize(region.means = mean(charges_log, na.rm = TRUE))

insurance %>% group_by(sex) %>% summarize(region.means = mean(charges_log, na.rm = TRUE))

# Means of BMI by bin

# BMI Cutoffs https://www.cdc.gov/healthyweight/assessing/bmi/adult_bmi/index.html
# Below 18.5 = Underweight
# 18.5 - 24.9 - NOmral or Healthy Weight
# 25.0 - 29.9 - Overweight
# > 30 - Obese

insurance.bmi.groups <-  insurance %>% 
  arrange(bmi) %>% mutate(BMI_Categories = cut(bmi, 
                                               breaks = c(-Inf, 18.5, 24.9, 29.9, Inf), 
                                               labels = c('Underweight', 'Normal', 'Overweight', 'Obese')))


insurance.bmi.groups %>% group_by(BMI_Categories) %>% 
  summarize(BMI_Group_Log_Charges_Means = mean(charges_log, na.rm = TRUE))

################################### ANOVA not a good test to use on this data #######################################
# ANOVA of BMI Categories log charges
BMI.aov <- aov(charges_log ~ BMI_Categories, data = insurance.bmi.groups)

# F-test was significant
# Possible too many observations for ANOVA
summary(BMI.aov)

####################################################################################################################
# Means of ages by group
# groups http://www.widener.edu/about/campus_resources/wolfgram_library/documents/life_span_chart_final.pdf

# Development Through life Grouping
insurance.age.groups <- insurance %>% 
  arrange(age) %>% 
mutate(AGE_Categories_Life = cut(age,
                                 breaks = c(17, 24, 34, 60, 75),
                                 labels = c('Later Adolescence', 'Early Adulthood', 'Middle Adulthood' ,'Later Adulthood'))) 

insurance.age.groups %>% group_by(AGE_Categories_Life) %>% summarize(Life_Age_Means = mean(charges_log, na.rm = TRUE))

#######################################################################################################################  
# Cut Age by different age categrories
  # %>% mutate(AGE_Categories_Academic = cut(age,
  #                                      breaks = c(17, 25, 40, 60, 'INF'),
  #                                      labels = c('Young Adult', 'Adulthood', 'Middle Age' ,'Older People')))

# mean ages for bins by Academic categories
# insurance.age.groups %>% group_by(AGE_Categories_Academic) %>% summarize(Academic_Age = mean(charges_log))
#######################################################################################################################

#######################################################################################################################
# ANOVA not a good method to use on data this large
# get rid of charges column and numeric age column
insurance.age.groups <- insurance.age.groups[, -c(1, 7)] 
# ANOVA of BMI Categories
Age.aov <- aov(charges_log ~ (.)^2, data = insurance.age.groups)

# F-test is significant, meaning at least one of the groups is statistically different from another group
summary(Age.aov)

# TukeyHSD(Age.aov, 'AGE_Categories_Life', conf.level=0.95)
# No age groups are statistically significant from each other for academic age

#######################################################################################################################

# Group LASSO Regression with interaction terms
# Make matrices suitable for glinternet function
insurance.p <- insurance %>% dplyr::select(-c(charges, charges_log))
# males 0 females 1
insurance.p$sex <- recode(insurance.p$sex, male = 0, female = 1)
# smoker yes = 1 no = 0
insurance.p$smoker <- recode(insurance.p$smoker, yes = 1, no = 0)
# region NE = 1, NW = 2, SE = 3, SW = 4
insurance.p$region <- recode(insurance.p$region, northeast = 0, northwest = 1, southeast = 2, southwest = 3)
# change age variables to factors
insurance.p$age <- insurance.age.groups$AGE_Categories_Life %>% recode('Later Adolescence' = 0,
                                                                       'Early Adulthood' = 1,
                                                                       'Middle Adulthood' = 2,
                                                                       'Later Adulthood' = 3)
# change BMI to factors
insurance.p$bmi <- insurance.bmi.groups$BMI_Categories %>% recode('Underweight' = 0,
                                                                  'Normal' = 1,
                                                                  'Overweight' = 2,
                                                                  'Obese' = 3)
insurance.p$children <- as.numeric(insurance.p$children)
X <- as.matrix(sapply(insurance.p, as.integer))
# Too many interactions I can't sort out what is what
fit <- glinternet(X, insurance$charges_log, numLevels = c(4, 2, 4, 6, 2, 4), family = 'gaussian')


###################### Ridge Regression ################################
# fit <- glmnet(X, insurance$charges_log, )
# Cross tab of region and children
table(insurance$region, insurance$children)

# Cross tab of sex and children
table(insurance$sex, insurance$children)

#################### Tests of Independence #############################
# Should these even be used here?
sex.region <- table(insurance$sex, insurance$region)
chisq.test(sex.region) # non-significant therefore independent

smoker.region <- table(insurance$smoker, insurance$region)
chisq.test(smoker.region) # marginially non-significant (suprising!)

binned_age.binned_bmi <- table(insurance.age.groups$AGE_Categories_Life, insurance.bmi.groups$BMI_Categories)
# wouldn't work
fisher.test(binned_age.binned_bmi)

binned_bmi.smoker <- table(insurance.bmi.groups$BMI_Categories, insurance$smoker)
binned_bmi.smoker
chisq.test(binned_bmi.smoker)

# FIt a Generalized Additive Model
gam1 <- gam(charges_log ~ ., data = insurance)
summary(gam1)
