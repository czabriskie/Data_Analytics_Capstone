rm(list=ls())
setwd("G:/My Drive/school/Stats/MDATA Capstone/Data_Analytics_Capstone")
library(gam)
library(ggplot2)


dir <- "G:/My Drive/school/Stats/MDATA Capstone/Data_Analytics_Capstone"

insurance <- read.csv(file = file.path(dir, "insurance.csv"))
summary(insurance)
qplot(x = charges, data = insurance)

insurance$charges_log <- log10(insurance$charges) 

gam1 <- gam(charges_log ~ ., data = insurance)
summary(gam1)
