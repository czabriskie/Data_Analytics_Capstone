require(randomForest)
require(dplyr)

H1B <- read.csv('./Project_2/H1B_Data/1. Master H1B Dataset.csv')
H1B_1 <- read.csv('./Project_2/H1B_Data/File 1 - H1B Dataset.csv')

summary(H1B$CASE_STATUS)
summary(H1B_1$CASE_STATUS)

lapply(H1B, class)

# a bunch of NA values, but I don't think we should just delete them like this
H1B1_1 <- na.omit(H1B1_1)

# Random Forest wont run even with obs with NA in them removed, since it can't handle more than 53 categories
randomForest(CASE_STATUS ~ ., data = H1B1_1)

             