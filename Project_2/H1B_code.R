library(randomForest)
H1B1 <- read.csv('C:\\Users\\camer\\Desktop\\H1B data\\1. Master H1B Dataset.csv')
H1B1_1 <- read.csv('C:\\Users\\camer\\Desktop\\H1B data\\File 1 - H1B Dataset.csv')
summary(H1B1$CASE_STATUS)
summary(H1B1_1)

randomForest(CASE_STATUS ~ ., data = H1B1_1)
