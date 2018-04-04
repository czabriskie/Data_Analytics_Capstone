require(randomForest)
require(dplyr)
require(RWeka)
require(party)

H1B <- read.csv('./Project_2/H1B_Data/1. Master H1B Dataset.csv')
H1B_1 <- read.csv('./Project_2/H1B_Data/File 1 - H1B Dataset.csv')

summary(H1B$CASE_STATUS)
summary(H1B_1$CASE_STATUS)

str(H1B_1)

# str(H1B_1[,-8])

lapply(H1B, class)

# # a bunch of NA values, but I don't think we should just delete them like this
# H1B1_1 <- na.omit(H1B_1)

H1B_1[H1B_1$SOC_NAME %in% c("MANAGERS","MANAGEMENT","FIRST LINE SUPERVISORS"), 11] <- "MANAGEMENT"
H1B_1[H1B_1$SOC_NAME %in% c("GRAPHIC DESIGNERS","FASHION DESIGNERS","DESIGNERS"), 11] <- "DESIGNERS"
H1B_1 <- droplevels(H1B_1)

#Create less categories for state according to: https://www2.census.gov/geo/pdfs/reference/GARM/Ch6GARM.pdf
H1B_1.divisions <- H1B_1 %>% arrange(EMPLOYER_STATE) %>%
                             mutate(EMPLOYER_DIVISION = ifelse(EMPLOYER_STATE %in% c("ME","NH","VT","MA","RI","CT"), "New England",
                                                             ifelse(EMPLOYER_STATE %in% c("NY","NJ","PA"), "Middle Atlantic",
                                                                    ifelse(EMPLOYER_STATE %in% c("OH","IN","IL","MI","WI"), "East North Central",
                                                                           ifelse(EMPLOYER_STATE %in% c("MN","IA","MO","ND","SD","NE","KS"), "West North Central",
                                                                                  ifelse(EMPLOYER_STATE %in% c("DE","MD","DC","VA","WV","NC","SC","GA","FL"), "South Atlantic",
                                                                                         ifelse(EMPLOYER_STATE %in% c("KY","TN","AL","MS"), "East South Central",
                                                                                                ifelse(EMPLOYER_STATE %in% c("AR","LA","OK","TX"), "West South Central",
                                                                                                       ifelse(EMPLOYER_STATE %in% c("MT","ID","WY","CO","NM","AZ","UT","NV"), "Mountain",
                                                                                                              ifelse(EMPLOYER_STATE %in% c("WA","OR","CA","AK","HI"), "Pacific", "Other")))))))))) %>%
                            arrange(WORKSITE_STATE) %>%
                            mutate(WORKSITE_DIVISION = ifelse(WORKSITE_STATE %in% c("ME","NH","VT","MA","RI","CT"), "New England",
                                                              ifelse(WORKSITE_STATE %in% c("NY","NJ","PA"), "Middle Atlantic",
                                                                     ifelse(WORKSITE_STATE %in% c("OH","IN","IL","MI","WI"), "East North Central",
                                                                            ifelse(WORKSITE_STATE %in% c("MN","IA","MO","ND","SD","NE","KS"), "West North Central",
                                                                                   ifelse(WORKSITE_STATE %in% c("DE","MD","DC","VA","WV","NC","SC","GA","FL"), "South Atlantic",
                                                                                          ifelse(WORKSITE_STATE %in% c("KY","TN","AL","MS"), "East South Central",
                                                                                                 ifelse(WORKSITE_STATE %in% c("AR","LA","OK","TX"), "West South Central",
                                                                                                        ifelse(WORKSITE_STATE %in% c("MT","ID","WY","CO","NM","AZ","UT","NV"), "Mountain",
                                                                                                               ifelse(WORKSITE_STATE %in% c("WA","OR","CA","AK","HI"), "Pacific", "Other"))))))))))

H1B_1.divisions$EMPLOYER_DIVISION <- as.factor(H1B_1.divisions$EMPLOYER_DIVISION)
H1B_1.divisions$WORKSITE_DIVISION <- as.factor(H1B_1.divisions$WORKSITE_DIVISION)

H1B_1 <- H1B_1.divisions[,-c(8,9,19,25,26)]
str(H1B_1)

# conv_prevailing_wage <- rep(0,27932)
# H1B_1 <- cbind(H1B_1, conv_prevailing_wage)
# 
# H1B_1$conv_prevailing_wage <- ifelse(H1B_1$PW_UNIT_OF_PAY == "Hour", H1B_1$PREVAILING_WAGE * 40 * 52,
#                            ifelse(H1B_1$PW_UNIT_OF_PAY == "Week", H1B_1$PREVAILING_WAGE * 52,
#                              ifelse(H1B_1$PW_UNIT_OF_PAY == "Bi-Weekly", H1B_1$PREVAILING_WAGE * 26,
#                                     ifelse(H1B_1$PW_UNIT_OF_PAY == "Month", H1B_1$PREVAILING_WAGE * 12,
#                                         H1B_1$PREVAILING_WAGE))))

H1B_1$PREVAILING_WAGE <- ifelse(H1B_1$PW_UNIT_OF_PAY == "Hour", H1B_1$PREVAILING_WAGE * 40 * 52,
                               ifelse(H1B_1$PW_UNIT_OF_PAY == "Week", H1B_1$PREVAILING_WAGE * 52,
                                      ifelse(H1B_1$PW_UNIT_OF_PAY == "Bi-Weekly", H1B_1$PREVAILING_WAGE * 26,
                                             ifelse(H1B_1$PW_UNIT_OF_PAY == "Month", H1B_1$PREVAILING_WAGE * 12,
                                                    H1B_1$PREVAILING_WAGE))))
H1B_1$WAGE_RATE_OF_PAY_FROM <- ifelse(H1B_1$WAGE_UNIT_OF_PAY == "Hour", H1B_1$WAGE_RATE_OF_PAY_FROM * 40 * 52,
                                     ifelse(H1B_1$WAGE_UNIT_OF_PAY == "Week", H1B_1$WAGE_RATE_OF_PAY_FROM * 52,
                                            ifelse(H1B_1$WAGE_UNIT_OF_PAY == "Bi-Weekly", H1B_1$WAGE_RATE_OF_PAY_FROM * 26,
                                                   ifelse(H1B_1$WAGE_UNIT_OF_PAY == "Month", H1B_1$WAGE_RATE_OF_PAY_FROM * 12,
                                                          H1B_1$WAGE_RATE_OF_PAY_FROM))))
H1B_1$WAGE_RATE_OF_PAY_TO <- ifelse(H1B_1$WAGE_UNIT_OF_PAY == "Hour", H1B_1$WAGE_RATE_OF_PAY_TO * 40 * 52,
                                      ifelse(H1B_1$WAGE_UNIT_OF_PAY == "Week", H1B_1$WAGE_RATE_OF_PAY_TO * 52,
                                             ifelse(H1B_1$WAGE_UNIT_OF_PAY == "Bi-Weekly", H1B_1$WAGE_RATE_OF_PAY_TO * 26,
                                                    ifelse(H1B_1$WAGE_UNIT_OF_PAY == "Month", H1B_1$WAGE_RATE_OF_PAY_TO * 12,
                                                           H1B_1$WAGE_RATE_OF_PAY_TO))))

#H1B_1 <- H1B_1.divisions[,-c(14,19)]
  

#### Do we need to change prevailing wage so that they are on the same scale (this field would be correlated with pw_unit_of_pay)?
#### There are other int variables that we may need to convert to factors such as postal code or naics_code, but that would make it a variable with more than 53 variables...

H1B_1 <- na.omit(H1B_1)
# dir <- "G:/My Drive/school/Stats/MDATA Capstone/H1B data"
# write.csv(H1B_1, file = file.path(dir, "H1B_1.csv"))


# Random Forest still wont run, even though str(H1B_1) shows that all factors have < 53 categories...
h1b1_1.rf <- randomForest(CASE_STATUS ~ ., 
                          proximity=TRUE, importance=TRUE, keep.forest=TRUE, ntree = 5,
                          data = H1B_1)

# ==============================================================
# try go parallel

# Find out how many cores are available (if you don't already know)
detectCores()

# Create cluster with desired number of cores
cl = makeCluster(3)

# Register cluster
registerDoParallel(cl)

rf.par.out = 
  foreach(i = 1:3, 
          .options.RNG = 2827,
          .packages = 'randomForest', 
          .inorder = FALSE, 
          .errorhandling = 'remove') %dorng% 
  randomForest(CASE_STATUS ~ ., data=H1B_1, importance=TRUE, proximity=TRUE, 
               ntree=1000, norm.votes=FALSE)

rf.par.combo = combine(rf.par.out[[1]], rf.par.out[[2]], rf.par.out[[3]])
yhat.par.combo = predict(rf.par.combo, type="response")
mean(H1B_1$CASE_STATUS != yhat.par.combo)
mean(yhat.combo != yhat.par.combo)

# microbenchmark(
#   rf.par.out = 
#     foreach(i = 1:3, 
#             .options.RNG = 2827,
#             .packages = 'randomForest', 
#             .inorder = FALSE, 
#             .errorhandling = 'remove') %dorng% 
#     randomForest(CASE_STATUS ~ ., data=H1B_1, importance=TRUE, proximity=TRUE, 
#                  ntree=1000, norm.votes=FALSE),
#   times=5
# )

# Shutdown cluster neatly
if(exists("parallelCluster")) {
  parallel::stopCluster(parallelCluster)
  parallelCluster = c()
}

##=======================================================

j48_result <- J48(CASE_STATUS ~., data = H1B_1)
summary(j48_result)
#if(require("party", quietly = TRUE)) plot(m1)


             