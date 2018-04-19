require(randomForest)
require(dplyr)
require(RWeka)
require(rpart)
require(gbm)

h1b.master <- read.csv('./Project_2/H1B_Data/1. Master H1B Dataset.csv')

# Find the amount of people that got certified
soc.counts.cert <- filter(h1b.master, h1b.master$CASE_STATUS %in% c('CERTIFIED')) %>% 
  group_by(SOC_NAME) %>% tally() %>% write.csv('./Project_2/certified_counts.csv')

# Find amount of people that got denied
soc.counts.deni <- filter(h1b.master, h1b.master$CASE_STATUS %in% c('DENIED')) %>% 
  group_by(SOC_NAME) %>% tally() %>% write.csv('./Project_2/denied_counts.csv')

##======================================================
# Gradient Boosting
gbms <- list()
tables <- list()
tot.pct.correct <- list()
for (i in 1:10){
  h1b.boosting <- read.csv(paste0('./Project_2/H1B_Data/','File ', i, ' - H1B Dataset.csv'))
  h1b.boosting <- h1b.boosting[, -c(8, 26)]
  
  h1b.boost <- gbm(CASE_STATUS ~ . ,
                   data = h1b.boosting, 
                   n.trees = 5000, 
                   # cv.folds = 10,
                   shrinkage = 0.01)
  
  pred <- predict(h1b.boost, type = 'response', n.trees=5000)
  pred_class <- apply(pred, 1, which.max)
  for (x in 1:length(pred_class)){
    if (pred_class[x] == 1){
      pred_class[x] <- 'CERTIFIED'
    }
    else if (pred_class[x] == 2){
      pred_class[x] <- 'CERTIFIEDWITHDRAWN'
    }
    else if (pred_class[x] == 3){
      pred_class[x] <- 'DENIED'
    }
    else pred_class[x] <- 'WITHDRAWN'
  }
  
  gbms[[i]] <- h1b.boost
  tables[[i]] <- table(h1b.boosting$CASE_STATUS, pred_class) 
  tot.pct.correct[i] <- sum(diag(tables[[i]])) / dim(h1b.boosting)[1]
}

# Use this to find the percent accuracy for each level of CASE_STATUS
# for (i in 1:4){
#   if (i == 1){
#     print(tables[[1]][i, 1] / sum(tables[[1]][i, ]))
#   }
#   else if (i == 2){
#     print(tables[[1]][i, 2] / sum(tables[[1]][i, ]))
#   }
#   else if (i == 3){
#     print(tables[[1]][i, 3] / sum(tables[[1]][i, ]))
#   }
#   else {
#     print(tables[[1]][i, 4] / sum(tables[[1]][i, ]))
#   }
# }

rel.inf <- list()
for (x in 1:length(tables)){
  rel.inf[[x]] <- as.data.frame(summary((gbms[[x]])))
  
}

# Export variable importance for each file
merge(rel.inf[[1]], rel.inf[[2]], by = c('var')) %>%
  merge(., rel.inf[[3]], by = 'var') %>%
  merge(., rel.inf[[4]], by = 'var') %>%
  merge(., rel.inf[[5]], by = 'var') %>%
  merge(., rel.inf[[6]], by = 'var') %>%
  merge(., rel.inf[[7]], by = 'var') %>%
  merge(., rel.inf[[8]], by = 'var') %>%
  merge(., rel.inf[[9]], by = 'var') %>%
  merge(., rel.inf[[10]], by = 'var') %>% 
  write.csv('./Project_2/rel_inf.csv')

###########################################################################
# 80 20 split testing and training datasets
##########################################################################
gbms <- list()
tables <- list()
tot.pct.correct <- list()

for (i in 1:10){
  h1b.boosting <- read.csv(paste0('./Project_2/H1B_Data/','File ', i, ' - H1B Dataset.csv'))
  h1b.boosting <- h1b.boosting[, -c(8, 26)]
  h1b.boosting <- h1b.boosting %>% select(-EMPLOYER_COUNTRY)
  
  h1b.boosting <- h1b.boosting %>% tibble::rownames_to_column('row_num') 
  test <- h1b.boosting %>% sample_frac(0.2)
  train <- subset(h1b.boosting, !(h1b.boosting$row_num %in% test$row_num))
  
  test <- test %>% select(-row_num)
  train <- train %>% select(-row_num)
  
  h1b.gbm <- gbm(CASE_STATUS ~ .,
                 data = train, 
                 n.trees = 5000, 
                 shrinkage = 0.01)
  
  predictions <- predict(object = h1b.gbm,
                         newdata = test,
                         n.trees = 500,
                         type = 'response')

  pred_class <- apply(predictions, 1, which.max)
  
  for (x in 1:length(pred_class)){
    if (pred_class[x] == 1){
      pred_class[x] <- 'CERTIFIED'
    }
    else if (pred_class[x] == 2){
      pred_class[x] <- 'CERTIFIEDWITHDRAWN'
    }
    else if (pred_class[x] == 3){
      pred_class[x] <- 'DENIED'
    }
    else pred_class[x] <- 'WITHDRAWN'
  }
  
  gbms[[i]] <- h1b.gbm
  tables[[i]] <- table(test$CASE_STATUS, pred_class) 
  tot.pct.correct[i] <- sum(diag(tables[[i]])) / dim(test)[1]
}

c3.correctly.classified <- as.data.frame(cbind(1:10, unlist(tot.pct.correct)))
names(c3.correctly.classified) <- c('File', 'Percent Correclty Classified')
write.csv(c3.correctly.classified, './Project_2/GBM_PCT_Correctly_Classified_80_20_Split.csv')

rel.inf <- list()
for (x in 1:length(tables)){
  rel.inf[[x]] <- as.data.frame(summary((gbms[[x]])))
  
}

# Export variable importance for each file
merge(rel.inf[[1]], rel.inf[[2]], by = c('var')) %>%
  merge(., rel.inf[[3]], by = 'var') %>%
  merge(., rel.inf[[4]], by = 'var') %>%
  merge(., rel.inf[[5]], by = 'var') %>%
  merge(., rel.inf[[6]], by = 'var') %>%
  merge(., rel.inf[[7]], by = 'var') %>%
  merge(., rel.inf[[8]], by = 'var') %>%
  merge(., rel.inf[[9]], by = 'var') %>%
  merge(., rel.inf[[10]], by = 'var') %>% 
  write.csv('./Project_2/rel_inf_80_20.csv')


##########################################################################################
# Can we Beat the Poster at SAS global Forum?
# They only used observations that were either accepted or rejected
# Claim the highest was 86.139% classification rate with AdaBoost
# With a 0.142 balanced error rate on validation data
# BER = 0.5*(b/(a+b) + c/(c+d)), with 2 * 2 table
# Claimed most important variables were acceptance ratio for employer and the number of petitions filled by employer
# BRING IT ON SADHIR!
##########################################################################################

h1b.master <- read.csv('./Project_2/H1B_Data/1. Master H1B Dataset.csv')

sub.master <- h1b.master %>% filter(CASE_STATUS %in% c('CERTIFIED', 'DENIED')) 
sub.master$CASE_STATUS <- droplevels(sub.master$CASE_STATUS)
new_case.status <- list()
for (x in 1:length(sub.master$CASE_STATUS)){
  if(sub.master$CASE_STATUS[x] == 'CERTIFIED'){
    new_case.status[x] <- 1
  }
  else {
    new_case.status[x] <- 0
  }
}

sub.master$CASE_STATUS <- unlist(new_case.status) 
h1b.boosting <- sub.master[, -c(8, 26)]
################### Gradient Boosting on binary master data #################
h1b.boosting <- h1b.boosting %>% tibble::rownames_to_column('row_num') 
test <- h1b.boosting %>% sample_frac(0.2)
train <- subset(h1b.boosting, !(h1b.boosting$row_num %in% test$row_num))

test <- test %>% select(-row_num)
train <- train %>% select(-row_num)


h1b.gbm <- gbm(CASE_STATUS ~ . ,
                 data = train, 
                 n.trees = 5000, 
                 shrinkage = 0.01)

pred <- predict(object = h1b.gbm,
                       newdata = test,
                       n.trees = 500,
                       type = 'response')

for (x in 1:length(pred)){
  if (pred[x] >= 0.5){
    pred[x] <- 1
  }
  else {
    pred[x] <- 0
  }
}

table <- table(test$CASE_STATUS, pred)
# 0.9863536accuracy
tot.pct.correct.gbm <- sum(diag(table)) / sum(sapply(table, sum))
# BER = 0.5*(b/(a+b) + c/(c+d))
0.5 * (table[1, 2]/ (table[1, 1] + table[1, 2]) + table[2,1] / (table[2, 1] + table[2, 2]))
# BER = 0.4536703

################################################ Random Forest ###############################################
h1b.master <- read.csv('./Project_2/H1B_Data/1. Master H1B Dataset.csv')

sub.master <- h1b.master %>% filter(CASE_STATUS %in% c('CERTIFIED', 'DENIED')) 
sub.master$CASE_STATUS <- droplevels(sub.master$CASE_STATUS)

sub.master[sub.master$SOC_NAME %in% c("MANAGERS","MANAGEMENT","FIRST LINE SUPERVISORS"), 11] <- "MANAGEMENT"
sub.master[sub.master$SOC_NAME %in% c("GRAPHIC DESIGNERS","FASHION DESIGNERS","DESIGNERS"), 11] <- "DESIGNERS"
sub.master <- droplevels(sub.master)
#Create less categories for state according to: https://www2.census.gov/geo/pdfs/reference/GARM/Ch6GARM.pdf
sub.master.divisions <- sub.master %>% arrange(EMPLOYER_STATE) %>%
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

sub.master.divisions$EMPLOYER_DIVISION <- as.factor(sub.master.divisions$EMPLOYER_DIVISION)
sub.master.divisions$WORKSITE_DIVISION <- as.factor(sub.master.divisions$WORKSITE_DIVISION)

sub.master <- sub.master.divisions[,-c(8,9,19,25,26)]
sub.master <- na.omit(sub.master)

sub.master.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                              ntree = 100,
                              data = sub.master)

rf.tbl <- sub.master.rf$confusion
# CERTIFIED DENIED class.error
# CERTIFIED    466460   2509 0.005350034
# DENIED         6772    179 0.974248310
sum(diag(rf.tbl)) / sum(sapply(rf.tbl, sum))
# accuracy = 0.9804968
# BER
0.5 * (rf.tbl[1, 2]/ (rf.tbl[1, 1] + rf.tbl[1, 2]) + rf.tbl[2,1] / (rf.tbl[2, 1] + rf.tbl[2, 2]))
# BER = 0.4897992