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





H1B_1 <- read.csv('./Project_2/H1B_Data/File 1 - H1B Dataset.csv')

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


H1B_1 <- na.omit(H1B_1)

h1b1_1.rf <- randomForest(CASE_STATUS ~ ., data = H1B_1)

##========================================================
# Classification tree will not finish in a really long time
# h1b_1.cart <- rpart(CASE_STATUS ~ . , method = 'class', data = H1B_1) 

# Since there are so many variables with many factor levels the amount of combinations to split on is enormous
sapply(sapply(H1B_1,levels), length)

##=======================================================

j48_result <- J48(CASE_STATUS ~., data = H1B_1)
summary(j48_result)
#if(require("party", quietly = TRUE)) plot(m1)

# j48 on master that was not sampled java.lang.out of memory error????
j48.master <- J48(CASE_STATUS ~., data = h1b.master)
summary(j48_result)
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
    #print(pred_class[x])
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
