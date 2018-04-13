require(randomForest)
require(dplyr)
require(RWeka)
# require(party)
require(verification)

kappa=function(x){
  n=sum(x)
  pobs=(x[1,1]+x[2,2])/n
  pexp=(sum(x[1,])*sum(x[,1])+sum(x[2,])*sum(x[,2]))/n^2
  kappa=(pobs-pexp)/(1-pexp)
  t1=0
  t2=0
  t3=0
  pii=x/n
  pidot=apply(pii,1,sum)
  pdotj=apply(pii,2,sum)
  for(i in 1:2){
    t1 = t1 + pii[i,i]*((1-pexp) - (1-pobs)*(pidot[i]+pdotj[i]))^2
  }
  t2 = pii[1,2]*(pdotj[1]+pidot[2])^2 + pii[2,1]*(pdotj[2] + pidot[1])^2
  t3 = (pobs*pexp-2*pexp+pobs)^2
  vhat = (t1 + t2*(1-pobs)^2 -t3)/(n*(1-pexp)^4)
  se=sqrt(vhat)
  return(c(kappa,se))
}
class.sum=function(truth,predicted){
  xt=table(truth,round(predicted+0.000001))
  pcc=round(100*sum(diag(xt))/sum(xt),2)
  spec=round(100*xt[1,1]/sum(xt[1,]),2)
  sens=round(100*xt[2,2]/sum(xt[2,]),2)
  kap=round(kappa(xt)[1],4)
  au=round(roc.area(truth,predicted)$A,4)
  list(round(c(pcc,spec,sens,kap,au),3))
}


H1B <- read.csv('./Project_2/H1B_Data/1. Master H1B Dataset.csv')
H1B_1 <- read.csv('./Project_2/H1B_Data/File 1 - H1B Dataset.csv')
H1B_2 <- read.csv('./Project_2/H1B_Data/File 2 - H1B Dataset.csv')
H1B_3 <- read.csv('./Project_2/H1B_Data/File 3 - H1B Dataset.csv')
H1B_4 <- read.csv('./Project_2/H1B_Data/File 4 - H1B Dataset.csv')
H1B_5 <- read.csv('./Project_2/H1B_Data/File 5 - H1B Dataset.csv')
H1B_6 <- read.csv('./Project_2/H1B_Data/File 6 - H1B Dataset.csv')
H1B_7 <- read.csv('./Project_2/H1B_Data/File 7 - H1B Dataset.csv')
H1B_8 <- read.csv('./Project_2/H1B_Data/File 8 - H1B Dataset.csv')
H1B_9 <- read.csv('./Project_2/H1B_Data/File 9 - H1B Dataset.csv')
H1B_10 <- read.csv('./Project_2/H1B_Data/File 10 - H1B Dataset.csv')

summary(H1B$CASE_STATUS)
summary(H1B_1$CASE_STATUS)


#########################################################
# File 1
#########################################################
str(H1B_1)

# str(H1B_1[,-8])

# lapply(H1B, class)

# # a bunch of NA values, but I don't think we should just delete them like this
# H1B_1 <- na.omit(H1B_1)

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

H1B_1$conv_PREVAILING_WAGE <- ifelse(H1B_1$PW_UNIT_OF_PAY == "Hour", H1B_1$PREVAILING_WAGE * 40 * 52,
                               ifelse(H1B_1$PW_UNIT_OF_PAY == "Week", H1B_1$PREVAILING_WAGE * 52,
                                      ifelse(H1B_1$PW_UNIT_OF_PAY == "Bi-Weekly", H1B_1$PREVAILING_WAGE * 26,
                                             ifelse(H1B_1$PW_UNIT_OF_PAY == "Month", H1B_1$PREVAILING_WAGE * 12,
                                                    H1B_1$PREVAILING_WAGE))))
H1B_1$conv_WAGE_RATE_OF_PAY_FROM <- ifelse(H1B_1$WAGE_UNIT_OF_PAY == "Hour", H1B_1$WAGE_RATE_OF_PAY_FROM * 40 * 52,
                                     ifelse(H1B_1$WAGE_UNIT_OF_PAY == "Week", H1B_1$WAGE_RATE_OF_PAY_FROM * 52,
                                            ifelse(H1B_1$WAGE_UNIT_OF_PAY == "Bi-Weekly", H1B_1$WAGE_RATE_OF_PAY_FROM * 26,
                                                   ifelse(H1B_1$WAGE_UNIT_OF_PAY == "Month", H1B_1$WAGE_RATE_OF_PAY_FROM * 12,
                                                          H1B_1$WAGE_RATE_OF_PAY_FROM))))
H1B_1$conv_WAGE_RATE_OF_PAY_TO <- ifelse(H1B_1$WAGE_UNIT_OF_PAY == "Hour", H1B_1$WAGE_RATE_OF_PAY_TO * 40 * 52,
                                      ifelse(H1B_1$WAGE_UNIT_OF_PAY == "Week", H1B_1$WAGE_RATE_OF_PAY_TO * 52,
                                             ifelse(H1B_1$WAGE_UNIT_OF_PAY == "Bi-Weekly", H1B_1$WAGE_RATE_OF_PAY_TO * 26,
                                                    ifelse(H1B_1$WAGE_UNIT_OF_PAY == "Month", H1B_1$WAGE_RATE_OF_PAY_TO * 12,
                                                           H1B_1$WAGE_RATE_OF_PAY_TO))))
  

#### Do we need to change prevailing wage so that they are on the same scale (this field would be correlated with pw_unit_of_pay)?
#### There are other int variables that we may need to convert to factors such as postal code or naics_code, but that would make it a variable with more than 53 variables...

H1B_1 <- na.omit(H1B_1)
# dir <- "G:/My Drive/school/Stats/MDATA Capstone/H1B data"
# write.csv(H1B_1, file = file.path(dir, "H1B_1.csv"))


### Random Forest - without converted salaries
H1B_1.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                        DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                        VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                        PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                        WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                        H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                          ntree = 100,
                          data = H1B_1)
H1B_1.rf #OOB = 22.62%
###################################################################################
# Confusion Matrix                                                                #
#                   CERTIFIED CERTIFIEDWITHDRAWN DENIED WITHDRAWN class.error     #
# CERTIFIED               5868                149    725       241  0.15967349    #
# CERTIFIEDWITHDRAWN       289               6504     89       101  0.06859516    #
# DENIED                  1223                 97   4384      1247  0.36929938    #
# WITHDRAWN                579                444   1127      4833  0.30789059    #
###################################################################################

# class.sum(H1B_1$CASE_STATUS, predict(H1B_1.rf, type = "prob")[ ,2])
#### Pcc:  ####

#Determine important Variables
varImpPlot(H1B_1.rf, main = "Variable Importance of Predicting Case Status")

# #Partial Dependence Plots
# partialPlot(H1B_1.rf, H1B_1, SOC_NAME, which.class = "1", main = "Partial Dependence Plot for SOC_NAME")


### Random Forest - with converted salaries
H1B_1.conv_wage.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                                  DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                                  VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                                  conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                                  conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                                  H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                    ntree = 100,
                                    data = H1B_1)
H1B_1.conv_wage.rf ##OOB: 22.55%

varImpPlot(H1B_1.conv_wage.rf, main = "Variable Importance of Predicting Case Status")

### J48
H1B_1.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                 DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                 VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                 PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                 WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                 H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                 data = H1B_1)
summary(H1B_1.j48)
##PCC: 86.3369 %

H1B_1.conv_wage.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                           DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                           VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                           conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                           conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                           H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                           data = H1B_1)
summary(H1B_1.conv_wage.j48)
##PCC: 86.1254 %



##############################################
# Divide data into training and testing sets #
##############################################

############ 80/20 split ############
set.seed(12345)
H1B_1.samp.80 <- sample(1:nrow(H1B_1), nrow(H1B_1)*(.8), replace = FALSE)
train.H1B_1.80 <- H1B_1[H1B_1.samp.80, ]
test.H1B_1.80 <- H1B_1[-H1B_1.samp.80, ]

### Random Forest - without converted salaries
H1B_1.samp80.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                              DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                              VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                              PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                              WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                              H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                                 ntree = 500,
                                 data = train.H1B_1.80)
H1B_1.samp80.rf #OOB = 22.76%

#Determine important Variables
varImpPlot(H1B_1.samp80.rf, main = "Variable Importance of Predicting Case Status")

H1B_1.samp80.rf.pred <- predict(H1B_1.samp80.rf, newdata = test.H1B_1.80)
table(H1B_1.samp80.rf.pred, test.H1B_1.80$CASE_STATUS)
(1161 + 1343 + 859 + 986) / nrow(test.H1B_1.80)
#PCC: 77.94%



### Random Forest - with converted salaries
H1B_1.conv_wage.samp80.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                     DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                     VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                     conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                     conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                     H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                   ntree = 500,
                                   data = train.H1B_1.80)
H1B_1.conv_wage.samp80.rf ##OOB: 22.36%

varImpPlot(H1B_1.conv_wage.samp80.rf, main = "Variable Importance of Predicting Case Status")

H1B_1.conv_wage.samp80.rf.pred <- predict(H1B_1.conv_wage.samp80.rf, newdata = test.H1B_1.80)
table(H1B_1.conv_wage.samp80.rf.pred, test.H1B_1.80$CASE_STATUS)
(1160 + 1340 + 871 + 981) / nrow(test.H1B_1.80)
#PCC: 77.99%

### J48
H1B_1.samp80.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                      DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                      VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                      PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                      WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                      H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                        data = train.H1B_1.80)
summary(H1B_1.samp80.j48)
H1B_1.samp80.j48.pred <- predict(H1B_1.samp80.j48, newdata = test.H1B_1.80)
table(H1B_1.samp80.j48.pred, test.H1B_1.80$CASE_STATUS)
(1128 + 1280 + 838 + 875) / nrow(test.H1B_1.80)
##PCC: 73.85%

H1B_1.conv_wage.samp80.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                                DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                                VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                                conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                                conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                                H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                  data = train.H1B_1.80)
summary(H1B_1.conv_wage.samp80.j48)
H1B_1.conv_wage.samp80.j48.pred <- predict(H1B_1.conv_wage.samp80.j48, newdata = test.H1B_1.80)
table(H1B_1.conv_wage.samp80.j48.pred, test.H1B_1.80$CASE_STATUS)
(1120 + 1282 + 842 + 866) / nrow(test.H1B_1.80)
##PCC: 73.66%


############ 66/34 split ############
set.seed(12345)
H1B_1.samp.66 <- sample(1:nrow(H1B_1), nrow(H1B_1)*(.66), replace = FALSE)
train.H1B_1.66 <- H1B_1[H1B_1.samp.66, ]
test.H1B_1.66 <- H1B_1[-H1B_1.samp.66, ]

### Random Forest - without converted salaries
H1B_1.samp66.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                  DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                  VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                  PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                  WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                  H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                                ntree = 500,
                                data = train.H1B_1.66)
H1B_1.samp66.rf #OOB = 23.31%

#Determine important Variables
varImpPlot(H1B_1.samp66.rf, main = "Variable Importance of Predicting Case Status")

H1B_1.samp66.rf.pred <- predict(H1B_1.samp66.rf, newdata = test.H1B_1.66)
table(H1B_1.samp66.rf.pred, test.H1B_1.66$CASE_STATUS)
(1978 + 2229 + 1445 + 1663) / nrow(test.H1B_1.66)
#PCC: 77.11%


### Random Forest - with converted salaries
H1B_1.conv_wage.samp66.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                            DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                            VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                            conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                            conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                            H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                          ntree = 500,
                                          data = train.H1B_1.66)
H1B_1.conv_wage.samp66.rf ##OOB: 23.05%

varImpPlot(H1B_1.conv_wage.samp66.rf, main = "Variable Importance of Predicting Case Status")

H1B_1.conv_wage.samp66.rf.pred <- predict(H1B_1.conv_wage.samp66.rf, newdata = test.H1B_1.66)
table(H1B_1.conv_wage.samp66.rf.pred, test.H1B_1.66$CASE_STATUS)
(1985 + 2238 + 1461 + 1654) / nrow(test.H1B_1.66)
#PCC: 77.36%

### J48
H1B_1.samp66.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                          DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                          VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                          PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                          WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                          H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                        data = train.H1B_1.66)
summary(H1B_1.samp66.j48)
H1B_1.samp66.j48.pred <- predict(H1B_1.samp66.j48, newdata = test.H1B_1.66)
table(H1B_1.samp66.j48.pred, test.H1B_1.66$CASE_STATUS)
(1895 + 2173 + 1328 + 1516) / nrow(test.H1B_1.66)
##PCC: 72.87%

H1B_1.conv_wage.samp66.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                    DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                    VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                    conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                    conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                    H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                  data = train.H1B_1.66)
summary(H1B_1.conv_wage.samp66.j48)
H1B_1.conv_wage.samp66.j48.pred <- predict(H1B_1.conv_wage.samp66.j48, newdata = test.H1B_1.66)
table(H1B_1.conv_wage.samp66.j48.pred, test.H1B_1.66$CASE_STATUS)
(1894 + 2176 + 1332 + 1483) / nrow(test.H1B_1.66)
##PCC: 72.58%




#########################################################
# File 2                                                #
#########################################################
H1B_2[H1B_2$SOC_NAME %in% c("MANAGERS","MANAGEMENT","FIRST LINE SUPERVISORS"), 11] <- "MANAGEMENT"
H1B_2[H1B_2$SOC_NAME %in% c("GRAPHIC DESIGNERS","FASHION DESIGNERS","DESIGNERS"), 11] <- "DESIGNERS"
H1B_2 <- droplevels(H1B_2)

#Create less categories for state according to: https://www2.census.gov/geo/pdfs/reference/GARM/Ch6GARM.pdf
H1B_2.divisions <- H1B_2 %>% arrange(EMPLOYER_STATE) %>%
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

H1B_2.divisions$EMPLOYER_DIVISION <- as.factor(H1B_2.divisions$EMPLOYER_DIVISION)
H1B_2.divisions$WORKSITE_DIVISION <- as.factor(H1B_2.divisions$WORKSITE_DIVISION)

H1B_2 <- H1B_2.divisions[,-c(8,9,19,25,26)]

str(H1B_2)

H1B_2$conv_PREVAILING_WAGE <- ifelse(H1B_2$PW_UNIT_OF_PAY == "Hour", H1B_2$PREVAILING_WAGE * 40 * 52,
                                     ifelse(H1B_2$PW_UNIT_OF_PAY == "Week", H1B_2$PREVAILING_WAGE * 52,
                                            ifelse(H1B_2$PW_UNIT_OF_PAY == "Bi-Weekly", H1B_2$PREVAILING_WAGE * 26,
                                                   ifelse(H1B_2$PW_UNIT_OF_PAY == "Month", H1B_2$PREVAILING_WAGE * 12,
                                                          H1B_2$PREVAILING_WAGE))))
H1B_2$conv_WAGE_RATE_OF_PAY_FROM <- ifelse(H1B_2$WAGE_UNIT_OF_PAY == "Hour", H1B_2$WAGE_RATE_OF_PAY_FROM * 40 * 52,
                                           ifelse(H1B_2$WAGE_UNIT_OF_PAY == "Week", H1B_2$WAGE_RATE_OF_PAY_FROM * 52,
                                                  ifelse(H1B_2$WAGE_UNIT_OF_PAY == "Bi-Weekly", H1B_2$WAGE_RATE_OF_PAY_FROM * 26,
                                                         ifelse(H1B_2$WAGE_UNIT_OF_PAY == "Month", H1B_2$WAGE_RATE_OF_PAY_FROM * 12,
                                                                H1B_2$WAGE_RATE_OF_PAY_FROM))))
H1B_2$conv_WAGE_RATE_OF_PAY_TO <- ifelse(H1B_2$WAGE_UNIT_OF_PAY == "Hour", H1B_2$WAGE_RATE_OF_PAY_TO * 40 * 52,
                                         ifelse(H1B_2$WAGE_UNIT_OF_PAY == "Week", H1B_2$WAGE_RATE_OF_PAY_TO * 52,
                                                ifelse(H1B_2$WAGE_UNIT_OF_PAY == "Bi-Weekly", H1B_2$WAGE_RATE_OF_PAY_TO * 26,
                                                       ifelse(H1B_2$WAGE_UNIT_OF_PAY == "Month", H1B_2$WAGE_RATE_OF_PAY_TO * 12,
                                                              H1B_2$WAGE_RATE_OF_PAY_TO))))

H1B_2 <- na.omit(H1B_2)

### Random Forest - without converted salaries
H1B_2.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                            DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                            VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                            PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                            WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                            H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                          ntree = 100,
                          data = H1B_2)
H1B_2.rf ##OOB: 22.99%

### Random Forest - with converted salaries
H1B_2.conv_wage.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                      DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                      VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                      conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                      conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                      H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                    ntree = 100,
                                    data = H1B_2)
H1B_2.conv_wage.rf ##OOB: 22.66%

### J48
H1B_2.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                   DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                   VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                   PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                   WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                   H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                 data = H1B_2)
summary(H1B_2.j48) #PCC: 85.1685%

H1B_2.conv_wage.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                             DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                             VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                             conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                             conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                             H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                           data = H1B_2)
summary(H1B_2.conv_wage.j48) ##PCC: 85.3978 %


##############################################
# Divide data into training and testing sets #
##############################################

############ 80/20 split ############
set.seed(12345)
H1B_2.samp.80 <- sample(1:nrow(H1B_2), nrow(H1B_2)*(.8), replace = FALSE)
train.H1B_2.80 <- H1B_2[H1B_2.samp.80, ]
test.H1B_2.80 <- H1B_2[-H1B_2.samp.80, ]

### Random Forest - without converted salaries
H1B_2.samp80.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                  DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                  VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                  PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                  WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                  H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                                ntree = 500,
                                data = train.H1B_2.80)
H1B_2.samp80.rf #OOB = 23.45%

H1B_2.samp80.rf.pred <- predict(H1B_2.samp80.rf, newdata = test.H1B_2.80)
table(H1B_2.samp80.rf.pred, test.H1B_2.80$CASE_STATUS)
(1171 + 1274 + 869 + 975) / nrow(test.H1B_2.80)
#PCC: 76.86%



### Random Forest - with converted salaries
H1B_2.conv_wage.samp80.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                            DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                            VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                            conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                            conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                            H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                          ntree = 500,
                                          data = train.H1B_2.80)
H1B_2.conv_wage.samp80.rf ##OOB: 22.54%

H1B_2.conv_wage.samp80.rf.pred <- predict(H1B_2.conv_wage.samp80.rf, newdata = test.H1B_2.80)
table(H1B_2.conv_wage.samp80.rf.pred, test.H1B_2.80$CASE_STATUS)
(1187 + 1281 + 887 + 967) / nrow(test.H1B_2.80)
#PCC: 77.46%

### J48
H1B_2.samp80.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                          DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                          VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                          PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                          WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                          H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                        data = train.H1B_2.80)
summary(H1B_2.samp80.j48)
H1B_2.samp80.j48.pred <- predict(H1B_2.samp80.j48, newdata = test.H1B_2.80)
table(H1B_2.samp80.j48.pred, test.H1B_2.80$CASE_STATUS)
(1158 + 1259 + 780 + 913) / nrow(test.H1B_2.80)
##PCC: 73.66%

H1B_2.conv_wage.samp80.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                    DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                    VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                    conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                    conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                    H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                  data = train.H1B_2.80)
summary(H1B_2.conv_wage.samp80.j48)
H1B_2.conv_wage.samp80.j48.pred <- predict(H1B_2.conv_wage.samp80.j48, newdata = test.H1B_2.80)
table(H1B_2.conv_wage.samp80.j48.pred, test.H1B_2.80$CASE_STATUS)
(1168 + 1262 + 779 + 902) / nrow(test.H1B_2.80)
##PCC: 73.67%


############ 66/34 split ############
set.seed(12345)
H1B_2.samp.66 <- sample(1:nrow(H1B_2), nrow(H1B_2)*(.66), replace = FALSE)
train.H1B_2.66 <- H1B_2[H1B_2.samp.66, ]
test.H1B_2.66 <- H1B_2[-H1B_2.samp.66, ]

### Random Forest - without converted salaries
H1B_2.samp66.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                  DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                  VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                  PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                  WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                  H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                                ntree = 500,
                                data = train.H1B_2.66)
H1B_2.samp66.rf #OOB = 23.46%

H1B_2.samp66.rf.pred <- predict(H1B_2.samp66.rf, newdata = test.H1B_2.66)
table(H1B_2.samp66.rf.pred, test.H1B_2.66$CASE_STATUS)
(2002 + 2171 + 1515 + 1624) / nrow(test.H1B_2.66)
#PCC: 77.08%


### Random Forest - with converted salaries
H1B_2.conv_wage.samp66.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                            DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                            VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                            conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                            conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                            H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                          ntree = 500,
                                          data = train.H1B_2.66)
H1B_2.conv_wage.samp66.rf ##OOB: 23.22%

H1B_2.conv_wage.samp66.rf.pred <- predict(H1B_2.conv_wage.samp66.rf, newdata = test.H1B_2.66)
table(H1B_2.conv_wage.samp66.rf.pred, test.H1B_2.66$CASE_STATUS)
(2006 + 2184 + 1531 + 1640) / nrow(test.H1B_2.66)
#PCC: 77.60%

### J48
H1B_2.samp66.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                          DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                          VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                          PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                          WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                          H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                        data = train.H1B_2.66)
summary(H1B_2.samp66.j48)
H1B_2.samp66.j48.pred <- predict(H1B_2.samp66.j48, newdata = test.H1B_2.66)
table(H1B_2.samp66.j48.pred, test.H1B_2.66$CASE_STATUS)
(1923 + 2146 + 1331 + 1443) / nrow(test.H1B_2.66)
##PCC: 72.14%

H1B_2.conv_wage.samp66.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                    DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                    VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                    conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                    conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                    H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                  data = train.H1B_2.66)
summary(H1B_2.conv_wage.samp66.j48)
H1B_2.conv_wage.samp66.j48.pred <- predict(H1B_2.conv_wage.samp66.j48, newdata = test.H1B_2.66)
table(H1B_2.conv_wage.samp66.j48.pred, test.H1B_2.66$CASE_STATUS)
(1913 + 2131 + 1344 + 1470) / nrow(test.H1B_2.66)
##PCC: 72.30%





#########################################################
# File 3                                                #
#########################################################
H1B_3[H1B_3$SOC_NAME %in% c("MANAGERS","MANAGEMENT","FIRST LINE SUPERVISORS"), 11] <- "MANAGEMENT"
H1B_3[H1B_3$SOC_NAME %in% c("GRAPHIC DESIGNERS","FASHION DESIGNERS","DESIGNERS"), 11] <- "DESIGNERS"
H1B_3 <- droplevels(H1B_3)

#Create less categories for state according to: https://www2.census.gov/geo/pdfs/reference/GARM/Ch6GARM.pdf
H1B_3.divisions <- H1B_3 %>% arrange(EMPLOYER_STATE) %>%
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

H1B_3.divisions$EMPLOYER_DIVISION <- as.factor(H1B_3.divisions$EMPLOYER_DIVISION)
H1B_3.divisions$WORKSITE_DIVISION <- as.factor(H1B_3.divisions$WORKSITE_DIVISION)

H1B_3 <- H1B_3.divisions[,-c(8,9,19,25,26)]

str(H1B_3)

H1B_3$conv_PREVAILING_WAGE <- ifelse(H1B_3$PW_UNIT_OF_PAY == "Hour", H1B_3$PREVAILING_WAGE * 40 * 52,
                                     ifelse(H1B_3$PW_UNIT_OF_PAY == "Week", H1B_3$PREVAILING_WAGE * 52,
                                            ifelse(H1B_3$PW_UNIT_OF_PAY == "Bi-Weekly", H1B_3$PREVAILING_WAGE * 26,
                                                   ifelse(H1B_3$PW_UNIT_OF_PAY == "Month", H1B_3$PREVAILING_WAGE * 12,
                                                          H1B_3$PREVAILING_WAGE))))
H1B_3$conv_WAGE_RATE_OF_PAY_FROM <- ifelse(H1B_3$WAGE_UNIT_OF_PAY == "Hour", H1B_3$WAGE_RATE_OF_PAY_FROM * 40 * 52,
                                           ifelse(H1B_3$WAGE_UNIT_OF_PAY == "Week", H1B_3$WAGE_RATE_OF_PAY_FROM * 52,
                                                  ifelse(H1B_3$WAGE_UNIT_OF_PAY == "Bi-Weekly", H1B_3$WAGE_RATE_OF_PAY_FROM * 26,
                                                         ifelse(H1B_3$WAGE_UNIT_OF_PAY == "Month", H1B_3$WAGE_RATE_OF_PAY_FROM * 12,
                                                                H1B_3$WAGE_RATE_OF_PAY_FROM))))
H1B_3$conv_WAGE_RATE_OF_PAY_TO <- ifelse(H1B_3$WAGE_UNIT_OF_PAY == "Hour", H1B_3$WAGE_RATE_OF_PAY_TO * 40 * 52,
                                         ifelse(H1B_3$WAGE_UNIT_OF_PAY == "Week", H1B_3$WAGE_RATE_OF_PAY_TO * 52,
                                                ifelse(H1B_3$WAGE_UNIT_OF_PAY == "Bi-Weekly", H1B_3$WAGE_RATE_OF_PAY_TO * 26,
                                                       ifelse(H1B_3$WAGE_UNIT_OF_PAY == "Month", H1B_3$WAGE_RATE_OF_PAY_TO * 12,
                                                              H1B_3$WAGE_RATE_OF_PAY_TO))))
H1B_3 <- na.omit(H1B_3)

### Random Forest - without converted salaries
H1B_3.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                            DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                            VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                            PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                            WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                            H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                          ntree = 100,
                          data = H1B_3)
H1B_3.rf ##OOB: 23.26%

### Random Forest - with converted salaries
H1B_3.conv_wage.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                      DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                      VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                      conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                      conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                      H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                    ntree = 100,
                                    data = H1B_3)
H1B_3.conv_wage.rf ##OOB: 77.23%

### J48
H1B_3.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                   DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                   VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                   PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                   WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                   H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                 data = H1B_3)
summary(H1B_3.j48) #PCC: 85.1541%

H1B_3.conv_wage.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                             DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                             VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                             conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                             conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                             H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                           data = H1B_3)
summary(H1B_3.conv_wage.j48) #PCC: 85.2366 %


##############################################
# Divide data into training and testing sets #
##############################################

############ 80/20 split ############
set.seed(12345)
H1B_3.samp.80 <- sample(1:nrow(H1B_3), nrow(H1B_3)*(.8), replace = FALSE)
train.H1B_3.80 <- H1B_3[H1B_3.samp.80, ]
test.H1B_3.80 <- H1B_3[-H1B_3.samp.80, ]

### Random Forest - without converted salaries
H1B_3.samp80.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                  DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                  VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                  PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                  WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                  H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                                ntree = 500,
                                data = train.H1B_3.80)
H1B_3.samp80.rf #OOB = 23.09%

H1B_3.samp80.rf.pred <- predict(H1B_3.samp80.rf, newdata = test.H1B_3.80)
table(H1B_3.samp80.rf.pred, test.H1B_3.80$CASE_STATUS)
(1154 + 1303 + 884 + 972) / nrow(test.H1B_3.80)
#PCC: 77.29%



### Random Forest - with converted salaries
H1B_3.conv_wage.samp80.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                            DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                            VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                            conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                            conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                            H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                          ntree = 500,
                                          data = train.H1B_3.80)
H1B_3.conv_wage.samp80.rf ##OOB: 22.78%

H1B_3.conv_wage.samp80.rf.pred <- predict(H1B_3.conv_wage.samp80.rf, newdata = test.H1B_3.80)
table(H1B_3.conv_wage.samp80.rf.pred, test.H1B_3.80$CASE_STATUS)
(1151 + 1296 + 889 + 969) / nrow(test.H1B_3.80)
#PCC: 77.15%

### J48
H1B_3.samp80.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                          DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                          VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                          PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                          WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                          H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                        data = train.H1B_3.80)
summary(H1B_3.samp80.j48)
H1B_3.samp80.j48.pred <- predict(H1B_3.samp80.j48, newdata = test.H1B_3.80)
table(H1B_3.samp80.j48.pred, test.H1B_3.80$CASE_STATUS)
(1090 + 1265 + 792 + 894) / nrow(test.H1B_3.80)
##PCC: 72.42%

H1B_3.conv_wage.samp80.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                    DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                    VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                    conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                    conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                    H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                  data = train.H1B_3.80)
summary(H1B_3.conv_wage.samp80.j48)
H1B_3.conv_wage.samp80.j48.pred <- predict(H1B_3.conv_wage.samp80.j48, newdata = test.H1B_3.80)
table(H1B_3.conv_wage.samp80.j48.pred, test.H1B_3.80$CASE_STATUS)
(1116 + 1242 + 798 + 899) / nrow(test.H1B_3.80)
##PCC: 72.67%


############ 66/34 split ############
set.seed(12345)
H1B_3.samp.66 <- sample(1:nrow(H1B_3), nrow(H1B_3)*(.66), replace = FALSE)
train.H1B_3.66 <- H1B_3[H1B_3.samp.66, ]
test.H1B_3.66 <- H1B_3[-H1B_3.samp.66, ]

### Random Forest - without converted salaries
H1B_3.samp66.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                  DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                  VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                  PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                  WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                  H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                                ntree = 500,
                                data = train.H1B_3.66)
H1B_3.samp66.rf #OOB = 23.49%

H1B_3.samp66.rf.pred <- predict(H1B_3.samp66.rf, newdata = test.H1B_3.66)
table(H1B_3.samp66.rf.pred, test.H1B_3.66$CASE_STATUS)
(2003 + 2221 + 1459 + 1594) / nrow(test.H1B_3.66)
#PCC: 76.71%


### Random Forest - with converted salaries
H1B_3.conv_wage.samp66.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                            DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                            VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                            conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                            conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                            H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                          ntree = 500,
                                          data = train.H1B_3.66)
H1B_3.conv_wage.samp66.rf ##OOB: 23.4%

H1B_3.conv_wage.samp66.rf.pred <- predict(H1B_3.conv_wage.samp66.rf, newdata = test.H1B_3.66)
table(H1B_3.conv_wage.samp66.rf.pred, test.H1B_3.66$CASE_STATUS)
(2005 + 2223 + 1452 + 1594) / nrow(test.H1B_3.66)
#PCC: 76.68%

### J48
H1B_3.samp66.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                          DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                          VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                          PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                          WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                          H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                        data = train.H1B_3.66)
summary(H1B_3.samp66.j48)
H1B_3.samp66.j48.pred <- predict(H1B_3.samp66.j48, newdata = test.H1B_3.66)
table(H1B_3.samp66.j48.pred, test.H1B_3.66$CASE_STATUS)
(1907 + 2133 + 1241 + 1489) / nrow(test.H1B_3.66)
##PCC: 71.37%

H1B_3.conv_wage.samp66.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                    DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                    VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                    conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                    conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                    H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                  data = train.H1B_3.66)
summary(H1B_3.conv_wage.samp66.j48)
H1B_3.conv_wage.samp66.j48.pred <- predict(H1B_3.conv_wage.samp66.j48, newdata = test.H1B_3.66)
table(H1B_3.conv_wage.samp66.j48.pred, test.H1B_3.66$CASE_STATUS)
(1927 + 2140 + 1255 + 1488) / nrow(test.H1B_3.66)
##PCC: 71.79%




#########################################################
# File 4                                                #
#########################################################
H1B_4[H1B_4$SOC_NAME %in% c("MANAGERS","MANAGEMENT","FIRST LINE SUPERVISORS"), 11] <- "MANAGEMENT"
H1B_4[H1B_4$SOC_NAME %in% c("GRAPHIC DESIGNERS","FASHION DESIGNERS","DESIGNERS"), 11] <- "DESIGNERS"
H1B_4 <- droplevels(H1B_4)

#Create less categories for state according to: https://www2.census.gov/geo/pdfs/reference/GARM/Ch6GARM.pdf
H1B_4.divisions <- H1B_4 %>% arrange(EMPLOYER_STATE) %>%
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

H1B_4.divisions$EMPLOYER_DIVISION <- as.factor(H1B_4.divisions$EMPLOYER_DIVISION)
H1B_4.divisions$WORKSITE_DIVISION <- as.factor(H1B_4.divisions$WORKSITE_DIVISION)

H1B_4 <- H1B_4.divisions[,-c(8,9,19,25,26)]

str(H1B_4)

H1B_4$conv_PREVAILING_WAGE <- ifelse(H1B_4$PW_UNIT_OF_PAY == "Hour", H1B_4$PREVAILING_WAGE * 40 * 52,
                                     ifelse(H1B_4$PW_UNIT_OF_PAY == "Week", H1B_4$PREVAILING_WAGE * 52,
                                            ifelse(H1B_4$PW_UNIT_OF_PAY == "Bi-Weekly", H1B_4$PREVAILING_WAGE * 26,
                                                   ifelse(H1B_4$PW_UNIT_OF_PAY == "Month", H1B_4$PREVAILING_WAGE * 12,
                                                          H1B_4$PREVAILING_WAGE))))
H1B_4$conv_WAGE_RATE_OF_PAY_FROM <- ifelse(H1B_4$WAGE_UNIT_OF_PAY == "Hour", H1B_4$WAGE_RATE_OF_PAY_FROM * 40 * 52,
                                           ifelse(H1B_4$WAGE_UNIT_OF_PAY == "Week", H1B_4$WAGE_RATE_OF_PAY_FROM * 52,
                                                  ifelse(H1B_4$WAGE_UNIT_OF_PAY == "Bi-Weekly", H1B_4$WAGE_RATE_OF_PAY_FROM * 26,
                                                         ifelse(H1B_4$WAGE_UNIT_OF_PAY == "Month", H1B_4$WAGE_RATE_OF_PAY_FROM * 12,
                                                                H1B_4$WAGE_RATE_OF_PAY_FROM))))
H1B_4$conv_WAGE_RATE_OF_PAY_TO <- ifelse(H1B_4$WAGE_UNIT_OF_PAY == "Hour", H1B_4$WAGE_RATE_OF_PAY_TO * 40 * 52,
                                         ifelse(H1B_4$WAGE_UNIT_OF_PAY == "Week", H1B_4$WAGE_RATE_OF_PAY_TO * 52,
                                                ifelse(H1B_4$WAGE_UNIT_OF_PAY == "Bi-Weekly", H1B_4$WAGE_RATE_OF_PAY_TO * 26,
                                                       ifelse(H1B_4$WAGE_UNIT_OF_PAY == "Month", H1B_4$WAGE_RATE_OF_PAY_TO * 12,
                                                              H1B_4$WAGE_RATE_OF_PAY_TO))))

H1B_4 <- na.omit(H1B_4)

### Random Forest - without converted salaries
H1B_4.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                            DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                            VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                            PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                            WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                            H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                          ntree = 100,
                          data = H1B_4)
H1B_4.rf #OOB: 22.8%

### Random Forest - with converted salaries
H1B_4.conv_wage.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                      DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                      VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                      conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                      conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                      H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                    ntree = 100,
                                    data = H1B_4)
H1B_4.conv_wage.rf #OOB: 22.79%

### J48
H1B_4.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                   DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                   VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                   PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                   WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                   H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                 data = H1B_4)
summary(H1B_4.j48) #PCC:85.6022 %

H1B_4.conv_wage.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                             DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                             VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                             conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                             conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                             H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                           data = H1B_4)
summary(H1B_4.conv_wage.j48) #PCC: 85.7312 %

##############################################
# Divide data into training and testing sets #
##############################################

############ 80/20 split ############
set.seed(12345)
H1B_4.samp.80 <- sample(1:nrow(H1B_4), nrow(H1B_4)*(.8), replace = FALSE)
train.H1B_4.80 <- H1B_4[H1B_4.samp.80, ]
test.H1B_4.80 <- H1B_4[-H1B_4.samp.80, ]

### Random Forest - without converted salaries
H1B_4.samp80.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                  DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                  VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                  PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                  WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                  H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                                ntree = 500,
                                data = train.H1B_4.80)
H1B_4.samp80.rf #OOB = 22.29%

H1B_4.samp80.rf.pred <- predict(H1B_4.samp80.rf, newdata = test.H1B_4.80)
table(H1B_4.samp80.rf.pred, test.H1B_4.80$CASE_STATUS)
(1120 + 1339 + 871 + 962) / nrow(test.H1B_4.80)
#PCC: 76.92%



### Random Forest - with converted salaries
H1B_4.conv_wage.samp80.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                            DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                            VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                            conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                            conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                            H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                          ntree = 500,
                                          data = train.H1B_4.80)
H1B_4.conv_wage.samp80.rf ##OOB: 22.02%

H1B_4.conv_wage.samp80.rf.pred <- predict(H1B_4.conv_wage.samp80.rf, newdata = test.H1B_4.80)
table(H1B_4.conv_wage.samp80.rf.pred, test.H1B_4.80$CASE_STATUS)
(1114 + 1342 + 882 + 962) / nrow(test.H1B_4.80)
#PCC: 77.06%

### J48
H1B_4.samp80.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                          DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                          VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                          PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                          WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                          H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                        data = train.H1B_4.80)
summary(H1B_4.samp80.j48)
H1B_4.samp80.j48.pred <- predict(H1B_4.samp80.j48, newdata = test.H1B_4.80)
table(H1B_4.samp80.j48.pred, test.H1B_4.80$CASE_STATUS)
(1086 + 1304 + 889 + 854) / nrow(test.H1B_4.80)
##PCC: 74.07%

H1B_4.conv_wage.samp80.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                    DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                    VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                    conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                    conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                    H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                  data = train.H1B_4.80)
summary(H1B_4.conv_wage.samp80.j48)
H1B_4.conv_wage.samp80.j48.pred <- predict(H1B_4.conv_wage.samp80.j48, newdata = test.H1B_4.80)
table(H1B_4.conv_wage.samp80.j48.pred, test.H1B_4.80$CASE_STATUS)
(1097 + 1316 + 836 + 851) / nrow(test.H1B_4.80)
##PCC: 73.48%


############ 66/34 split ############
set.seed(12345)
H1B_4.samp.66 <- sample(1:nrow(H1B_4), nrow(H1B_4)*(.66), replace = FALSE)
train.H1B_4.66 <- H1B_4[H1B_4.samp.66, ]
test.H1B_4.66 <- H1B_4[-H1B_4.samp.66, ]

### Random Forest - without converted salaries
H1B_4.samp66.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                  DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                  VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                  PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                  WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                  H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                                ntree = 500,
                                data = train.H1B_4.66)
H1B_4.samp66.rf #OOB = 22.69%

H1B_4.samp66.rf.pred <- predict(H1B_4.samp66.rf, newdata = test.H1B_4.66)
table(H1B_4.samp66.rf.pred, test.H1B_4.66$CASE_STATUS)
(1959 + 2228 + 1497 + 1570) / nrow(test.H1B_4.66)
#PCC: 76.47%


### Random Forest - with converted salaries
H1B_4.conv_wage.samp66.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                            DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                            VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                            conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                            conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                            H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                          ntree = 500,
                                          data = train.H1B_4.66)
H1B_4.conv_wage.samp66.rf ##OOB: 22.8%

H1B_4.conv_wage.samp66.rf.pred <- predict(H1B_4.conv_wage.samp66.rf, newdata = test.H1B_4.66)
table(H1B_4.conv_wage.samp66.rf.pred, test.H1B_4.66$CASE_STATUS)
(1963 + 2224 + 1502 + 1566) / nrow(test.H1B_4.66)
#PCC: 76.48%

### J48
H1B_4.samp66.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                          DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                          VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                          PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                          WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                          H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                        data = train.H1B_4.66)
summary(H1B_4.samp66.j48)
H1B_4.samp66.j48.pred <- predict(H1B_4.samp66.j48, newdata = test.H1B_4.66)
table(H1B_4.samp66.j48.pred, test.H1B_4.66$CASE_STATUS)
(1930 + 2116 + 1297 + 1373) / nrow(test.H1B_4.66)
##PCC: 70.80%

H1B_4.conv_wage.samp66.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                    DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                    VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                    conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                    conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                    H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                  data = train.H1B_4.66)
summary(H1B_4.conv_wage.samp66.j48)
H1B_4.conv_wage.samp66.j48.pred <- predict(H1B_4.conv_wage.samp66.j48, newdata = test.H1B_4.66)
table(H1B_4.conv_wage.samp66.j48.pred, test.H1B_4.66$CASE_STATUS)
(1933 + 2124 + 1253 + 1409) / nrow(test.H1B_4.66)
##PCC: 70.83%




#########################################################
# File 5                                                #
#########################################################
H1B_5[H1B_5$SOC_NAME %in% c("MANAGERS","MANAGEMENT","FIRST LINE SUPERVISORS"), 11] <- "MANAGEMENT"
H1B_5[H1B_5$SOC_NAME %in% c("GRAPHIC DESIGNERS","FASHION DESIGNERS","DESIGNERS"), 11] <- "DESIGNERS"
H1B_5 <- droplevels(H1B_5)

#Create less categories for state according to: https://www2.census.gov/geo/pdfs/reference/GARM/Ch6GARM.pdf
H1B_5.divisions <- H1B_5 %>% arrange(EMPLOYER_STATE) %>%
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

H1B_5.divisions$EMPLOYER_DIVISION <- as.factor(H1B_5.divisions$EMPLOYER_DIVISION)
H1B_5.divisions$WORKSITE_DIVISION <- as.factor(H1B_5.divisions$WORKSITE_DIVISION)

H1B_5 <- H1B_5.divisions[,-c(8,9,19,25,26)]

str(H1B_5)

H1B_5$conv_PREVAILING_WAGE <- ifelse(H1B_5$PW_UNIT_OF_PAY == "Hour", H1B_5$PREVAILING_WAGE * 40 * 52,
                                     ifelse(H1B_5$PW_UNIT_OF_PAY == "Week", H1B_5$PREVAILING_WAGE * 52,
                                            ifelse(H1B_5$PW_UNIT_OF_PAY == "Bi-Weekly", H1B_5$PREVAILING_WAGE * 26,
                                                   ifelse(H1B_5$PW_UNIT_OF_PAY == "Month", H1B_5$PREVAILING_WAGE * 12,
                                                          H1B_5$PREVAILING_WAGE))))
H1B_5$conv_WAGE_RATE_OF_PAY_FROM <- ifelse(H1B_5$WAGE_UNIT_OF_PAY == "Hour", H1B_5$WAGE_RATE_OF_PAY_FROM * 40 * 52,
                                           ifelse(H1B_5$WAGE_UNIT_OF_PAY == "Week", H1B_5$WAGE_RATE_OF_PAY_FROM * 52,
                                                  ifelse(H1B_5$WAGE_UNIT_OF_PAY == "Bi-Weekly", H1B_5$WAGE_RATE_OF_PAY_FROM * 26,
                                                         ifelse(H1B_5$WAGE_UNIT_OF_PAY == "Month", H1B_5$WAGE_RATE_OF_PAY_FROM * 12,
                                                                H1B_5$WAGE_RATE_OF_PAY_FROM))))
H1B_5$conv_WAGE_RATE_OF_PAY_TO <- ifelse(H1B_5$WAGE_UNIT_OF_PAY == "Hour", H1B_5$WAGE_RATE_OF_PAY_TO * 40 * 52,
                                         ifelse(H1B_5$WAGE_UNIT_OF_PAY == "Week", H1B_5$WAGE_RATE_OF_PAY_TO * 52,
                                                ifelse(H1B_5$WAGE_UNIT_OF_PAY == "Bi-Weekly", H1B_5$WAGE_RATE_OF_PAY_TO * 26,
                                                       ifelse(H1B_5$WAGE_UNIT_OF_PAY == "Month", H1B_5$WAGE_RATE_OF_PAY_TO * 12,
                                                              H1B_5$WAGE_RATE_OF_PAY_TO))))

H1B_5 <- na.omit(H1B_5)

### Random Forest - without converted salaries
H1B_5.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                            DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                            VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                            PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                            WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                            H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                          ntree = 100,
                          data = H1B_5)
H1B_5.rf #OOB: 23.09%

### Random Forest - with converted salaries
H1B_5.conv_wage.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                      DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                      VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                      conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                      conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                      H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                    ntree = 100,
                                    data = H1B_5)
H1B_5.conv_wage.rf #OOB: 22.65%

### J48
H1B_5.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                   DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                   VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                   PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                   WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                   H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                 data = H1B_5)
summary(H1B_5.j48) #PCC: 86.1004 %

H1B_5.conv_wage.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                             DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                             VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                             conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                             conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                             H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                           data = H1B_5)
summary(H1B_5.conv_wage.j48) #PCC: 85.8566 %


##############################################
# Divide data into training and testing sets #
##############################################

############ 80/20 split ############
set.seed(12345)
H1B_5.samp.80 <- sample(1:nrow(H1B_5), nrow(H1B_5)*(.8), replace = FALSE)
train.H1B_5.80 <- H1B_5[H1B_5.samp.80, ]
test.H1B_5.80 <- H1B_5[-H1B_5.samp.80, ]

### Random Forest - without converted salaries
H1B_5.samp80.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                  DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                  VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                  PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                  WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                  H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                                ntree = 500,
                                data = train.H1B_5.80)
H1B_5.samp80.rf #OOB = 22.93%

H1B_5.samp80.rf.pred <- predict(H1B_5.samp80.rf, newdata = test.H1B_5.80)
table(H1B_5.samp80.rf.pred, test.H1B_5.80$CASE_STATUS)
(1170 + 1309 + 869 + 993) / nrow(test.H1B_5.80)
#PCC: 77.80%



### Random Forest - with converted salaries
H1B_5.conv_wage.samp80.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                            DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                            VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                            conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                            conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                            H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                          ntree = 500,
                                          data = train.H1B_5.80)
H1B_5.conv_wage.samp80.rf ##OOB: 22.72%

H1B_5.conv_wage.samp80.rf.pred <- predict(H1B_5.conv_wage.samp80.rf, newdata = test.H1B_5.80)
table(H1B_5.conv_wage.samp80.rf.pred, test.H1B_5.80$CASE_STATUS)
(1172 + 1314 + 894 + 998) / nrow(test.H1B_5.80)
#PCC: 78.46%

### J48
H1B_5.samp80.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                          DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                          VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                          PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                          WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                          H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                        data = train.H1B_5.80)
summary(H1B_5.samp80.j48)
H1B_5.samp80.j48.pred <- predict(H1B_5.samp80.j48, newdata = test.H1B_5.80)
table(H1B_5.samp80.j48.pred, test.H1B_5.80$CASE_STATUS)
(1080 + 1279 + 808 + 918) / nrow(test.H1B_5.80)
##PCC: 73.821%

H1B_5.conv_wage.samp80.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                    DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                    VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                    conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                    conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                    H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                  data = train.H1B_5.80)
summary(H1B_5.conv_wage.samp80.j48)
H1B_5.conv_wage.samp80.j48.pred <- predict(H1B_5.conv_wage.samp80.j48, newdata = test.H1B_5.80)
table(H1B_5.conv_wage.samp80.j48.pred, test.H1B_5.80$CASE_STATUS)
(1074 + 1274 + 820 + 927) / nrow(test.H1B_5.80)
##PCC: 73.39%


############ 66/34 split ############
set.seed(12345)
H1B_5.samp.66 <- sample(1:nrow(H1B_5), nrow(H1B_5)*(.66), replace = FALSE)
train.H1B_5.66 <- H1B_5[H1B_5.samp.66, ]
test.H1B_5.66 <- H1B_5[-H1B_5.samp.66, ]

### Random Forest - without converted salaries
H1B_5.samp66.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                  DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                  VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                  PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                  WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                  H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                                ntree = 500,
                                data = train.H1B_5.66)
H1B_5.samp66.rf #OOB = 23.36%

H1B_5.samp66.rf.pred <- predict(H1B_5.samp66.rf, newdata = test.H1B_5.66)
table(H1B_5.samp66.rf.pred, test.H1B_5.66$CASE_STATUS)
(1959 + 2226 + 1471 + 1629) / nrow(test.H1B_5.66)
#PCC: 76.79%


### Random Forest - with converted salaries
H1B_5.conv_wage.samp66.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                            DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                            VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                            conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                            conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                            H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                          ntree = 500,
                                          data = train.H1B_5.66)
H1B_5.conv_wage.samp66.rf ##OOB: 23.42%

H1B_5.conv_wage.samp66.rf.pred <- predict(H1B_5.conv_wage.samp66.rf, newdata = test.H1B_5.66)
table(H1B_5.conv_wage.samp66.rf.pred, test.H1B_5.66$CASE_STATUS)
(1960 + 2232 + 1492 + 1628) / nrow(test.H1B_5.66)
#PCC: 77.08%

### J48
H1B_5.samp66.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                          DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                          VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                          PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                          WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                          H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                        data = train.H1B_5.66)
summary(H1B_5.samp66.j48)
H1B_5.samp66.j48.pred <- predict(H1B_5.samp66.j48, newdata = test.H1B_5.66)
table(H1B_5.samp66.j48.pred, test.H1B_5.66$CASE_STATUS)
(1812 + 2177 + 1346 + 1475) / nrow(test.H1B_5.66)
##PCC: 71.79%

H1B_5.conv_wage.samp66.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                    DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                    VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                    conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                    conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                    H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                  data = train.H1B_5.66)
summary(H1B_5.conv_wage.samp66.j48)
H1B_5.conv_wage.samp66.j48.pred <- predict(H1B_5.conv_wage.samp66.j48, newdata = test.H1B_5.66)
table(H1B_5.conv_wage.samp66.j48.pred, test.H1B_5.66$CASE_STATUS)
(1816 + 2176 + 1361 + 1481) / nrow(test.H1B_5.66)
##PCC: 72.04%




#########################################################
# File 6                                                #
#########################################################
H1B_6[H1B_6$SOC_NAME %in% c("MANAGERS","MANAGEMENT","FIRST LINE SUPERVISORS"), 11] <- "MANAGEMENT"
H1B_6[H1B_6$SOC_NAME %in% c("GRAPHIC DESIGNERS","FASHION DESIGNERS","DESIGNERS"), 11] <- "DESIGNERS"
H1B_6 <- droplevels(H1B_6)

#Create less categories for state according to: https://www2.census.gov/geo/pdfs/reference/GARM/Ch6GARM.pdf
H1B_6.divisions <- H1B_6 %>% arrange(EMPLOYER_STATE) %>%
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

H1B_6.divisions$EMPLOYER_DIVISION <- as.factor(H1B_6.divisions$EMPLOYER_DIVISION)
H1B_6.divisions$WORKSITE_DIVISION <- as.factor(H1B_6.divisions$WORKSITE_DIVISION)

H1B_6 <- H1B_6.divisions[,-c(8,9,19,25,26)]

str(H1B_6)

H1B_6$conv_PREVAILING_WAGE <- ifelse(H1B_6$PW_UNIT_OF_PAY == "Hour", H1B_6$PREVAILING_WAGE * 40 * 52,
                                     ifelse(H1B_6$PW_UNIT_OF_PAY == "Week", H1B_6$PREVAILING_WAGE * 52,
                                            ifelse(H1B_6$PW_UNIT_OF_PAY == "Bi-Weekly", H1B_6$PREVAILING_WAGE * 26,
                                                   ifelse(H1B_6$PW_UNIT_OF_PAY == "Month", H1B_6$PREVAILING_WAGE * 12,
                                                          H1B_6$PREVAILING_WAGE))))
H1B_6$conv_WAGE_RATE_OF_PAY_FROM <- ifelse(H1B_6$WAGE_UNIT_OF_PAY == "Hour", H1B_6$WAGE_RATE_OF_PAY_FROM * 40 * 52,
                                           ifelse(H1B_6$WAGE_UNIT_OF_PAY == "Week", H1B_6$WAGE_RATE_OF_PAY_FROM * 52,
                                                  ifelse(H1B_6$WAGE_UNIT_OF_PAY == "Bi-Weekly", H1B_6$WAGE_RATE_OF_PAY_FROM * 26,
                                                         ifelse(H1B_6$WAGE_UNIT_OF_PAY == "Month", H1B_6$WAGE_RATE_OF_PAY_FROM * 12,
                                                                H1B_6$WAGE_RATE_OF_PAY_FROM))))
H1B_6$conv_WAGE_RATE_OF_PAY_TO <- ifelse(H1B_6$WAGE_UNIT_OF_PAY == "Hour", H1B_6$WAGE_RATE_OF_PAY_TO * 40 * 52,
                                         ifelse(H1B_6$WAGE_UNIT_OF_PAY == "Week", H1B_6$WAGE_RATE_OF_PAY_TO * 52,
                                                ifelse(H1B_6$WAGE_UNIT_OF_PAY == "Bi-Weekly", H1B_6$WAGE_RATE_OF_PAY_TO * 26,
                                                       ifelse(H1B_6$WAGE_UNIT_OF_PAY == "Month", H1B_6$WAGE_RATE_OF_PAY_TO * 12,
                                                              H1B_6$WAGE_RATE_OF_PAY_TO))))
H1B_6 <- na.omit(H1B_6)

### Random Forest - without converted salaries
H1B_6.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                            DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                            VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                            PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                            WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                            H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                          ntree = 100,
                          data = H1B_6)
H1B_6.rf #OOB: 22.65%

### Random Forest - with converted salaries
H1B_6.conv_wage.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                      DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                      VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                      conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                      conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                      H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                    ntree = 100,
                                    data = H1B_6)
H1B_6.conv_wage.rf #OOB: 22.85%

### J48
H1B_6.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                   DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                   VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                   PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                   WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                   H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                 data = H1B_6)
summary(H1B_6.j48) #PCC: 85.362  %

H1B_6.conv_wage.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                             DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                             VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                             conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                             conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                             H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                           data = H1B_6)
summary(H1B_6.conv_wage.j48) #PCC: 85.4265 %

##############################################
# Divide data into training and testing sets #
##############################################

############ 80/20 split ############
set.seed(12345)
H1B_6.samp.80 <- sample(1:nrow(H1B_6), nrow(H1B_6)*(.8), replace = FALSE)
train.H1B_6.80 <- H1B_6[H1B_6.samp.80, ]
test.H1B_6.80 <- H1B_6[-H1B_6.samp.80, ]

### Random Forest - without converted salaries
H1B_6.samp80.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                  DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                  VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                  PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                  WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                  H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                                ntree = 500,
                                data = train.H1B_6.80)
H1B_6.samp80.rf #OOB = 22.8%

H1B_6.samp80.rf.pred <- predict(H1B_6.samp80.rf, newdata = test.H1B_6.80)
table(H1B_6.samp80.rf.pred, test.H1B_6.80$CASE_STATUS)
(1195 + 1324 + 895 + 921) / nrow(test.H1B_6.80)
#PCC: 77.69%



### Random Forest - with converted salaries
H1B_6.conv_wage.samp80.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                            DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                            VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                            conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                            conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                            H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                          ntree = 500,
                                          data = train.H1B_6.80)
H1B_6.conv_wage.samp80.rf ##OOB: 22.56%

H1B_6.conv_wage.samp80.rf.pred <- predict(H1B_6.conv_wage.samp80.rf, newdata = test.H1B_6.80)
table(H1B_6.conv_wage.samp80.rf.pred, test.H1B_6.80$CASE_STATUS)
(1190 + 1328 + 906 + 919) / nrow(test.H1B_6.80)
#PCC: 77.83%

### J48
H1B_6.samp80.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                          DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                          VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                          PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                          WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                          H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                        data = train.H1B_6.80)
summary(H1B_6.samp80.j48)
H1B_6.samp80.j48.pred <- predict(H1B_6.samp80.j48, newdata = test.H1B_6.80)
table(H1B_6.samp80.j48.pred, test.H1B_6.80$CASE_STATUS)
(1097 + 1280 + 774 + 835) / nrow(test.H1B_6.80)
##PCC: 71.43%

H1B_6.conv_wage.samp80.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                    DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                    VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                    conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                    conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                    H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                  data = train.H1B_6.80)
summary(H1B_6.conv_wage.samp80.j48)
H1B_6.conv_wage.samp80.j48.pred <- predict(H1B_6.conv_wage.samp80.j48, newdata = test.H1B_6.80)
table(H1B_6.conv_wage.samp80.j48.pred, test.H1B_6.80$CASE_STATUS)
(1106 + 1280 + 776 + 837) / nrow(test.H1B_6.80)
##PCC: 71.67%


############ 66/34 split ############
set.seed(12345)
H1B_6.samp.66 <- sample(1:nrow(H1B_6), nrow(H1B_6)*(.66), replace = FALSE)
train.H1B_6.66 <- H1B_6[H1B_6.samp.66, ]
test.H1B_6.66 <- H1B_6[-H1B_6.samp.66, ]

### Random Forest - without converted salaries
H1B_6.samp66.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                  DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                  VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                  PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                  WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                  H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                                ntree = 500,
                                data = train.H1B_6.66)
H1B_6.samp66.rf #OOB = 23.15%

H1B_6.samp66.rf.pred <- predict(H1B_6.samp66.rf, newdata = test.H1B_6.66)
table(H1B_6.samp66.rf.pred, test.H1B_6.66$CASE_STATUS)
(2034 + 2203 + 1492 + 1546) / nrow(test.H1B_6.66)
#PCC: 76.69%


### Random Forest - with converted salaries
H1B_6.conv_wage.samp66.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                            DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                            VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                            conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                            conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                            H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                          ntree = 500,
                                          data = train.H1B_6.66)
H1B_6.conv_wage.samp66.rf ##OOB: 22.94%

H1B_6.conv_wage.samp66.rf.pred <- predict(H1B_6.conv_wage.samp66.rf, newdata = test.H1B_6.66)
table(H1B_6.conv_wage.samp66.rf.pred, test.H1B_6.66$CASE_STATUS)
(2037 + 2214 + 1512 + 1543) / nrow(test.H1B_6.66)
#PCC: 77.02%

### J48
H1B_6.samp66.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                          DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                          VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                          PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                          WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                          H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                        data = train.H1B_6.66)
summary(H1B_6.samp66.j48)
H1B_6.samp66.j48.pred <- predict(H1B_6.samp66.j48, newdata = test.H1B_6.66)
table(H1B_6.samp66.j48.pred, test.H1B_6.66$CASE_STATUS)
(1895 + 2113 + 1363 + 1424) / nrow(test.H1B_6.66)
##PCC: 71,63%

H1B_6.conv_wage.samp66.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                    DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                    VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                    conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                    conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                    H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                  data = train.H1B_6.66)
summary(H1B_6.conv_wage.samp66.j48)
H1B_6.conv_wage.samp66.j48.pred <- predict(H1B_6.conv_wage.samp66.j48, newdata = test.H1B_6.66)
table(H1B_6.conv_wage.samp66.j48.pred, test.H1B_6.66$CASE_STATUS)
(1912 + 2116 + 1364 + 1443) / nrow(test.H1B_6.66)
##PCC: 72.05%



#########################################################
# File 7                                                #
#########################################################
H1B_7[H1B_7$SOC_NAME %in% c("MANAGERS","MANAGEMENT","FIRST LINE SUPERVISORS"), 11] <- "MANAGEMENT"
H1B_7[H1B_7$SOC_NAME %in% c("GRAPHIC DESIGNERS","FASHION DESIGNERS","DESIGNERS"), 11] <- "DESIGNERS"
H1B_7 <- droplevels(H1B_7)

#Create less categories for state according to: https://www2.census.gov/geo/pdfs/reference/GARM/Ch6GARM.pdf
H1B_7.divisions <- H1B_7 %>% arrange(EMPLOYER_STATE) %>%
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

H1B_7.divisions$EMPLOYER_DIVISION <- as.factor(H1B_7.divisions$EMPLOYER_DIVISION)
H1B_7.divisions$WORKSITE_DIVISION <- as.factor(H1B_7.divisions$WORKSITE_DIVISION)

H1B_7 <- H1B_7.divisions[,-c(8,9,19,25,26)]

str(H1B_7)

H1B_7$conv_PREVAILING_WAGE <- ifelse(H1B_7$PW_UNIT_OF_PAY == "Hour", H1B_7$PREVAILING_WAGE * 40 * 52,
                                     ifelse(H1B_7$PW_UNIT_OF_PAY == "Week", H1B_7$PREVAILING_WAGE * 52,
                                            ifelse(H1B_7$PW_UNIT_OF_PAY == "Bi-Weekly", H1B_7$PREVAILING_WAGE * 26,
                                                   ifelse(H1B_7$PW_UNIT_OF_PAY == "Month", H1B_7$PREVAILING_WAGE * 12,
                                                          H1B_7$PREVAILING_WAGE))))
H1B_7$conv_WAGE_RATE_OF_PAY_FROM <- ifelse(H1B_7$WAGE_UNIT_OF_PAY == "Hour", H1B_7$WAGE_RATE_OF_PAY_FROM * 40 * 52,
                                           ifelse(H1B_7$WAGE_UNIT_OF_PAY == "Week", H1B_7$WAGE_RATE_OF_PAY_FROM * 52,
                                                  ifelse(H1B_7$WAGE_UNIT_OF_PAY == "Bi-Weekly", H1B_7$WAGE_RATE_OF_PAY_FROM * 26,
                                                         ifelse(H1B_7$WAGE_UNIT_OF_PAY == "Month", H1B_7$WAGE_RATE_OF_PAY_FROM * 12,
                                                                H1B_7$WAGE_RATE_OF_PAY_FROM))))
H1B_7$conv_WAGE_RATE_OF_PAY_TO <- ifelse(H1B_7$WAGE_UNIT_OF_PAY == "Hour", H1B_7$WAGE_RATE_OF_PAY_TO * 40 * 52,
                                         ifelse(H1B_7$WAGE_UNIT_OF_PAY == "Week", H1B_7$WAGE_RATE_OF_PAY_TO * 52,
                                                ifelse(H1B_7$WAGE_UNIT_OF_PAY == "Bi-Weekly", H1B_7$WAGE_RATE_OF_PAY_TO * 26,
                                                       ifelse(H1B_7$WAGE_UNIT_OF_PAY == "Month", H1B_7$WAGE_RATE_OF_PAY_TO * 12,
                                                              H1B_7$WAGE_RATE_OF_PAY_TO))))
H1B_7 <- na.omit(H1B_7)

### Random Forest - without converted salaries
H1B_7.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                            DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                            VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                            PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                            WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                            H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                          ntree = 100,
                          data = H1B_7)
H1B_7.rf ##OOB: 22.85%

### Random Forest - with converted salaries
H1B_7.conv_wage.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                      DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                      VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                      conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                      conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                      H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                    ntree = 100,
                                    data = H1B_7)
H1B_7.conv_wage.rf ##OOB: 22.79%

### J48
H1B_7.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                   DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                   VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                   PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                   WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                   H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                 data = H1B_7)
summary(H1B_7.j48) #PCC: 85.021  %

H1B_7.conv_wage.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                             DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                             VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                             conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                             conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                             H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                           data = H1B_7)
summary(H1B_7.conv_wage.j48) #PCC: 85.0389 %


##############################################
# Divide data into training and testing sets #
##############################################

############ 80/20 split ############
set.seed(12345)
H1B_7.samp.80 <- sample(1:nrow(H1B_7), nrow(H1B_7)*(.8), replace = FALSE)
train.H1B_7.80 <- H1B_7[H1B_7.samp.80, ]
test.H1B_7.80 <- H1B_7[-H1B_7.samp.80, ]

### Random Forest - without converted salaries
H1B_7.samp80.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                  DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                  VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                  PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                  WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                  H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                                ntree = 500,
                                data = train.H1B_7.80)
H1B_7.samp80.rf #OOB = 22.75%

H1B_7.samp80.rf.pred <- predict(H1B_7.samp80.rf, newdata = test.H1B_7.80)
table(H1B_7.samp80.rf.pred, test.H1B_7.80$CASE_STATUS)
(1188 + 1313 + 880 + 950) / nrow(test.H1B_7.80)
#PCC: 77.61649%



### Random Forest - with converted salaries
H1B_7.conv_wage.samp80.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                            DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                            VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                            conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                            conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                            H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                          ntree = 500,
                                          data = train.H1B_7.80)
H1B_7.conv_wage.samp80.rf ##OOB: 22.58%

H1B_7.conv_wage.samp80.rf.pred <- predict(H1B_7.conv_wage.samp80.rf, newdata = test.H1B_7.80)
table(H1B_7.conv_wage.samp80.rf.pred, test.H1B_7.80$CASE_STATUS)
(1180 + 1311 + 890 + 955) / nrow(test.H1B_7.80)
#PCC: 77.71%

### J48
H1B_7.samp80.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                          DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                          VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                          PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                          WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                          H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                        data = train.H1B_7.80)
summary(H1B_7.samp80.j48)
H1B_7.samp80.j48.pred <- predict(H1B_7.samp80.j48, newdata = test.H1B_7.80)
table(H1B_7.samp80.j48.pred, test.H1B_7.80$CASE_STATUS)
(1127 + 1285 + 786 + 903) / nrow(test.H1B_7.80)
##PCC: 73.49%

H1B_7.conv_wage.samp80.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                    DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                    VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                    conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                    conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                    H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                  data = train.H1B_7.80)
summary(H1B_7.conv_wage.samp80.j48)
H1B_7.conv_wage.samp80.j48.pred <- predict(H1B_7.conv_wage.samp80.j48, newdata = test.H1B_7.80)
table(H1B_7.conv_wage.samp80.j48.pred, test.H1B_7.80$CASE_STATUS)
(1134 + 1267 + 795 + 891) / nrow(test.H1B_7.80)
##PCC: 73.24%


############ 66/34 split ############
set.seed(12345)
H1B_7.samp.66 <- sample(1:nrow(H1B_7), nrow(H1B_7)*(.66), replace = FALSE)
train.H1B_7.66 <- H1B_7[H1B_7.samp.66, ]
test.H1B_7.66 <- H1B_7[-H1B_7.samp.66, ]

### Random Forest - without converted salaries
H1B_7.samp66.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                  DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                  VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                  PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                  WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                  H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                                ntree = 500,
                                data = train.H1B_7.66)
H1B_7.samp66.rf #OOB = 22.91%

H1B_7.samp66.rf.pred <- predict(H1B_7.samp66.rf, newdata = test.H1B_7.66)
table(H1B_7.samp66.rf.pred, test.H1B_7.66$CASE_STATUS)
(2012 + 2201 + 1471 + 1605) / nrow(test.H1B_7.66)
#PCC: 76.83955 %


### Random Forest - with converted salaries
H1B_7.conv_wage.samp66.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                            DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                            VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                            conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                            conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                            H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                          ntree = 500,
                                          data = train.H1B_7.66)
H1B_7.conv_wage.samp66.rf ##OOB: 22.77%

H1B_7.conv_wage.samp66.rf.pred <- predict(H1B_7.conv_wage.samp66.rf, newdata = test.H1B_7.66)
table(H1B_7.conv_wage.samp66.rf.pred, test.H1B_7.66$CASE_STATUS)
(2010 + 2194 + 1479 + 1588) / nrow(test.H1B_7.66)
#PCC:76.6498 %

### J48
H1B_7.samp66.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                          DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                          VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                          PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                          WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                          H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                        data = train.H1B_7.66)
summary(H1B_7.samp66.j48)
H1B_7.samp66.j48.pred <- predict(H1B_7.samp66.j48, newdata = test.H1B_7.66)
table(H1B_7.samp66.j48.pred, test.H1B_7.66$CASE_STATUS)
(1873 + 2119 + 1307 + 1419) / nrow(test.H1B_7.66)
##PCC: 70.82016%

H1B_7.conv_wage.samp66.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                    DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                    VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                    conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                    conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                    H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                  data = train.H1B_7.66)
summary(H1B_7.conv_wage.samp66.j48)
H1B_7.conv_wage.samp66.j48.pred <- predict(H1B_7.conv_wage.samp66.j48, newdata = test.H1B_7.66)
table(H1B_7.conv_wage.samp66.j48.pred, test.H1B_7.66$CASE_STATUS)
(1925 + 2136 + 1312 + 1443) / nrow(test.H1B_7.66)
##PCC:71.85326 %



#########################################################
# File 8                                                #
#########################################################
H1B_8[H1B_8$SOC_NAME %in% c("MANAGERS","MANAGEMENT","FIRST LINE SUPERVISORS"), 11] <- "MANAGEMENT"
H1B_8[H1B_8$SOC_NAME %in% c("GRAPHIC DESIGNERS","FASHION DESIGNERS","DESIGNERS"), 11] <- "DESIGNERS"
H1B_8 <- droplevels(H1B_8)

#Create less categories for state according to: https://www2.census.gov/geo/pdfs/reference/GARM/Ch6GARM.pdf
H1B_8.divisions <- H1B_8 %>% arrange(EMPLOYER_STATE) %>%
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

H1B_8.divisions$EMPLOYER_DIVISION <- as.factor(H1B_8.divisions$EMPLOYER_DIVISION)
H1B_8.divisions$WORKSITE_DIVISION <- as.factor(H1B_8.divisions$WORKSITE_DIVISION)

H1B_8 <- H1B_8.divisions[,-c(8,9,19,25,26)]

str(H1B_8)

H1B_8$conv_PREVAILING_WAGE <- ifelse(H1B_8$PW_UNIT_OF_PAY == "Hour", H1B_8$PREVAILING_WAGE * 40 * 52,
                                     ifelse(H1B_8$PW_UNIT_OF_PAY == "Week", H1B_8$PREVAILING_WAGE * 52,
                                            ifelse(H1B_8$PW_UNIT_OF_PAY == "Bi-Weekly", H1B_8$PREVAILING_WAGE * 26,
                                                   ifelse(H1B_8$PW_UNIT_OF_PAY == "Month", H1B_8$PREVAILING_WAGE * 12,
                                                          H1B_8$PREVAILING_WAGE))))
H1B_8$conv_WAGE_RATE_OF_PAY_FROM <- ifelse(H1B_8$WAGE_UNIT_OF_PAY == "Hour", H1B_8$WAGE_RATE_OF_PAY_FROM * 40 * 52,
                                           ifelse(H1B_8$WAGE_UNIT_OF_PAY == "Week", H1B_8$WAGE_RATE_OF_PAY_FROM * 52,
                                                  ifelse(H1B_8$WAGE_UNIT_OF_PAY == "Bi-Weekly", H1B_8$WAGE_RATE_OF_PAY_FROM * 26,
                                                         ifelse(H1B_8$WAGE_UNIT_OF_PAY == "Month", H1B_8$WAGE_RATE_OF_PAY_FROM * 12,
                                                                H1B_8$WAGE_RATE_OF_PAY_FROM))))
H1B_8$conv_WAGE_RATE_OF_PAY_TO <- ifelse(H1B_8$WAGE_UNIT_OF_PAY == "Hour", H1B_8$WAGE_RATE_OF_PAY_TO * 40 * 52,
                                         ifelse(H1B_8$WAGE_UNIT_OF_PAY == "Week", H1B_8$WAGE_RATE_OF_PAY_TO * 52,
                                                ifelse(H1B_8$WAGE_UNIT_OF_PAY == "Bi-Weekly", H1B_8$WAGE_RATE_OF_PAY_TO * 26,
                                                       ifelse(H1B_8$WAGE_UNIT_OF_PAY == "Month", H1B_8$WAGE_RATE_OF_PAY_TO * 12,
                                                              H1B_8$WAGE_RATE_OF_PAY_TO))))

H1B_8 <- na.omit(H1B_8)

### Random Forest - without converted salaries
H1B_8.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                            DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                            VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                            PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                            WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                            H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                          ntree = 100,
                          data = H1B_8)
H1B_8.rf ##OOB: 23.14%

### Random Forest - with converted salaries
H1B_8.conv_wage.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                      DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                      VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                      conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                      conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                      H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                    ntree = 100,
                                    data = H1B_8)
H1B_8.conv_wage.rf ##OOB: 23.07%

### J48
H1B_8.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                   DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                   VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                   PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                   WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                   H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                 data = H1B_8)
summary(H1B_8.j48) #PCC: 85.4014 %

H1B_8.conv_wage.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                             DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                             VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                             conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                             conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                             H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                           data = H1B_8)
summary(H1B_8.conv_wage.j48) #PCC: 85.6703 %

##############################################
# Divide data into training and testing sets #
##############################################

############ 80/20 split ############
set.seed(12345)
H1B_8.samp.80 <- sample(1:nrow(H1B_8), nrow(H1B_8)*(.8), replace = FALSE)
train.H1B_8.80 <- H1B_8[H1B_8.samp.80, ]
test.H1B_8.80 <- H1B_8[-H1B_8.samp.80, ]

### Random Forest - without converted salaries
H1B_8.samp80.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                  DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                  VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                  PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                  WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                  H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                                ntree = 500,
                                data = train.H1B_8.80)
H1B_8.samp80.rf #OOB = 22.81%

H1B_8.samp80.rf.pred <- predict(H1B_8.samp80.rf, newdata = test.H1B_8.80)
table(H1B_8.samp80.rf.pred, test.H1B_8.80$CASE_STATUS)
(1178 + 1331 + 881 + 906) / nrow(test.H1B_8.80)
#PCC: 76.98925%



### Random Forest - with converted salaries
H1B_8.conv_wage.samp80.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                            DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                            VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                            conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                            conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                            H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                          ntree = 500,
                                          data = train.H1B_8.80)
H1B_8.conv_wage.samp80.rf ##OOB: 22.61%

H1B_8.conv_wage.samp80.rf.pred <- predict(H1B_8.conv_wage.samp80.rf, newdata = test.H1B_8.80)
table(H1B_8.conv_wage.samp80.rf.pred, test.H1B_8.80$CASE_STATUS)
(1181 + 1335 + 882 + 914) / nrow(test.H1B_8.80)
#PCC:77.27599 %

### J48
H1B_8.samp80.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                          DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                          VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                          PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                          WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                          H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                        data = train.H1B_8.80)
summary(H1B_8.samp80.j48)
H1B_8.samp80.j48.pred <- predict(H1B_8.samp80.j48, newdata = test.H1B_8.80)
table(H1B_8.samp80.j48.pred, test.H1B_8.80$CASE_STATUS)
(1140 + 1289 + 787 + 828) / nrow(test.H1B_8.80)
##PCC: 72.47312%

H1B_8.conv_wage.samp80.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                    DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                    VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                    conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                    conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                    H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                  data = train.H1B_8.80)
summary(H1B_8.conv_wage.samp80.j48)
H1B_8.conv_wage.samp80.j48.pred <- predict(H1B_8.conv_wage.samp80.j48, newdata = test.H1B_8.80)
table(H1B_8.conv_wage.samp80.j48.pred, test.H1B_8.80$CASE_STATUS)
(1140 + 1287 + 793 + 821) / nrow(test.H1B_8.80)
##PCC:72.41935 %


############ 66/34 split ############
set.seed(12345)
H1B_8.samp.66 <- sample(1:nrow(H1B_8), nrow(H1B_8)*(.66), replace = FALSE)
train.H1B_8.66 <- H1B_8[H1B_8.samp.66, ]
test.H1B_8.66 <- H1B_8[-H1B_8.samp.66, ]

### Random Forest - without converted salaries
H1B_8.samp66.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                  DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                  VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                  PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                  WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                  H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                                ntree = 500,
                                data = train.H1B_8.66)
H1B_8.samp66.rf #OOB = 23.35%

H1B_8.samp66.rf.pred <- predict(H1B_8.samp66.rf, newdata = test.H1B_8.66)
table(H1B_8.samp66.rf.pred, test.H1B_8.66$CASE_STATUS)
(2001 + 2208 + 1485 + 1559) / nrow(test.H1B_8.66)
#PCC: 76.46005%


### Random Forest - with converted salaries
H1B_8.conv_wage.samp66.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                            DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                            VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                            conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                            conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                            H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                          ntree = 500,
                                          data = train.H1B_8.66)
H1B_8.conv_wage.samp66.rf ##OOB: 23.04%

H1B_8.conv_wage.samp66.rf.pred <- predict(H1B_8.conv_wage.samp66.rf, newdata = test.H1B_8.66)
table(H1B_8.conv_wage.samp66.rf.pred, test.H1B_8.66$CASE_STATUS)
(2019 + 2212 + 1499 + 1562) / nrow(test.H1B_8.66)
#PCC:76.87118 %

### J48
H1B_8.samp66.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                          DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                          VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                          PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                          WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                          H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                        data = train.H1B_8.66)
summary(H1B_8.samp66.j48)
H1B_8.samp66.j48.pred <- predict(H1B_8.samp66.j48, newdata = test.H1B_8.66)
table(H1B_8.samp66.j48.pred, test.H1B_8.66$CASE_STATUS)
(1898 + 2176 + 1314 + 1371) / nrow(test.H1B_8.66)
##PCC: 71.25237%

H1B_8.conv_wage.samp66.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                    DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                    VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                    conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                    conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                    H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                  data = train.H1B_8.66)
summary(H1B_8.conv_wage.samp66.j48)
H1B_8.conv_wage.samp66.j48.pred <- predict(H1B_8.conv_wage.samp66.j48, newdata = test.H1B_8.66)
table(H1B_8.conv_wage.samp66.j48.pred, test.H1B_8.66$CASE_STATUS)
(1914 + 2185 + 1303 + 1370) / nrow(test.H1B_8.66)
##PCC: 71.38942%



#########################################################
# File 9                                                #
#########################################################
H1B_9[H1B_9$SOC_NAME %in% c("MANAGERS","MANAGEMENT","FIRST LINE SUPERVISORS"), 11] <- "MANAGEMENT"
H1B_9[H1B_9$SOC_NAME %in% c("GRAPHIC DESIGNERS","FASHION DESIGNERS","DESIGNERS"), 11] <- "DESIGNERS"
H1B_9 <- droplevels(H1B_9)

#Create less categories for state according to: https://www2.census.gov/geo/pdfs/reference/GARM/Ch6GARM.pdf
H1B_9.divisions <- H1B_9 %>% arrange(EMPLOYER_STATE) %>%
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

H1B_9.divisions$EMPLOYER_DIVISION <- as.factor(H1B_9.divisions$EMPLOYER_DIVISION)
H1B_9.divisions$WORKSITE_DIVISION <- as.factor(H1B_9.divisions$WORKSITE_DIVISION)

H1B_9 <- H1B_9.divisions[,-c(8,9,19,25,26)]

str(H1B_9)

H1B_9$conv_PREVAILING_WAGE <- ifelse(H1B_9$PW_UNIT_OF_PAY == "Hour", H1B_9$PREVAILING_WAGE * 40 * 52,
                                     ifelse(H1B_9$PW_UNIT_OF_PAY == "Week", H1B_9$PREVAILING_WAGE * 52,
                                            ifelse(H1B_9$PW_UNIT_OF_PAY == "Bi-Weekly", H1B_9$PREVAILING_WAGE * 26,
                                                   ifelse(H1B_9$PW_UNIT_OF_PAY == "Month", H1B_9$PREVAILING_WAGE * 12,
                                                          H1B_9$PREVAILING_WAGE))))
H1B_9$conv_WAGE_RATE_OF_PAY_FROM <- ifelse(H1B_9$WAGE_UNIT_OF_PAY == "Hour", H1B_9$WAGE_RATE_OF_PAY_FROM * 40 * 52,
                                           ifelse(H1B_9$WAGE_UNIT_OF_PAY == "Week", H1B_9$WAGE_RATE_OF_PAY_FROM * 52,
                                                  ifelse(H1B_9$WAGE_UNIT_OF_PAY == "Bi-Weekly", H1B_9$WAGE_RATE_OF_PAY_FROM * 26,
                                                         ifelse(H1B_9$WAGE_UNIT_OF_PAY == "Month", H1B_9$WAGE_RATE_OF_PAY_FROM * 12,
                                                                H1B_9$WAGE_RATE_OF_PAY_FROM))))
H1B_9$conv_WAGE_RATE_OF_PAY_TO <- ifelse(H1B_9$WAGE_UNIT_OF_PAY == "Hour", H1B_9$WAGE_RATE_OF_PAY_TO * 40 * 52,
                                         ifelse(H1B_9$WAGE_UNIT_OF_PAY == "Week", H1B_9$WAGE_RATE_OF_PAY_TO * 52,
                                                ifelse(H1B_9$WAGE_UNIT_OF_PAY == "Bi-Weekly", H1B_9$WAGE_RATE_OF_PAY_TO * 26,
                                                       ifelse(H1B_9$WAGE_UNIT_OF_PAY == "Month", H1B_9$WAGE_RATE_OF_PAY_TO * 12,
                                                              H1B_9$WAGE_RATE_OF_PAY_TO))))

H1B_9 <- na.omit(H1B_9)

### Random Forest - without converted salaries
H1B_9.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                            DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                            VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                            PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                            WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                            H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                          ntree = 100,
                          data = H1B_9)
H1B_9.rf #OOB: 22.99%

### Random Forest - with converted salaries
H1B_9.conv_wage.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                      DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                      VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                      conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                      conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                      H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                    ntree = 100,
                                    data = H1B_9)
H1B_9.conv_wage.rf #OOB: 22.91%

### J48
H1B_9.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                   DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                   VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                   PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                   WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                   H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                 data = H1B_9)
summary(H1B_9.j48) #PCC: 84.9892 %

H1B_9.conv_wage.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                             DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                             VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                             conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                             conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                             H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                           data = H1B_9)
summary(H1B_9.conv_wage.j48) #PCC: 84.9928 %

##############################################
# Divide data into training and testing sets #
##############################################

############ 80/20 split ############
set.seed(12345)
H1B_9.samp.80 <- sample(1:nrow(H1B_9), nrow(H1B_9)*(.8), replace = FALSE)
train.H1B_9.80 <- H1B_9[H1B_9.samp.80, ]
test.H1B_9.80 <- H1B_9[-H1B_9.samp.80, ]

### Random Forest - without converted salaries
H1B_9.samp80.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                  DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                  VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                  PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                  WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                  H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                                ntree = 500,
                                data = train.H1B_9.80)
H1B_9.samp80.rf #OOB = 22.63%

H1B_9.samp80.rf.pred <- predict(H1B_9.samp80.rf, newdata = test.H1B_9.80)
table(H1B_9.samp80.rf.pred, test.H1B_9.80$CASE_STATUS)
(1167 + 1279 + 891 + 951) / nrow(test.H1B_9.80)
#PCC: 76.84588%



### Random Forest - with converted salaries
H1B_9.conv_wage.samp80.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                            DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                            VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                            conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                            conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                            H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                          ntree = 500,
                                          data = train.H1B_9.80)
H1B_9.conv_wage.samp80.rf ##OOB: 22.5%

H1B_9.conv_wage.samp80.rf.pred <- predict(H1B_9.conv_wage.samp80.rf, newdata = test.H1B_9.80)
table(H1B_9.conv_wage.samp80.rf.pred, test.H1B_9.80$CASE_STATUS)
(1173 + 1276 + 902 + 946) / nrow(test.H1B_9.80)
#PCC: 77.00717%

### J48
H1B_9.samp80.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                          DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                          VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                          PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                          WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                          H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                        data = train.H1B_9.80)
summary(H1B_9.samp80.j48)
H1B_9.samp80.j48.pred <- predict(H1B_9.samp80.j48, newdata = test.H1B_9.80)
table(H1B_9.samp80.j48.pred, test.H1B_9.80$CASE_STATUS)
(1118 + 1249 + 785 + 860) / nrow(test.H1B_9.80)
##PCC: 71.89964%

H1B_9.conv_wage.samp80.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                    DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                    VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                    conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                    conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                    H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                  data = train.H1B_9.80)
summary(H1B_9.conv_wage.samp80.j48)
H1B_9.conv_wage.samp80.j48.pred <- predict(H1B_9.conv_wage.samp80.j48, newdata = test.H1B_9.80)
table(H1B_9.conv_wage.samp80.j48.pred, test.H1B_9.80$CASE_STATUS)
(1112 + 1240 + 781 + 862) / nrow(test.H1B_9.80)
##PCC: 71.59498%


############ 66/34 split ############
set.seed(12345)
H1B_9.samp.66 <- sample(1:nrow(H1B_9), nrow(H1B_9)*(.66), replace = FALSE)
train.H1B_9.66 <- H1B_9[H1B_9.samp.66, ]
test.H1B_9.66 <- H1B_9[-H1B_9.samp.66, ]

### Random Forest - without converted salaries
H1B_9.samp66.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                  DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                  VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                  PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                  WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                  H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                                ntree = 500,
                                data = train.H1B_9.66)
H1B_9.samp66.rf #OOB = 22.68%

H1B_9.samp66.rf.pred <- predict(H1B_9.samp66.rf, newdata = test.H1B_9.66)
table(H1B_9.samp66.rf.pred, test.H1B_9.66$CASE_STATUS)
(2015 + 2174 + 1496 + 1568) / nrow(test.H1B_9.66)
#PCC: 76.46005%


### Random Forest - with converted salaries
H1B_9.conv_wage.samp66.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                            DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                            VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                            conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                            conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                            H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                          ntree = 500,
                                          data = train.H1B_9.66)
H1B_9.conv_wage.samp66.rf ##OOB: 22.73%

H1B_9.conv_wage.samp66.rf.pred <- predict(H1B_9.conv_wage.samp66.rf, newdata = test.H1B_9.66)
table(H1B_9.conv_wage.samp66.rf.pred, test.H1B_9.66$CASE_STATUS)
(1997 + 2175 + 1511 + 1555) / nrow(test.H1B_9.66)
#PCC: 76.30192%

### J48
H1B_9.samp66.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                          DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                          VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                          PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                          WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                          H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                        data = train.H1B_9.66)
summary(H1B_9.samp66.j48)
H1B_9.samp66.j48.pred <- predict(H1B_9.samp66.j48, newdata = test.H1B_9.66)
table(H1B_9.samp66.j48.pred, test.H1B_9.66$CASE_STATUS)
(1919 + 2114 + 1278 + 1358) / nrow(test.H1B_9.66)
##PCC: 70.30361%

H1B_9.conv_wage.samp66.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                    DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                    VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                    conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                    conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                    H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                  data = train.H1B_9.66)
summary(H1B_9.conv_wage.samp66.j48)
H1B_9.conv_wage.samp66.j48.pred <- predict(H1B_9.conv_wage.samp66.j48, newdata = test.H1B_9.66)
table(H1B_9.conv_wage.samp66.j48.pred, test.H1B_9.66$CASE_STATUS)
(1919 + 2114 + 1263 + 1368) / nrow(test.H1B_9.66)
##PCC: 70.2509%



#########################################################
# File 10                                               #
#########################################################
H1B_10[H1B_10$SOC_NAME %in% c("MANAGERS","MANAGEMENT","FIRST LINE SUPERVISORS"), 11] <- "MANAGEMENT"
H1B_10[H1B_10$SOC_NAME %in% c("GRAPHIC DESIGNERS","FASHION DESIGNERS","DESIGNERS"), 11] <- "DESIGNERS"
H1B_10 <- droplevels(H1B_10)

#Create less categories for state according to: https://www2.census.gov/geo/pdfs/reference/GARM/Ch6GARM.pdf
H1B_10.divisions <- H1B_10 %>% arrange(EMPLOYER_STATE) %>%
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

H1B_10.divisions$EMPLOYER_DIVISION <- as.factor(H1B_10.divisions$EMPLOYER_DIVISION)
H1B_10.divisions$WORKSITE_DIVISION <- as.factor(H1B_10.divisions$WORKSITE_DIVISION)

H1B_10 <- H1B_10.divisions[,-c(8,9,19,25,26)]

str(H1B_10)

H1B_10$conv_PREVAILING_WAGE <- ifelse(H1B_10$PW_UNIT_OF_PAY == "Hour", H1B_10$PREVAILING_WAGE * 40 * 52,
                                     ifelse(H1B_10$PW_UNIT_OF_PAY == "Week", H1B_10$PREVAILING_WAGE * 52,
                                            ifelse(H1B_10$PW_UNIT_OF_PAY == "Bi-Weekly", H1B_10$PREVAILING_WAGE * 26,
                                                   ifelse(H1B_10$PW_UNIT_OF_PAY == "Month", H1B_10$PREVAILING_WAGE * 12,
                                                          H1B_10$PREVAILING_WAGE))))
H1B_10$conv_WAGE_RATE_OF_PAY_FROM <- ifelse(H1B_10$WAGE_UNIT_OF_PAY == "Hour", H1B_10$WAGE_RATE_OF_PAY_FROM * 40 * 52,
                                           ifelse(H1B_10$WAGE_UNIT_OF_PAY == "Week", H1B_10$WAGE_RATE_OF_PAY_FROM * 52,
                                                  ifelse(H1B_10$WAGE_UNIT_OF_PAY == "Bi-Weekly", H1B_10$WAGE_RATE_OF_PAY_FROM * 26,
                                                         ifelse(H1B_10$WAGE_UNIT_OF_PAY == "Month", H1B_10$WAGE_RATE_OF_PAY_FROM * 12,
                                                                H1B_10$WAGE_RATE_OF_PAY_FROM))))
H1B_10$conv_WAGE_RATE_OF_PAY_TO <- ifelse(H1B_10$WAGE_UNIT_OF_PAY == "Hour", H1B_10$WAGE_RATE_OF_PAY_TO * 40 * 52,
                                         ifelse(H1B_10$WAGE_UNIT_OF_PAY == "Week", H1B_10$WAGE_RATE_OF_PAY_TO * 52,
                                                ifelse(H1B_10$WAGE_UNIT_OF_PAY == "Bi-Weekly", H1B_10$WAGE_RATE_OF_PAY_TO * 26,
                                                       ifelse(H1B_10$WAGE_UNIT_OF_PAY == "Month", H1B_10$WAGE_RATE_OF_PAY_TO * 12,
                                                              H1B_10$WAGE_RATE_OF_PAY_TO))))

H1B_10 <- na.omit(H1B_10)

### Random Forest - without converted salaries
H1B_10.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                            DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                            VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                            PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                            WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                            H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                          ntree = 100,
                          data = H1B_10)
H1B_10.rf ##OOB: 22.82%

### Random Forest - with converted salaries
H1B_10.conv_wage.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                      DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                      VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                      conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                      conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                      H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                    ntree = 100,
                                    data = H1B_10)
H1B_10.conv_wage.rf #OOB: 22.69%

### J48
H1B_10.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                   DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                   VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                   PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                   WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                   H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                 data = H1B_10)
summary(H1B_10.j48) #PCC: 85.7916 %

H1B_10.conv_wage.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                             DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                             VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                             conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                             conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                             H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                           data = H1B_10)
summary(H1B_10.conv_wage.j48) #PCC: 85.4332 %


##############################################
# Divide data into training and testing sets #
##############################################

############ 80/20 split ############
set.seed(12345)
H1B_10.samp.80 <- sample(1:nrow(H1B_10), nrow(H1B_10)*(.8), replace = FALSE)
train.H1B_10.80 <- H1B_10[H1B_10.samp.80, ]
test.H1B_10.80 <- H1B_10[-H1B_10.samp.80, ]

### Random Forest - without converted salaries
H1B_10.samp80.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                  DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                  VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                  PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                  WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                  H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                                ntree = 500,
                                data = train.H1B_10.80)
H1B_10.samp80.rf #OOB = 22.70%

H1B_10.samp80.rf.pred <- predict(H1B_10.samp80.rf, newdata = test.H1B_10.80)
table(H1B_10.samp80.rf.pred, test.H1B_10.80$CASE_STATUS)
(1210 + 1329 + 865 + 966) / nrow(test.H1B_10.80)
#PCC: 78.31541%



### Random Forest - with converted salaries
H1B_10.conv_wage.samp80.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                            DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                            VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                            conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                            conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                            H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                          ntree = 500,
                                          data = train.H1B_10.80)
H1B_10.conv_wage.samp80.rf ##OOB: 22.38%

H1B_10.conv_wage.samp80.rf.pred <- predict(H1B_10.conv_wage.samp80.rf, newdata = test.H1B_10.80)
table(H1B_10.conv_wage.samp80.rf.pred, test.H1B_10.80$CASE_STATUS)
(1218 + 1328 + 862 + 957) / nrow(test.H1B_10.80)
#PCC: 78.22581%

### J48
H1B_10.samp80.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                          DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                          VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                          PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                          WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                          H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                        data = train.H1B_10.80)
summary(H1B_10.samp80.j48)
H1B_10.samp80.j48.pred <- predict(H1B_10.samp80.j48, newdata = test.H1B_10.80)
table(H1B_10.samp80.j48.pred, test.H1B_10.80$CASE_STATUS)
(1153 + 1283 + 763 + 873) / nrow(test.H1B_10.80)
##PCC: 72.97491%

H1B_10.conv_wage.samp80.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                    DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                    VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                    conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                    conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                    H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                  data = train.H1B_10.80)
summary(H1B_10.conv_wage.samp80.j48)
H1B_10.conv_wage.samp80.j48.pred <- predict(H1B_10.conv_wage.samp80.j48, newdata = test.H1B_10.80)
table(H1B_10.conv_wage.samp80.j48.pred, test.H1B_10.80$CASE_STATUS)
(1139 + 1269 + 822 + 931) / nrow(test.H1B_10.80)
##PCC: 74.56989%


############ 66/34 split ############
set.seed(12345)
H1B_10.samp.66 <- sample(1:nrow(H1B_10), nrow(H1B_10)*(.66), replace = FALSE)
train.H1B_10.66 <- H1B_10[H1B_10.samp.66, ]
test.H1B_10.66 <- H1B_10[-H1B_10.samp.66, ]

### Random Forest - without converted salaries
H1B_10.samp66.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                  DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                  VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                  PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                  WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                  H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                                ntree = 500,
                                data = train.H1B_10.66)
H1B_10.samp66.rf #OOB = 23.00%

H1B_10.samp66.rf.pred <- predict(H1B_10.samp66.rf, newdata = test.H1B_10.66)
table(H1B_10.samp66.rf.pred, test.H1B_10.66$CASE_STATUS)
(2018 + 2204 + 1481 + 1655) / nrow(test.H1B_10.66)
#PCC: 77.56694%


### Random Forest - with converted salaries
H1B_10.conv_wage.samp66.rf <- randomForest(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                            DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                            VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                            conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                            conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                            H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                          ntree = 500,
                                          data = train.H1B_10.66)
H1B_10.conv_wage.samp66.rf ##OOB: 22.65%

H1B_10.conv_wage.samp66.rf.pred <- predict(H1B_10.conv_wage.samp66.rf, newdata = test.H1B_10.66)
table(H1B_10.conv_wage.samp66.rf.pred, test.H1B_10.66$CASE_STATUS)
(2020 + 2204 + 1491 + 1631) / nrow(test.H1B_10.66)
#PCC: 77.44044%

### J48
H1B_10.samp66.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                          DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                          VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                          PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                          WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                          H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION, 
                        data = train.H1B_10.66)
summary(H1B_10.samp66.j48)
H1B_10.samp66.j48.pred <- predict(H1B_10.samp66.j48, newdata = test.H1B_10.66)
table(H1B_10.samp66.j48.pred, test.H1B_10.66$CASE_STATUS)
(1909 + 2118 + 1306 + 1484) / nrow(test.H1B_10.66)
##PCC: 71.8638%

H1B_10.conv_wage.samp66.j48 <- J48(CASE_STATUS ~ CASE_SUBMITTED_DAY + CASE_SUBMITTED_MONTH + CASE_SUBMITTED_YEAR + 
                                    DECISION_DAY + DECISION_MONTH + DECISION_YEAR + 
                                    VISA_CLASS + EMPLOYER_COUNTRY + SOC_NAME + NAICS_CODE + TOTAL_WORKERS + FULL_TIME_POSITION +
                                    conv_PREVAILING_WAGE + PW_UNIT_OF_PAY + PW_SOURCE + PW_SOURCE_YEAR + 
                                    conv_WAGE_RATE_OF_PAY_FROM + conv_WAGE_RATE_OF_PAY_TO + WAGE_UNIT_OF_PAY +
                                    H.1B_DEPENDENT + WILLFUL_VIOLATOR + EMPLOYER_DIVISION + WORKSITE_DIVISION,
                                  data = train.H1B_10.66)
summary(H1B_10.conv_wage.samp66.j48)
H1B_10.conv_wage.samp66.j48.pred <- predict(H1B_10.conv_wage.samp66.j48, newdata = test.H1B_10.66)
table(H1B_10.conv_wage.samp66.j48.pred, test.H1B_10.66$CASE_STATUS)
(1929 + 2102 + 1293 + 1485) / nrow(test.H1B_10.66)
##PCC: 71.77946%

             