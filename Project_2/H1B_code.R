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


#########################################################
# File 2
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

#########################################################
# File 3
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


#########################################################
# File 4
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


#########################################################
# File 5
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


#########################################################
# File 6
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



#########################################################
# File 7
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


#########################################################
# File 8
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


#########################################################
# File 9
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
H1B_9.rf #OOB: 23.14%

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


#########################################################
# File 10
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

             