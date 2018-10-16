library(tidyverse)
library(readxl)
library(caret)
library(reshape2)


# loading in the data 
setwd("C:/Users/12ces/OneDrive/Desktop/MMA/MMA 867/Assignment 3")
bank_file <- "credit_data.xlsx"


bank <- read_xlsx(bank_file,1)
# deal with the scientific notation is changed below by changing to type double 


names(bank) <- gsub(" ", "_", names(bank))
str(bank)

################ DATA CLEANING ###########################

bank$LIMIT_BAL <- as.double(bank$LIMIT_BAL) 
bank$SEX <- as.factor(bank$SEX)
bank$EDUCATION <- as.factor(bank$EDUCATION)
bank$MARRIAGE <- as.factor(bank$MARRIAGE)
bank$PAY_0 <- as.factor(bank$PAY_0)
bank$PAY_2 <- as.factor(bank$PAY_2)
bank$PAY_3 <- as.factor(bank$PAY_3)
bank$PAY_4 <- as.factor(bank$PAY_4)
bank$PAY_5 <- as.factor(bank$PAY_5) 
bank$PAY_6 <- as.factor(bank$PAY_6)
bank$PAY_AMT1 <- as.numeric(bank$PAY_AMT1)
bank$PAY_AMT2 <- as.numeric(bank$PAY_AMT2)
bank$PAY_AMT3 <- as.numeric(bank$PAY_AMT3)
bank$PAY_AMT4 <- as.numeric(bank$PAY_AMT4)
bank$PAY_AMT5 <- as.numeric(bank$PAY_AMT5)
bank$PAY_AMT6 <- as.numeric(bank$PAY_AMT6)
names(bank[-1]) <- "test"


summary(bank)
str(bank)
sum(is.na(bank))

# just need to consider IDs and switch default payment to factor (Y/N)
bank$default.payment.next.month <- as.factor(bank$default.payment.next.month)

# No NAs
sum(is.na(bank))

# now we have the issue of small categorical variable levels 
str(bank)
table(bank$PAY_0)
table(bank$PAY_2)
table(bank$PAY_3)
table(bank$PAY_4)
table(bank$PAY_5)
table(bank$PAY_6)
table(bank$SEX)
table(bank$EDUCATION) # here we have 11 zeros which is not in the data dictionary
table(bank$MARRIAGE) # here we have 19 zeros which is not in the data dictionary 

# the zeros in Education and Marriage should be treated as NAs and errors
levels(bank$EDUCATION)[levels(bank$EDUCATION)==0] <- NA
levels(bank$MARRIAGE)[levels(bank$MARRIAGE)==0] <- NA

# removing the clear errors 
bank <- bank[complete.cases(bank), ]
sum(is.na(bank)) # done 

# now we have the issue of the other really small levels in PAY_0 to PAY_6
levels(bank$PAY_3)[levels(bank$PAY_3)==8] <- NA
levels(bank$PAY_4)[levels(bank$PAY_4)==1] <- NA
levels(bank$PAY_4)[levels(bank$PAY_4)==8] <- NA
levels(bank$PAY_5)[levels(bank$PAY_5)==8] <- NA
levels(bank$PAY_6)[levels(bank$PAY_6)==8] <- NA
bank <- bank[complete.cases(bank), ]
sum(is.na(bank))

# just cleaning up a few categorical variable names to align with the data dictionary 
levels(bank$MARRIAGE) <- c("married","single","others")
levels(bank$EDUCATION) <- c("graduate school", "university", "high school", "others" , "unknown", "unknown")
table(bank$EDUCATION) # coupled 5 and 6 to one unknown level 
levels(bank$SEX) <- c("male", "female")

#### FEATURE ENGINEERING  #### 
# let's look at correlations of our independent variables with our dependent variable
#install.packages("corrgram")
library(corrgram)
corrgram(bank, order = TRUE, lower.panel = panel.shade,
         upper.panel = panel.pie, text.panel = panel.txt)


# we can see in the correlation plot that BILL_AMTs are very highly correlated
# consider a new var to combine these
# tried removing the IDs and it really boosted predictions
bank$OLD_SYSTEM_ID <- NULL
bank$CURRENT_ID <- NULL


# lets look at LIMIT
hist(bank$LIMIT_BAL)
sum(bank$LIMIT_BAL > 500000) # outliers
# create an ELITE status for credit card holders with huge balances
# DID NOT HELP
# bank$ELITE <- ifelse(bank$LIMIT_BAL > 500000, 1, 0)
# bank$ELITE <- as.factor(bank$ELITE)
# levels(bank$ELITE) <- c("Normal", "ELITE")

# # would be also nice to know when a customer is getting close to it's limit
# plot(bank$BILL_AMT1, bank$LIMIT_BAL)
# mean(bank$BILL_AMT1)
# mean(bank$LIMIT_BAL)
# # say within 5 grand of limit
# bank$CLOSE_TO_LIMIT <- ifelse(abs(bank$LIMIT_BAL - bank$BILL_AMT1) < 1000, 1, 0)
# bank$CLOSE_TO_LIMIT <- as.factor(bank$CLOSE_TO_LIMIT)
# levels(bank$CLOSE_TO_LIMIT) <- c("NOT CLOSE", "CLOSE")
# 


# SEX MARRIAGE 
bank$SEX_MARRIAGE <- with(bank, interaction(SEX, MARRIAGE))
levels(bank$SEX_MARRIAGE)

# SEX EDUCATION
bank$SEX_EDUCATION <- with(bank, interaction(SEX, EDUCATION))
levels(bank$SEX_EDUCATION)

#  change in bill amount
bank <- bank %>% mutate(BILL_CHANGE1 = BILL_AMT1-BILL_AMT2,
                        BILL_CHANGE2 = BILL_AMT2-BILL_AMT3,
                        BILL_CHANGE3 = BILL_AMT3-BILL_AMT4,
                        BILL_CHANGE4 = BILL_AMT4-BILL_AMT5,
                        BILL_CHANGE4 = BILL_AMT5-BILL_AMT6
)

str(bank)


#checks if balance is paid in full
# bank <- bank %>% mutate(PayFull1 = ifelse(BILL_AMT2-PAY_AMT1 <0, 1, 0),
#                         PayFull2 = ifelse(BILL_AMT3-PAY_AMT2 <0, 1, 0),
#                         PayFull3 = ifelse(BILL_AMT4-PAY_AMT3 <0, 1, 0),
#                         PayFull4 = ifelse(BILL_AMT5-PAY_AMT4 <0, 1, 0),
#                         PayFull5 = ifelse(BILL_AMT6-PAY_AMT5 <0, 1, 0)
# )





#checks if paid amount is greater then balance amount
# bank <- bank %>% mutate(Bal_Minuse_Pay1 = BILL_AMT2-PAY_AMT1,
#                         Bal_Minuse_Pay2 = BILL_AMT3-PAY_AMT2,
#                         Bal_Minuse_Pay3 = BILL_AMT4-PAY_AMT3,
#                         Bal_Minuse_Pay4 = BILL_AMT5-PAY_AMT4,
#                         Bal_Minuse_Pay5 = BILL_AMT6-PAY_AMT5
# )



## AGE GROUPS
# bank <- bank %>% mutate(AgeGroup = ifelse(AGE<30, "Under 30",
#                                           ifelse(AGE<65,"30 to 64","65 and Over"
#                                           )))
# bank$AgeGroup <- as.factor(bank$AgeGroup)

# Need something better 



# 
# 
# # # TOTAL PAY
# # bank$TOTAL_PAY <- bank$PAY_AMT1 + bank$PAY_AMT2 + bank$PAY_AMT3 + bank$PAY_AMT4 + bank$PAY_AMT5 + bank$PAY_AMT6
# # 
# # # now we want a variable that tells us if people are not paying their bills 
# bank$SUM_BILL <- apply(bank[,13:18], 1, function(x) length(which(x==0)))
# bank$SUM_BILL <- NULL
# # 
# # 
# # # we think sex, education, marriage have an affect on limit balance
# # # interaction
# bank$COMBO <- as.numeric(bank$SEX) + as.numeric(bank$EDUCATION) + as.numeric(bank$EDUCATION)
# bank$COMBO <- as.factor(bank$COMBO)
# # the lower the better for ability to repay based on analysis

### try combining SEX, MARRIAGE, AGE 



# intuition into combining SEX, MARRIAGE and EDUCATION 
ggplot(bank, aes(SEX, ..count..)) + geom_bar(aes(fill = default.payment.next.month), position = "dodge")
table(bank$SEX)
ggplot(bank, aes(MARRIAGE, ..count..)) + geom_bar(aes(fill = default.payment.next.month), position = "dodge")
ggplot(bank, aes(EDUCATION, ..count..)) + geom_bar(aes(fill = default.payment.next.month), position = "dodge")



# skewness of numeric variables
str(bank)
skewness(bank$BILL_AMT1)
skewness(bank$BILL_AMT2)
skewness(bank$BILL_AMT3)
skewness(bank$BILL_AMT4)
skewness(bank$BILL_AMT5)
skewness(bank$BILL_AMT6)



# we also have the issue of negative amounts 
sum(bank$BILL_AMT1 < 0) # also in the other bill amounts
summary(bank)

# this could be because they had just paid a large amount out and so their bill is negative that month



# splitting and training the data 

set.seed(77850) #set a random number generation seed to ensure that the split is the same everytime
inTrain <- createDataPartition(y = bank$default.payment.next.month, p = 0.8, list = FALSE)
train <- bank[ inTrain,]
test <- bank[ -inTrain,]


#######################################################
############################################################ LOGISTIC 
##############################################################


# logistic 
model_logistic<- glm(default.payment.next.month~.,data=train, family="binomial"(link="logit"))
summary(model_logistic)
# 
# remove NA levels
levels(bank$PAY_4)[levels(bank$PAY_4)==6] <- NA
bank <- bank[complete.cases(bank), ]
levels(bank$PAY_3)[levels(bank$PAY_3)==6] <- NA
bank <- bank[complete.cases(bank), ]
levels(bank$PAY_5)[levels(bank$PAY_5)==7] <- NA
bank <- bank[complete.cases(bank), ]
levels(bank$PAY_5)[levels(bank$PAY_5)==6] <- NA
bank <- bank[complete.cases(bank), ]
summary(bank)
sum(is.na(bank))


# re-split
set.seed(77850) 
inTrain <- createDataPartition(y = bank$default.payment.next.month, p = 0.8, list = FALSE)
train <- bank[ inTrain,]
test <- bank[ -inTrain,]


# re-run logistic
model_logistic<- glm(default.payment.next.month~.,data=train, family="binomial"(link="logit"))
summary(model_logistic) # NO NAs left


library(MASS)
model_logistic_stepwiseAIC <- stepAIC(model_logistic,direction = c("both"),trace = 1) #AIC stepwise
summary(model_logistic_stepwiseAIC) # only included BILL AMT 2 interestingly 
par(mfrow=c(1,4))
plot(model_logistic_stepwiseAIC) #Error plots: similar nature to lm plots
par(mfrow=c(1,1))



###Finding predicitons: probabilities and classification
logistic_probabilities<-predict(model_logistic_stepwiseAIC,newdata=test,type="response") #Predict probabilities
logistic_ROC_prediction <- prediction(logistic_probabilities, test$default.payment.next.month)
logistic_ROC <- performance(logistic_ROC_prediction,"tpr","fpr") #Create ROC curve data

plot(logistic_ROC) #Plot ROC curve

####AUC (area under curve)
auc.tmp <- performance(logistic_ROC_prediction,"auc") #Create AUC data
logistic_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
logistic_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value
#AUC#0.786#
# achieved by simpling dropping both ID columns 
# removing NAs in original factor levels
# and the uncommented featured engineers 




################################ Decision Tree #######################
# RPART
# The rpart method has an important "complexity parameter", cp, which determines how big the tree is.  

CART_cp = rpart.control(cp = 0.0002)

rpart_tree<-rpart(default.payment.next.month~.,data=train, method="class", control=CART_cp) #Run ctree on training data

printcp(rpart_tree) # Understand the relationship between the error and cp
plotcp(rpart_tree) # As a rule of thumb pick up the largest cp which does not give a substantial drop in error

prunned_rpart_tree<-prune(rpart_tree, cp=0.002) #Prun the tree. Play with cp to see how the resultant tree changes
plot(as.party(prunned_rpart_tree), type = "extended",gp = gpar(fontsize = 7)) #Plotting the tree (adjust fontsize if needed)

rpart_prediction_class<-predict(prunned_rpart_tree,newdata=test, type="class") #Predict classification (for confusion matrix)

rpart_probabilities_testing <-predict(prunned_rpart_tree,newdata=test,type = "prob") #Predict probabilities
rpart_pred_testing <- prediction(rpart_probabilities_testing[,2], test$default.payment.next.month) #Calculate errors
rpart_ROC_testing <- performance(rpart_pred_testing,"tpr","fpr") #Create ROC curve data
plot(rpart_ROC_testing) #Plot ROC curve

auc.tmp <- performance(rpart_pred_testing,"auc") #Create AUC data
rpart_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
rpart_auc_testing #Display AUC value
## 0.69 ## 


### XGBOOST #### 
if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not

pacman::p_load("caret","ROCR","lift","xgboost")

training.x <- model.matrix(default.payment.next.month~ ., data = train)
testing.x <- model.matrix(default.payment.next.month~ ., data = test)

model_XGboost <- xgboost(data = data.matrix(training.x[,-1]), 
                         label = as.numeric(as.character(train$default.payment.next.month)), 
                         eta = 0.1,
                         max_depth = 20, 
                         nround=50, 
                         objective = "binary:logistic")

####ROC Curve
XGboost_prediction <- predict(model_XGboost,newdata=testing.x[,-1], type="response")
XGboost_pred_testing <- prediction(XGboost_prediction, test$default.payment.next.month) #Calculate errors
XGboost_ROC_testing <- performance(XGboost_pred_testing,"tpr","fpr") #Create ROC curve data
plot(XGboost_ROC_testing) #Plot ROC curve

####AUC
auc.tmp <- performance(XGboost_pred_testing,"auc") #Create AUC data
XGboost_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
XGboost_auc_testing 
### 0.7753 ###
# boosted by removing OLD ID
# boossted from 0.7655 to 0.7737 removing both IDs
### to 0.7746 by adding in SEX_EDUCATIOn and SEX_MARRIAGE 
## 0.7746 to 0.7752849 adding BILL PERCENT CHANGE 

# which variables were important?



### RANDOM FOREST ### 
#install.packages("randomForest")
library(randomForest)
model_forest <- randomForest(default.payment.next.month~ ., data=train, 
                             importance=TRUE,proximity=TRUE,
                             cutoff = c(0.5, 0.5),type="classification") #cutoffs need to be determined for class 0 and class 1. By default 50/50, but need not be those necessarily
print(model_forest)   
plot(model_forest)
importance(model_forest)
varImpPlot(model_forest) # PAY_0 important
# unintuitively, MARRIAGE, EDUCATION, SEX are unimportant 

###Finding predicitons: probabilities and classification
forest_probabilities <- predict(model_forest, newdata=test,type="prob") #Predict probabilities -- an array with 2 columns: for not retained (class 0) and for retained (class 1)


####ROC Curve
forest_ROC_prediction <- prediction(forest_probabilities[,2], test$default.payment.next.month) #Calculate errors
forest_ROC <- performance(forest_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(forest_ROC) #Plot ROC curve

####AUC (area under curve)
AUC.tmp <- performance(forest_ROC_prediction,"auc") #Create AUC data
forest_AUC <- as.numeric(AUC.tmp@y.values) #Calculate AUC
forest_AUC #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% -
### 0.7729479 ###
# this was achieved after moving the NAs that came up in logistic regression summary (different smaller factor levels)




### QUESTION 3 ### 
# Rerun your best model with and without the use of the "SEX" variable. 
# Comment on the resultant predictive performance of the two models.


# just using the current model with 0.78598 AUC logistic regression 

### taking out sex completely 
# this means taking it out of our interactive factors too
str(bank)
new_bank <- bank
new_bank$SEX_EDUCATION <- NULL
new_bank$SEX_MARRIAGE <- NULL
new_bank$SEX <- NULL

# resplit
set.seed(77850) 
inTrain_new <- createDataPartition(y = new_bank$default.payment.next.month, p = 0.8, list = FALSE)
train_new <- new_bank[ inTrain_new,]
test_new <- new_bank[ -inTrain_new,]

# re-run logit without sex
model_logistic_new<- glm(default.payment.next.month~.,data=train_new, family="binomial"(link="logit"))
summary(model_logistic_new)
model_logistic_stepwiseAIC_new <- stepAIC(model_logistic_new,direction = c("both"),trace = 1)
logistic_probabilities_new<-predict(model_logistic_stepwiseAIC_new,newdata=test_new,type="response") #Predict probabilities
logistic_ROC_prediction_new <- prediction(logistic_probabilities_new, test_new$default.payment.next.month)
auc.tmp_new <- performance(logistic_ROC_prediction_new,"auc") #Create AUC data
logistic_auc_testing_new <- as.numeric(auc.tmp_new@y.values) #Calculate AUC
logistic_auc_testing_new ### 0.77664 ### without SEX 

### without SEX it drops by a full 1% which is pretty solid drop ### 


### QUESTION 4 ### 
str(bank)
nrow(bank)

# male only 
male_bank <- bank %>% filter(SEX == "male")
# drop SEX related variables
male_bank$SEX <- NULL
male_bank$SEX_MARRIAGE <- NULL
male_bank$SEX_EDUCATION <- NULL
str(male_bank)

# resplit
set.seed(77850) 
inTrain_male <- createDataPartition(y = male_bank$default.payment.next.month, p = 0.8, list = FALSE)
train_male <- male_bank[ inTrain_male,]
test_male <- male_bank[ -inTrain_male,]

# re-run logit without sex
model_logistic_male <- glm(default.payment.next.month~.,data=train_male, family="binomial"(link="logit"))
summary(model_logistic_male)
model_logistic_stepwiseAIC_male <- stepAIC(model_logistic_male,direction = c("both"),trace = 1)
logistic_probabilities_male <- predict(model_logistic_stepwiseAIC_male,newdata=test_male,type="response") #Predict probabilities
logistic_ROC_prediction_male <- prediction(logistic_probabilities_male, test_male$default.payment.next.month)
auc.tmp_male <- performance(logistic_ROC_prediction_male,"auc") #Create AUC data
logistic_auc_testing_male <- as.numeric(auc.tmp_male@y.values) #Calculate AUC
logistic_auc_testing_male


# female only now
# male only 
female_bank <- bank %>% filter(SEX == "female")
# drop SEX related variables
female_bank$SEX <- NULL
female_bank$SEX_MARRIAGE <- NULL
female_bank$SEX_EDUCATION <- NULL
str(female_bank)

# resplit
set.seed(77850) 
inTrain_female <- createDataPartition(y = female_bank$default.payment.next.month, p = 0.8, list = FALSE)
train_female <- female_bank[ inTrain_female,]
test_female <- female_bank[ -inTrain_female,]

# re-run female only
model_logistic_female <- glm(default.payment.next.month~.,data=train_female, family="binomial"(link="logit"))
summary(model_logistic_female)
model_logistic_stepwiseAIC_female <- stepAIC(model_logistic_female,direction = c("both"),trace = 1)
logistic_probabilities_female <- predict(model_logistic_stepwiseAIC_female,newdata=test_female,type="response") #Predict probabilities
logistic_ROC_prediction_female <- prediction(logistic_probabilities_female, test_female$default.payment.next.month)
auc.tmp_female <- performance(logistic_ROC_prediction_female,"auc") #Create AUC data
logistic_auc_testing_female <- as.numeric(auc.tmp_female@y.values) #Calculate AUC
logistic_auc_testing_female

# female graph and male graph 
length(logistic_probabilities_female) # 618 
logistic_probabilities_female <- as.data.frame(logistic_probabilities_female)
colnames(logistic_probabilities_female) <- "Default_Probability"
logistic_probabilities_female$female <- "Female"
str(logistic_probabilities_female)
colnames(logistic_probabilities_female)[1] <- "Likelihood"
colnames(logistic_probabilities_female)[2] <- "Sex"
logistic_probabilities_male <- as.data.frame(logistic_probabilities_male)
colnames(logistic_probabilities_male) <- "Deault_Probability"
logistic_probabilities_male$male <- "Male"
colnames(logistic_probabilities_male)[1] <- "Likelihood"
colnames(logistic_probabilities_male)[2] <- "Sex"



# rbind
graph <- rbind(logistic_probabilities_male, logistic_probabilities_female)
graph$Sex <- as.factor(graph$Sex)
head(graph)
tail(graph)

# plot
plot(graph$Likelihood, col=ifelse(graph$Sex=="Male","red", "green"), xlab="Credit Card Holders",ylab="Default Likelihood")
abline(h=0.95, lwd = 3,col="purple")

# analysis
table(bank$SEX)
table(graph$Sex)

