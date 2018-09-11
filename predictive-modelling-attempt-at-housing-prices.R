## libraries needed
library(readr)
library(data.table)
library(dplyr)
library(taRifx)
library(caret)
library(anchors)
library(mice)
library(naniar)
library(e1071)
library(reshape2)
library(glmnet)

## loading in the data 
# reference: Baert Boerman for this clean way to load in the data
train.dt <- fread(input = "../input/train.csv", sep = ",", nrows = -1,header = T,
na.strings=c("NA","N/A","null"),stringsAsFactors = F,check.names = T,strip.white = T,blank.lines.skip = T,
data.table = T) 

test.dt <- fread(input = "../input/test.csv", sep = ",", nrows = -1,header = T,na.strings=c("NA","N/A","null"), stringsAsFactors = F,
check.names = T,strip.white = T,blank.lines.skip = T,data.table = T) 

## Create one data set for feature engineering. 
train.dt[, dataPartition:="train"]
test.dt[, SalePrice:=as.integer(NA)] 
test.dt[, dataPartition:="test"]
full.dt <- rbindlist(list(train.dt, test.dt), use.names = F, fill = F)

## Now split our three groups: prices, factors (factor converts), numeric (sq foot and discrete)

## first factors, 46 factors
Factors <- colnames(full.dt)[which(as.vector(full.dt[,sapply(full.dt, class)]) == "character")]
Factors <- setdiff(Factors, "dataPartition")
# adding in MSSubclass, Overall Qual and Overall Cond
ordinals <- c("MSSubClass", "OverallQual", "OverallCond")
FactorNames <- c(Factors, ordinals)

# lastly we have our two pricing vars, 2 pricing
pricingNames <- c("SalePrice", "MiscVal")

## now we move on to numerics, 33 numerics 
numericVarNames <- names(full.dt)[sapply(full.dt, is.numeric)]
numericVarNames <- numericVarNames[!numericVarNames %in% ordinals]
numericVarNames <- numericVarNames[!numericVarNames %in% pricingNames] 

# preparing the data to go into our model by changing var types
# this takes care of our numerics and our two pricing vars
full.dt <- japply(full.dt, which(sapply(full.dt, class)=="integer"), as.numeric)

# now we need to change all characters to factors
# excluding dataPartition
full.dt[,(FactorNames):= lapply(.SD, as.factor), .SDcols = FactorNames]
full.dt$dataPartition <- NULL

### Imputation ### 

# after reviewing the variable description file provided I came up with the following list
# of factors that have NAs that should be a "None" column instead of a missing value
NA_factor_list <- c("Alley", "BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1",
                "BsmtFinType2", "FireplaceQu", "GarageType", "GarageQual", 
                "GarageFinish", "GarageCond", "PoolQC", "Fence", "MiscFeature",
                "MSZoning", "Utilities", "Exterior1st", "Exterior2nd", "KitchenQual",
                "Functional", "SaleType")

# creating the new level "None"
full.dt <- as.data.frame(full.dt)
new_cols <- lapply(full.dt[NA_factor_list], function(x) {
  if(anyNA(x)) {
    levels(x) <- c(levels(x), "None")
    x[is.na(x)] <- "None"
    x}
  else x
})
new_cols <- as.data.frame(new_cols)

full.dt[,colnames(new_cols)] <- new_cols
sum(is.na(full.dt[,colnames(new_cols)])) # it worked

# now we just need to re-order our ordered variables
# thank you to Boerman again for the guidance on re-ordering these factors with his Kernel 
ordered(full.dt$OverallQual, levels = c(1:10))
ordered(full.dt$OverallCond, levels = c(1:10))
ordered(full.dt$KitchenQual, levels = c("None","Po","Fa","TA","Gd","Ex"))
ordered(full.dt$GarageFinish, levels = c("None","Unf","RFn","Fin"))
ordered(full.dt$GarageQual, levels = c("None","Po","Fa","TA","Gd","Ex"))
ordered(full.dt$GarageCond, levels = c("None","Po","Fa","TA","Gd","Ex"))
ordered(full.dt$ExterQual, levels = c("Po","Fa","TA","Gd","Ex"))
ordered(full.dt$ExterCond, levels = c("Po","Fa","TA","Gd","Ex"))
ordered(full.dt$BsmtQual, levels = c("None","Po","Fa","TA","Gd","Ex"))
ordered(full.dt$BsmtCond, levels = c("None","Po","Fa","TA","Gd","Ex"))
ordered(full.dt$BsmtExposure, levels = c("None","No","Mn","Av","Gd"))
ordered(full.dt$BsmtFinType1, levels = c("None","Unf","LwQ","Rec","BLQ","ALQ","GLQ"))
ordered(full.dt$FireplaceQu, levels = c("None","Po","Fa","TA","Gd","Ex"))
ordered(full.dt$Fence, levels = c("None","MnWw","MnPrv","GdWo","GdPrv"))
ordered(full.dt$PoolQC, levels = c("None","Fa","Gd","Ex"))

# forgot to change Electrical's NA to None
factor(ifelse(is.na(full.dt$Electrical), "NONE", paste(full.dt$Electrical)), levels = c(levels(full.dt$Electrical), "None"))
ordered(full.dt$Electrical, levels = c("None","FuseP","Mix","FuseF","FuseA","SBrkr"))

# impute MasVnrType and MasVnrArea values with zeros and Nones for NAs
for (i in 1:nrow(full.dt)){
    if(is.na(full.dt$MasVnrType[i])) {full.dt$MasVnrType[i] <- "None"}
}

for (i in 1:nrow(full.dt)){
    if(is.na(full.dt$MasVnrArea[i])) {full.dt$MasVnrArea[i] <- 0}
}

# let's look at the rest of our missing values
sum(is.na(full.dt)) # 2115 left
gg_miss_var(full.dt) + labs(y = "Look at all the missing ones")# we can see missing values in LotFrontage and GarageYrBlt

# first set all missing values in SalePrice (these are from test) to zero
for (i in 1:nrow(full.dt)){
    if(is.na(full.dt$SalePrice[i])) {full.dt$SalePrice[i] <- 0}
}

# using median replacement
full.dt <- as.data.table(full.dt)
full.dt[, LotFrontage := replace(LotFrontage, is.na(LotFrontage), median(LotFrontage, na.rm=TRUE)), by=.(Neighborhood)]
full.dt <- as.data.frame(full.dt)

# now we just need to deal with GrgYrBuilt missing values (0)
for (i in 1:nrow(full.dt)){
    if(is.na(full.dt$GarageYrBlt[i])) {full.dt$GarageYrBlt[i] <- 0}
}
sum(is.na(full.dt))

# there are so few NAs left, but we need to deal with them... 
# BsmtHalfBath and BsmtFullBath replace with zeros
for (i in 1:nrow(full.dt)){
    if(is.na(full.dt$BsmtHalfBath[i])) {full.dt$BsmtHalfBath[i] <- 0}
    if(is.na(full.dt$BsmtFullBath[i])) {full.dt$BsmtFullBath[i] <- 0}
    if(is.na(full.dt$GarageCars[i])) {full.dt$GarageCars[i] <- 2}
    if(is.na(full.dt$GarageArea[i])) {full.dt$GarageArea[i] <- 480}
    if(is.na(full.dt$BsmtUnfSF[i])) {full.dt$BsmtUnfSF[i] <- 467}
    if(is.na(full.dt$BsmtFinSF2[i])) {full.dt$BsmtFinSF2[i] <- 400}
    if(is.na(full.dt$BsmtFinSF1[i])) {full.dt$BsmtFinSF1[i] <- 368.5}
    if(is.na(full.dt$TotalBsmtSF[i])) {full.dt$TotalBsmtSF[i] <- 998.5}
}

# electrical replace with most common mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

for (i in 1:nrow(full.dt)){
    if(is.na(full.dt$Electrical[i])) {full.dt$Electrical[i] <- Mode(full.dt$Electrical)}
    }
    


# determine skew for each numeric feature
nums <- unlist(lapply(full.dt, is.numeric))  
numerics <- full.dt[,nums]
skewed_feats <- sapply(numerics, skewness)
# keep only features that exceed a threshold for skewness
skewed_feats <- skewed_feats[skewed_feats > 0.75]

# transform excessively skewed features with log(x + 1)
for(x in names(skewed_feats)) {
  full.dt[[x]] <- log(full.dt[[x]] + 1)
}


# adding total square foot feature
full.dt["TotalSF"] <- (full.dt['TotalBsmtSF'] + full.dt['X1stFlrSF'] + full.dt['X2ndFlrSF'])
full.dt$MoSold <- as.factor(full.dt$MoSold)
full.dt$YrSold <- as.factor(full.dt$YrSold)

# more feature engineering



# let's split back into train and test
train <- subset(full.dt, SalePrice > 0)
test <- full.dt[1461:2919,]
dim(train)
dim(test)

# remove outliers
plot(train$SalePrice, train$TotalSF) 
train <- subset(train, GrLivArea < log(4000) & LotArea < log(100000) & TotalSF < 23, GarageArea > log(1400))
nrow(train)





# need to remove factor levels that have few entries
# and get ready for glm
train$dataPartition <- NULL
train <- as.data.frame(train)
numericVarNames <- colnames(train)[sapply(train, is.numeric)]
DFfactors <- train[, !(names(train) %in% numericVarNames)]
DFdummies <- as.data.frame(model.matrix(~.-1, DFfactors))
fewOnes <- which(colSums(DFdummies[1:nrow(train[!is.na(train$SalePrice),]),])<10)
DFdummies <- DFdummies[,-fewOnes] #removing predictors
dim(DFdummies)


# same for test
test$dataPartition <- NULL
test$SalePrice <- NULL
numericVarNames_test <- names(test)[sapply(test, is.numeric)]
DFfactors_test <- test[, !(names(test) %in% numericVarNames)]
DFdummies_test <- as.data.frame(model.matrix(~.-1, DFfactors_test))
fewOnes_test <- which(colSums(DFdummies_test[1:nrow(test[!is.na(test$SalePrice),]),])<10)
DFdummies_test <- DFdummies_test[,-fewOnes] #removing predictors
dim(DFdummies_test)


# recombine our numerics and dummies for train and test
DFdummies <- as.data.frame(DFdummies)
train.nums <- train[,numericVarNames]
train.final <- cbind(DFdummies, train.nums)
dim(train.final)
train.final <- as.data.frame(train.final)
Y <- train.final[,"SalePrice"]
train.final <- train.final[,-which(names(train.final) == "SalePrice")]
train.final <- as.matrix(train.final)


DFdummies_test <- as.data.frame(DFdummies_test)
test.nums <- test[,numericVarNames_test]
test.final <- cbind(DFdummies_test, test.nums)
dim(test.final)
test.final <- as.matrix(test.final)

# time to run our first glm (lasso)
crossval <-  cv.glmnet(x = train.final, y = Y, alpha = 1) #create cross-validation data
plot(crossval)
penalty.lasso <- crossval$lambda.min #determine optimal penalty parameter, lambda
log(penalty.lasso) #see where it was on the graph
lasso.opt.fit <- glmnet(x = train.final, y = Y, alpha = 1, lambda = penalty.lasso) #estimate the model with the optimal penalty
coef(lasso.opt.fit) 
lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = test.final))
nrow(lasso.testing)

# let's try a ridge 
crossval2 <-  cv.glmnet(x = train.final, y = Y, alpha = 0)
plot(crossval2)
penalty.ridge2 <- crossval2$lambda.min 
log(penalty.ridge2) 
ridge.opt.fit <- glmnet(x = train.final, y = Y, alpha = 0, lambda = penalty.ridge2) #estimate the model with that
coef(ridge.opt.fit)
ridge.testing <- exp(predict(ridge.opt.fit, s = penalty.ridge2, newx = test.final))
nrow(ridge.testing)

#SVM 
svm_model<-svm(SalePrice~., data=train, cost = 3)
svm_pred <- exp(predict(svm_model,newdata = test))


# let's try a blend of both
# submission
test.final <- as.data.frame(test.final)
predictions <- (ridge.testing + lasso.testing + svm_pred)/3
predictions <- data.frame(matrix(unlist(predictions), nrow=1459, byrow=T),stringsAsFactors=FALSE)
my_submission <- data_frame("Id" = test$Id, "SalePrice" = predictions[,1])
my_submission$Id <- as.integer(my_submission$Id)
write_csv(my_submission, "submission.csv")






    
    
    
    
    





