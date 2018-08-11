#####################################
##### Do NOT delete this block #####
setwd(Sys.getenv('PROJECT_HOME'))
####################################

####################################
## write code to read input csv into data frame
df <- read.csv('merged_parcel.csv')
####################################

## start writing your R code from here
#Step 1: Feature select and Clean data with removing NAs
dataset<-df[,c("VacantBuil","FullName","block.address","SURA","Nhood","Quad","TNT_NAME","LandUse","Units","Condition","AssessedLa","AssessedVa","YearBuilt","SENIOR_EXE","VET_EXE","INNRSA","DIST","Aggravated.assault","Arson","Burglary","Larceny","Murder","Robbery","Vehicle.theft","Total")]
levels(dataset$INNRSA)<-c("N","Y")
levels(dataset$VET_EXE)<-c("N","Y")
levels(dataset$SENIOR_EXE)<-c("N","Y")

#additional clean(if need)
dataset$YearBuilt[dataset$YearBuilt==0]<-NA
dataset$YearBuilt[is.na(dataset$YearBuilt)]<-median(dataset$YearBuilt,na.rm = 1)
#dataset$VacantBuil<-as.numeric(dataset$VacantBuil)-1

#data selection
dfana<-dataset[,-10]

#Step 2: Create train and test data sets
randnum <- sample(1:dim(dfana)[1])
cutpoint <- randnum[1:(dim(dfana)[1] / 3)]

#2/3 dataset for training and 1/3 for testing.
dataset_test <- dfana[cutpoint,]
dataset_train <- dfana[-1 * cutpoint,]


#Step 3: Models
#lm model
library("ModelMetrics")

md_lm <-
  lm(formula = as.numeric(VacantBuil) ~ Units + AssessedVa + YearBuilt + Aggravated.assault + Arson + Vehicle.theft, data = dataset_train)
summary(md_lm)
Vacant_predict_lm <- round(predict(md_lm, dataset_test, type = "response"))
rmse(as.numeric(dataset_test$VacantBuil), Vacant_predict_lm)

#ksvm model
library("kernlab")
#Using Least Squares Support Vector Machine with Radial Basis Function Kernel
md <-
  ksvm(
    VacantBuil ~ Units + AssessedVa + YearBuilt + Aggravated.assault + Arson + Robbery + Vehicle.theft + Total,
    data = dataset_train,
    kernel = "rbfdot",
    kpar = "automatic"
  )
md
Vacant_predict <- predict(md, dataset_test, type = "response")
rmse(dataset_test$VacantBuil, Vacant_predict)
confusionMatrix(dataset_test$VacantBuil,Vacant_predict)

#SVM model
library("e1071")
md_svm <- svm(VacantBuil ~ Units + AssessedVa + YearBuilt + Aggravated.assault + Arson + Robbery + Vehicle.theft + Total, data = dataset_train)
md_svm
Vacant_predict_svm <- predict(md_svm, dataset_test)
rmse(dataset_test$VacantBuil, Vacant_predict_svm)
confusionMatrix(dataset_test$VacantBuil,Vacant_predict_svm)


#use Cross-Validated to tune model
library('caret')
library('RcppRoll')
library('lubridate')
fitControl <- trainControl(method = "repeatedcv",
                           number = 3,
                           #(2/3 for training and 1/3 for testing)
                           repeats = 10) #repeat sampling 10 times

#method="svmRadial" is for SVM with Radial Basis Function Kernel
svmtest <-
  caret::train(
    VacantBuil ~ Units + AssessedVa + YearBuilt + Aggravated.assault + Arson + Robbery + Vehicle.theft + Total,
    data = dfana,
    method = "svmRadial",
    trControl = fitControl,
    tuneLength = 10
  )
bestC_svm <- svmtest$finalModel@param$C

nbtest <-
  caret::train(
    as.factor(VacantBuil) ~ Units + AssessedVa + YearBuilt + Aggravated.assault + Arson + Robbery + Vehicle.theft + Total,
    data = dfana,
    method = "naive_bayes",
    trControl = fitControl,
    tuneLength = 10
  )
bestfL <- nbtest$bestTune$fL



#after tuning
best_ksvm <-
  ksvm(
    VacantBuil ~ Units + AssessedVa + YearBuilt + Aggravated.assault + Arson + Robbery + Vehicle.theft + Total,
    data = dfana,
    kernel = "rbfdot",
    kpar = "automatic",
    C = bestC_svm
  )
predictksvm <- predict(best_ksvm, dataset_test)
rmse(dataset_test$VacantBuil, predictksvm)
confusionMatrix(dataset_test$VacantBuil,predictksvm)


best_svm <-
  svm(VacantBuil ~ Units + AssessedVa + YearBuilt + Aggravated.assault + Arson + Robbery + Vehicle.theft + Total,C=bestC_svm)
predictsvm <- predict(best_svm, dataset_test)
rmse(dataset_test$VacantBuil, predictsvm)
confusionMatrix(dataset_test$VacantBuil,predictsvm)

best_nb <-
  naiveBayes(VacantBuil ~ Units + AssessedVa + YearBuilt + Aggravated.assault + Arson + Robbery + Vehicle.theft + Total,
             data = dataset_train,
             laplace = bestfL)
predictnb <-predict(best_nb, dataset_test, type = "raw")[, 2]
rmse(dataset_test$VacantBuil, predictnb)
confusionMatrix(dataset_test$VacantBuil,predictnb)



#####
###### other analyzing method
#####
#loading data with the selected columns
finalData <- df
finalData <- finalData[-(which(finalData$VacantBuil == '')),]
backup <- finalData

#converting data into categories 
finalData$WaterServi <- as.character(finalData$WaterServi)
finalData$WaterServi[which(finalData$WaterServi == '')] <- "unknown"
finalData$WaterServi <- as.factor(finalData$WaterServi)


finalData$VacantBuil <- as.character(finalData$VacantBuil)
finalData$VacantBuil[which(finalData$VacantBuil == 1)] <- "Y"
finalData$VacantBuil <- as.factor(finalData$VacantBuil)

is.na(finalData$YearBuilt) <- finalData$YearBuilt == 0 
hist(finalData$YearBuilt)


finalData$YearBuilt <- as.character(finalData$YearBuilt)
finalData$YearBuilt[which(finalData$YearBuilt <= 1900)] <- "Very Old"
finalData$YearBuilt[which(finalData$YearBuilt > 1900 & finalData$YearBuilt <= 1975)] <- "Old"
finalData$YearBuilt[which(finalData$YearBuilt > 1975 & finalData$YearBuilt <= 2017 )] <- "New"
finalData$YearBuilt <- as.factor(finalData$YearBuilt)

summary(finalData)


finalData$AssessedVa <-  ifelse(finalData$AssessedVa <= 75000, 'Cheap',
                                ifelse(finalData$AssessedVa > 75000 & finalData$AssessedVa <= 2000000, 'Moderate','Expensive'))
finalData$AssessedVa <- as.factor(finalData$AssessedVa)


#Treating NA values

#checking how much % of data is NAs
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(finalData,2,pMiss) #applying to columns

#Census columns have 15% or more of missing data
library(mice)
md.pattern(finalData) #shows the pattern of missing data

library(VIM)
aggr_plot <- aggr(finalData, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(finalData), cex.axis=.3,cex.numbers=0.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

library(DMwR)
temp <- knnImputation(finalData,3)
apply(temp,2,pMiss)
View(temp)

finalData <- temp
clean_backup <- finalData
#finalData <- clean_backup


################################
#Logit Model
################################

ldata <- finalData

ldata$VacantBuil <- ifelse(ldata$VacantBuil == "Y",1,0)

lm1 <- glm(formula = VacantBuil ~ LandUse+ AssessedVa + WaterServi + YearBuilt + Total + Occupied_Pr + Vacant_Pr + Units, data = ldata, family = binomial)
summary(lm1)

#selecting a model by taking only the significant predictors
lm2 <- glm(formula = VacantBuil ~ LandUse + AssessedVa + WaterServi + YearBuilt, data = ldata, family = binomial)
summary(lm2)

lm3 <- glm(formula = VacantBuil ~ AssessedVa + WaterServi + YearBuilt, data = ldata, family = binomial)
summary(lm3)

#Based on AIc criteria lm2 is better than lm3. Hence we select lm2 as our model

# Chi-square ( Log-likehood Ratio test)

anova(lm1, lm2, test ="Chisq") # p is not less than 0.05, hence we cannot reject H0 and reduced model is a better fit

#predicting on complete data

predict <- predict(lm2, type='response')
predict <- ifelse(predict < 0.5,0,1)
View(predict)

# Creating a confusion matrix

compTable1 <- data.frame(ldata$VacantBuil,predict)
colnames(compTable1) <- c('Actual','Predicted')
table(compTable1)
#Accuracy = 0.973

#ROCR Curve
library(ROCR)
ROCRpred <- prediction(predict, ldata$VacantBuil)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

#Using Training and Testing Data
randIndex <- sample(1:dim(ldata)[1])

train_cutpoint2_3 <- floor((2*dim(ldata)[1])/3)
trainData <- ldata[randIndex[1:train_cutpoint2_3],]
View(testData)

testCutpoint <- dim(ldata)[1]-(train_cutpoint2_3+1)
testData <- ldata[randIndex[train_cutpoint2_3+1:testCutpoint],]


lm2 <- glm(formula = VacantBuil ~ LandUse + AssessedVa + WaterServi + YearBuilt, data = trainData, family = binomial)
summary(lm2)

predict_test <- predict(lm2, testData, type='response')
predict_test <- ifelse(predict_test < 0.5,0,1)
View(predict_test)

# Creating a confusion matrix
compTable1 <- data.frame(testData$VacantBuil,predict_test)
colnames(compTable1) <- c('Actual','Predicted')
table(compTable1)

#Accuracy = 0.973

#ROCR Curve
ROCRpred <- prediction(predict_test, testData$VacantBuil)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

#fitted values

fit <- lm2$fitted.values 
hist(fit)

###########################################################
#Random Forest
###########################################################

library(randomForest)
library(caret)
str(finalData)
View(finalData)
finalData <- finalData[,-9]

#Training and Testing Data
randIndex <- sample(1:dim(finalData)[1])

train_cutpoint2_3 <- floor((2*dim(finalData)[1])/3)
train <- finalData[randIndex[1:train_cutpoint2_3],]

testCutpoint <- dim(finalData)[1]-(train_cutpoint2_3+1)
test <- finalData[randIndex[train_cutpoint2_3+1:testCutpoint],]

rf <- randomForest(VacantBuil ~ LandUse + AssessedVa + WaterServi + YearBuilt + Total  + Units ,data = finalData, na.action = na.exclude)
rf

plot(rf)

#From the graph we see that the error is same from 150 trees. We can choose any number of trees after that. We chose 200


rf <- randomForest(VacantBuil ~ LandUse + AssessedVa + WaterServi + YearBuilt + Total + Units ,ntree = 200,data = finalData, na.action = na.exclude)
rf

#Prediction on complete data
predict_rf <- predict(rf,finalData)
confusionMatrix(predict_rf,finalData$VacantBuil)

#Accuracy = 0.997

#Tuning the RF model to avoid over fitting
t <- tuneRF(finalData[,-5],finalData[,5],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 200,
            improve = 0.05)

#Here we see that OOB - out of bag error is least at mtry = 3, hence we select that for our model 

#Number of nodes in each tree
hist(treesize(rf),main = "No of nodes in each tree",col = "green")

#Prediction on test data
rf <- randomForest(VacantBuil~., data = train, mtry = 3,ntree = 200 ,importance = TRUE, proximity = TRUE)
rf

predict_rf <- predict(rf,test)
confusionMatrix(predict_rf,test$VacantBuil)
#Accuracy = 0.976

vif <- importance(rf)
varImp(rf)
varImpPlot(rf)
varImpPlot(rf,sort = T, n.var = 5, main = "Top 5 variables")
varUsed(rf)

par("mar")
par(mar=c(1,1,1,1))
par(mfrow=c(4, 2), xpd=NA)

#partial dependencies
partialPlot(rf,train,WaterServi)
partialPlot(rf,train,LandUse)
partialPlot(rf,train,Total)
partialPlot(rf,train,YearBuilt)
partialPlot(rf,train,AssessedVa)

#When building is vacant
partialPlot(rf,train,WaterServi,"Y")
partialPlot(rf,train,LandUse,"Y")
partialPlot(rf,train,Total,"Y")
partialPlot(rf,train,YearBuilt,"Y")
partialPlot(rf,train,AssessedVa,"Y")

dev.off()

#variable influence barplot
bp <- barplot(t(vif/sum(vif)),las = 2)




###########################################################
#Apriori Rules
###########################################################

library(arules)
library(arulesViz)
apriori.data <- clean_backup
str(apriori.data)

#new_data <- as(trialdf1,"transactions")
apriori.data$AssessedVa <-  ifelse(apriori.data$AssessedVa <= 75000, 'Low',ifelse(apriori.data$AssessedVa > 75000 & apriori.data$AssessedVa <= 2000000, 'Medium','High'))
apriori.data$Total <- ifelse(apriori.data$Total <= 5, 'Low',ifelse(apriori.data$Total > 20,'High','Medium'))
table(apriori.data$Total)

### Apriori ####

# Converting all the varibales into factors to run apriori function
for(i in 1:ncol(apriori.data))
{
  apriori.data[,i] <- as.factor(apriori.data[,i])
}

#aprioriDf <- na.omit(aprioriDf)
#View(aprioriDf)

ruleset <- apriori(apriori.data, parameter = list(support=0.01,confidence=0.3, target='rules'),
                   appearance = list(default='lhs',rhs=('VacantBuil=Y')))

inspect(ruleset)

importantRules<-ruleset[quality(ruleset)$lift>16]
importantRules
inspect(importantRules)


ruleset2 <- apriori(apriori.data, parameter = list(support=0.5,confidence=0.5, target='rules'),
                    appearance = list(default='lhs',rhs=('VacantBuil=N')))

inspect(ruleset2)

importantRules2<-ruleset2[quality(ruleset2)$lift>1.0484]
importantRules2
inspect(importantRules2)


#plot(ruleset, method="graph");

plot(importantRules, method="graph", control=list(type="items"));

aprioriDfTransaction <- as(apriori.data, 'transactions')
itemFrequencyPlot(aprioriDfTransaction, support=0.015,col='cyan')



## end your R code and logic 

####################################
##### write output file ############
# add your R code to write output file
####################################

