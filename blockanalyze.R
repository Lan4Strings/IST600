#####################################
##### Do NOT delete this block #####
setwd(Sys.getenv('PROJECT_HOME'))
####################################

####################################
## write code to read input csv into data frame
df <- read.csv('merged_block.csv')
####################################

## start writing your R code from here
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

rf <- randomForest(VacantBuil ~ LandUse + AssessedVa + WaterServi + YearBuilt + Total + Occupied_Pr + Vacant_Pr + Units ,data = finalData, na.action = na.exclude)
rf

plot(rf)

#From the graph we see that the error is same from 150 trees. We can choose any number of trees after that. We chose 200


rf <- randomForest(VacantBuil ~ LandUse + AssessedVa + ZIP + WaterServi + YearBuilt + Total + Occupied_Pr + Vacant_Pr + Units ,ntree = 200,data = finalData, na.action = na.exclude)
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


###########################################################
#Regression Model
###########################################################

#using uncategorized data for regression
finalData <- backup

library(DMwR)
temp <- knnImputation(finalData,3)
apply(temp,2,pMiss)

finalData <- temp

# Running a Regression model
str(finalData)
View(finalData)
summary(finalData)
unique(finalData$LandUse)

regression.data <- finalData
str(regression.data)
summary(regression.data)

# Preparing variables for regression. Converting significant variables into numeric and normalizing them.
normalize <- function(x)
{
  (x-min(x))/max(x)-min(x)
}

# Assessed Value

regression.data$AssessedVa <-  ifelse(regression.data$AssessedVa <= 75000, 1,ifelse(regression.data$AssessedVa > 75000 & regression.data$AssessedVa <= 2000000, 2,3))
hist(regression.data$AssessedVa)

# Year Built

hist(regression.data$YearBuilt)
regression.data$YearBuilt <- ifelse(regression.data$YearBuilt <=1900,1,ifelse(regression.data$YearBuilt > 1900 & regression.data$YearBuilt<=1975,2,3))

# Total Crime

hist(regression.data$Total)
unique(regression.data$Total)

# Vacant Building


# Running the regression Model

str(regression.data)
summary(regression.data)

regression.Model.1 <- lm(Vacant_Pr ~ AssessedVa + YearBuilt + Total  + Units
                           ,data = regression.data)
summary(regression.Model.1)

regression.Model.2 <- lm(Occupied_Pr ~ AssessedVa + YearBuilt + Total  + Units
                           ,data = regression.data)
summary(regression.Model.2)

# Visualizing the results for Y= Vancant_Pr

temp <- regression.data[,c("Vacant_Pr",'AssessedVa','YearBuilt',"Total","Units")]
#View(temp)
tempMelt <- melt(temp, id='Vacant_Pr')
#View(tempMelt)

library(ggplot2)
#Heat Map 
ggplot(tempMelt, aes(x=Vacant_Pr,y=variable, fill=value)) + geom_tile() + scale_fill_gradient(low="yellow", high = "orange")

# Scatter Plot
ggplot(regression.data, aes(x=YearBuilt, y=Vacant_Pr, size=Units, col=Units,
                            group=Owner_Occupied))+geom_jitter(width = 0.5, height = 1) + geom_abline()


# Visualizing the results for Y = Occupied_Pr

temp1 <- regression.data[,c("Occupied_Pr",
                            'AssessedVa','YearBuilt',"Total","Units")]
#View(temp)
tempMelt1 <- melt(temp1, id='Occupied_Pr')
#View(tempMelt)

#Heat Map 
ggplot(tempMelt1, aes(x=Occupied_Pr,y=variable, fill=value)) + geom_tile() + scale_fill_gradient(low="yellow", high = "orange")

# Scatter Plot
ggplot(regression.data, aes(x=YearBuilt, y=Occupied_Pr, size=Units, col=Units,
                            group=Owner_Occupied))+geom_jitter(width = 0.5, height = 1) + geom_abline()
## end your R code and logic 

####################################
##### write output file ############
# add your R code to write output file
####################################


