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




## end your R code and logic 

####################################
##### write output file ############
# add your R code to write output file
####################################

