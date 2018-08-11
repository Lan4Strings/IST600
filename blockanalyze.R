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

###########################################################
#Regression Model
###########################################################
# Running a Regression model
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
ggplot(regression.data, aes(x=YearBuilt, y=Vacant_Pr, size=Units, col=Units))+geom_jitter(width = 0.5, height = 1) + geom_abline()


# Visualizing the results for Y = Occupied_Pr

temp1 <- regression.data[,c("Occupied_Pr",
                            'AssessedVa','YearBuilt',"Total","Units")]
#View(temp)
tempMelt1 <- melt(temp1, id='Occupied_Pr')
#View(tempMelt)

#Heat Map 
ggplot(tempMelt1, aes(x=Occupied_Pr,y=variable, fill=value)) + geom_tile() + scale_fill_gradient(low="yellow", high = "orange")

# Scatter Plot
ggplot(regression.data, aes(x=YearBuilt, y=Occupied_Pr, size=Units, col=Units,))+geom_jitter(width = 0.5, height = 1) + geom_abline()
## end your R code and logic 

####################################
##### write output file ############
# add your R code to write output file
####################################


