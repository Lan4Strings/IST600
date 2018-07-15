#####################################
##### Do NOT delete this block #####
setwd(Sys.getenv('PROJECT_HOME'))
####################################

####################################
## write code to read input csv into data frame
df <- read.csv('crimedata.csv')
####################################

## start writing your R code from here
datanew<-df

#replace all NAs
datanew$Aggravated.assault[is.na(datanew$Aggravated.assault)]<-0
datanew$Arson[is.na(datanew$Arson)]<-0
datanew$Burglary[is.na(datanew$Burglary)]<-0
datanew$Larceny[is.na(datanew$Larceny)]<-0
datanew$Murder[is.na(datanew$Murder)]<-0
datanew$Robbery[is.na(datanew$Robbery)]<-0
datanew$Vehicle.theft[is.na(datanew$Vehicle.theft)]<-0

df<-datanew
## end your R code and logic 

####################################
##### write output file ############
write.csv(df, file = 'crime.csv')
####################################



