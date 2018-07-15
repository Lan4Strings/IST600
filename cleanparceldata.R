#####################################
##### Do NOT delete this block #####
setwd(Sys.getenv('PROJECT_HOME'))
####################################

####################################
## write code to read input csv into data frame
df <- read.csv('parceldata.csv')
####################################

## start writing your R code from here
datanew <- data.frame(df)

#keep residential buildings 
datanew<-datanew[datanew$LandUse!="Commercial",]
datanew<-datanew[datanew$LandUse!="Cemetery",]
datanew<-datanew[datanew$LandUse!="Community Services",]
datanew<-datanew[datanew$LandUse!="Industrial",]
datanew<-datanew[datanew$LandUse!="Parking",]
datanew<-datanew[datanew$LandUse!="Parks",]
datanew<-datanew[datanew$LandUse!="Recreation",]
datanew<-datanew[datanew$LandUse!="Religious",]
datanew<-datanew[datanew$LandUse!="Schools",]
datanew<-datanew[datanew$LandUse!="Utilities",]
datanew<-datanew[datanew$LandUse!="Vacant Land",]
datanew$LandUse<-factor(datanew$LandUse)
summary(datanew$LandUse)


#remove rows without address
datanew<-datanew[datanew$FullName!=" ",]
datanew$FullName<-factor(datanew$FullName)

#remove rows without vacancy info
datanew<-datanew[datanew$VacantBuil!=" ",]
datanew$VacantBuil<-factor(datanew$VacantBuil)

#format "Bankruptcy"
datanew$Bankruptcy<-as.character(datanew$Bankruptcy)
datanew$Bankruptcy[datanew$Bankruptcy==' ']<-'N'
datanew$Bankruptcy<-factor(datanew$Bankruptcy)

df<-datanew

## end your R code and logic 

####################################
##### write output file ############
write.csv(df, file = 'parcel.csv')
####################################



