#####################################
##### Do NOT delete this block #####
setwd(Sys.getenv('PROJECT_HOME'))
####################################

####################################
## write code to read input csv into data frame
df <- read.csv('merged_parcel.csv')
#df <- datamerged
####################################

## start writing your R code from here
datause<-df[,c('VacantBuil','LandUse','YearBuilt','SchoolTXBL','Aggravated.assault','Arson','Robbery','Vehicle.theft')
]
#create dummy varibles
single<-datause$LandUse=='Single Family'
two<-datause$LandUse=='Two Family'
three<-datause$LandUse=='Three Family'
more<-datause$LandUse=='Multiple Residence'
datause<-data.frame(datause,single,two,three,more)
datause<-datause[datause$LandUse=='Single Family'|datause$LandUse=='Two Family'|datause$LandUse=='Three Family'|datause$LandUse=='Multiple Residence',]
datause<-datause[,-2]

#clean data
datause<-datause[datause$SchoolTXBL!=0,]
datause$SchoolTXBL<-as.factor(datause$SchoolTXBL)

#exlop


## end your R code and logic 

####################################
##### write output file ############
# add your R code to write output file
####################################


