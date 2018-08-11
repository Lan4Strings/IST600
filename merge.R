#####################################
##### Do NOT delete this block #####
setwd(Sys.getenv('PROJECT_HOME'))
####################################

####################################
## write code to read input csv into data frame
df1 <- read.csv('parcel.csv')
df2 <- read.csv('crime.csv')
df3 <- read.csv('census.csv')
####################################

## start writing your R code from here
data<-df1[,-1]
datanew<-data.frame(data)
block<-gsub("-.*$","",datanew$StNum)
block<-gsub(" .*$","",block)
block<-gsub("&.*$","",block)
block<-floor(as.numeric(block)/100)*100
datanew$block.address<-paste(block,'block',datanew$StName)
datanew$block.address<-gsub("AVE","AV",datanew$block.address)
datanew$block.address<-tolower(datanew$block.address)
datanew$block.address<-gsub("\\s?&.*$","",datanew$block.address)
datanew$block.address<-gsub(" rear.*$","",datanew$block.address)
datanew$block.address<-gsub(" to .*$","",datanew$block.address)
datanew$block.address<-gsub(" #.*$","",datanew$block.address)
datanew$block.address<-gsub(" \\s+"," ",datanew$block.address)
datanew$block.address<-tolower(datanew$block.address)


crimedata<-df2[,-1]
datacrime<-data.frame(crimedata)
datacrime$Address<-tolower(datacrime$Address)
South<-unlist(gregexpr(" s ",datacrime$Address))
South<-as.numeric(gsub(-1,0,South))
datacrime$Address<-gsub(" s "," ",datacrime$Address)
datacrime$Address[South==TRUE]<-paste(datacrime$Address[South==TRUE],"s")


North<-unlist(gregexpr(" n ",datacrime$Address))
North<-as.numeric(gsub(-1,0,North))
datacrime$Address<-gsub(" n "," ",datacrime$Address)
datacrime$Address[North==TRUE]<-paste(datacrime$Address[North==TRUE],"n")

East<-unlist(gregexpr(" e ",datacrime$Address))
East<-as.numeric(gsub(-1,0,East))
datacrime$Address<-gsub(" e "," ",datacrime$Address)
datacrime$Address[East==TRUE]<-paste(datacrime$Address[East==TRUE],"e")

West<-unlist(gregexpr(" w ",datacrime$Address))
West<-as.numeric(gsub(-1,0,West))
datacrime$Address<-gsub(" w "," ",datacrime$Address)
datacrime$Address[West==TRUE]<-paste(datacrime$Address[West==TRUE],"w")







datamerged<-merge(datanew,datacrime,by.x="block.address",by.y="Address"
                  ,all.x=TRUE
                  #,all.y=TRUE
)
datamerged$Aggravated.assault[is.na(datamerged$Aggravated.assault)]<-0
datamerged$Arson[is.na(datamerged$Arson)]<-0
datamerged$Burglary[is.na(datamerged$Burglary)]<-0
datamerged$Larceny[is.na(datamerged$Larceny)]<-0
datamerged$Murder[is.na(datamerged$Murder)]<-0
datamerged$Robbery[is.na(datamerged$Robbery)]<-0
datamerged$Vehicle.theft[is.na(datamerged$Vehicle.theft)]<-0
datamerged$Total[is.na(datamerged$Total)]<-0
#save(datamerged,file="datamerged.Rdata")

missdata_parcel<-datamerged[is.na(datamerged$Sec_Block),]
missdata_crime<-datamerged[datamerged$Total==0,]



df2<-datamerged



####merge on block level
#adding latitude and longitude to the block level addresses
#Obtaining the corresponding latitude and longitude for the block level addresses of Group A & B merged dataset
#or just load the dataset if already done that

load("datamerged_coordinate.Rdata")

# for(i in 1:nrow(datamerged))
# {lonlat_sample[i] <- as.numeric(geocode(datamerged$Block.Address[i]))  #row containing the addresses. Note: Ensure the addresses are of the format: "xxx,Syracuse,NY". Else similar longitude latitude of different countries may get assigned
# }
# datamerged_coordinate<-data.frame(datamerged,Long=lonlat_sample[1],Lat=lonlat_sample[2])
# save(datamerged_coordinate,file="datamerged_coordinate.Rdata")


#calculating distance between two address points
library("spatstat")

###################################################################################################################

#loading the above obtained dataset containing the block level addresses(without duplicates), along with their corresponding latitude and longitude values
Data_AB <- datamerged_coordinate #Note: for consistency, the latitude and longitude have been switched. i.e. Latitude is in column 2 now and longitude is in column 3
View(Data_AB)

#loading census data, containg only the latitude and longitude 
Data_C <- df3

#Creating 4 new columns to store the results
Data_C$Lat_AB <- 0
Data_C$Long_AB <- 0
Data_C$Min_len <- 0
Data_C$Address <- " "

str(Data_AB)

#converting factor to character data type
Data_AB$block_Address <- as.character(Data_AB$block_Address)

library(spatstat) #library containg the crossdist function to calculate euclidean distance

for(i in 1:nrow(Data_C))
{
  for(j in 1:nrow(Data_AB))
  {
    dist <- crossdist(Data_C$INTPTLAT[i],Data_C$INTPTLON[i],Data_AB$LAT[j],Data_AB$LONG[j]) #function to calculate euclidean distance between latitude and longitude of Group C and lat, long of Group AB
    
    if(j==1 || dist < min_len)
    {
      min_len <- dist
      Data_C$Lat_AB[i] <- Data_AB$LAT[j] #assiging latitude of GroupAB whose distance from Group C latitude is minimum
      Data_C$Long_AB[i,4] <- Data_AB$LONG[j] #assiging longitude of GroupAB whose distance from Group C longitude is minimum
      Data_C$Min_len[i,5] <- min_len
      Data_C$Address[i,6] <- Data_AB$block_Address #assiging block address of GroupAB for the corresponding lat,long
    }
    
  }
}

df1<-Data_C



## end your R code and logic 


####################################
##### write output file ############
write.csv(df1, file = 'merged_block.csv')
write.csv(df2, file = 'merged_parcel.csv')
####################################






