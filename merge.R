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
datanew$block.address<-paste(datanew$StNum,datanew$StName)#################
datanew$block.address<-gsub("AVE","AV",datanew$block.address)
datanew$block.address<-tolower(datanew$block.address)
datanew$block.address<-gsub("\\s?&.*$","",datanew$block.address)
datanew$block.address<-gsub(" rear.*$","",datanew$block.address)
datanew$block.address<-gsub(" to .*$","",datanew$block.address)
datanew$block.address<-gsub(" #.*$","",datanew$block.address)



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
                 # ,all.y=TRUE
)
datamerged$Aggravated.assault[is.na(datamerged$Aggravated.assault)]<-0
datamerged$Arson[is.na(datamerged$Arson)]<-0
datamerged$Burglary[is.na(datamerged$Burglary)]<-0
datamerged$Larceny[is.na(datamerged$Larceny)]<-0
datamerged$Murder[is.na(datamerged$Murder)]<-0
datamerged$Robbery[is.na(datamerged$Robbery)]<-0
datamerged$Vehicle.theft[is.na(datamerged$Vehicle.theft)]<-0
#save(datamerged,file="datamerged.Rdata")

missdata<-datamerged[is.na(datamerged$Sec_Block),]
sum(datamerged$Total[is.na(datamerged$Sec_Block)])
sum(datamerged$Total)

## end your R code and logic 

####################################
##### write output file ############
write.csv(df1, file = 'merged_block.csv')
write.csv(df2, file = 'merged_parcel.csv')
####################################






