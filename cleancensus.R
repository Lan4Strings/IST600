#####################################
##### Do NOT delete this block #####
setwd(Sys.getenv('PROJECT_HOME'))
####################################

####################################
## write code to read input csv into data frame
df <- read.csv('censusdata.csv')
####################################

## start writing your R code from here
library(data.table)
library(magrittr)
df <- fread(paste0("censusdata.csv"),sep = "\n", header = FALSE)
geo_file <- df
geo <- geo_file[, .(LOGRECNO = as.numeric(substr(V1, 19, 25)),
                    SUMLEV = substr(V1, 9, 11),
                    PLACE = substr(V1, 46, 50),
                    INTPTLAT = as.numeric(substr(V1, 337, 347)),
                    INTPTLON = as.numeric(substr(V1, 348, 359)))]
syr_geo <- geo[PLACE == "73000"]
syr_geo
syr_block <- syr_geo[SUMLEV == "100"]
syr_block
df<-syr_block
## end your R code and logic 

####################################
##### write output file ############
write.csv(df, file = 'census.csv')
####################################



