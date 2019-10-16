#######################Combine into two large data sets############################
library(RSQLite)
setwd("~/Documents/graduate/Classes/D2k/Data/CAD")
dcon <- dbConnect(SQLite(), dbname = "CAD_data.sqlite")

############Don't need to run again(Collect the time stamp data)#############
dbWriteTable(conn = dcon, name = "Full_CAD_1", value = "-",
             row.names = FALSE, header = FALSE)
dbWriteTable(conn = dcon, name = "Full_CAD_2", value = "- 2",
             row.names = FALSE, header = FALSE)
dbWriteTable(conn = dcon, name = "Full_CAD_3", value = "- 3",
             row.names = FALSE, header = FALSE)
dbWriteTable(conn = dcon, name = "Full_CAD_4", value = "- 4",
             row.names = FALSE, header = FALSE)
dbWriteTable(conn = dcon, name = "Full_CAD_5", value = "- 5",
             row.names = FALSE, header = FALSE)
dbWriteTable(conn = dcon, name = "Full_CAD_6", value = "inc_unit_140101_140630.csv",
             row.names = FALSE, header = FALSE)
dbWriteTable(conn = dcon, name = "Full_CAD_7", value = "inc_unit_140701_141231.csv",
             row.names = FALSE, header = FALSE)
dbWriteTable(conn = dcon, name = "Full_CAD_8", value = "inc_unit_150101_150630.csv",
             row.names = FALSE, header = FALSE)
dbWriteTable(conn = dcon, name = "Full_CAD_9", value = "inc_unit_150701_151231.csv",
             row.names = FALSE, header = FALSE)
dbWriteTable(conn = dcon, name = "Full_CAD_10", value = "inc_unit_160101_160630.csv",
             row.names = FALSE, header = FALSE)
dbWriteTable(conn = dcon, name = "Full_CAD_11", value = "inc_unit_160701_161231.csv",
             row.names = FALSE, header = FALSE)
dbWriteTable(conn = dcon, name = "Full_CAD_12", value = "inc_unit_170101_170630.csv",
             row.names = FALSE, header = FALSE)
##################### SQL code for cad_time data set. Don't need to run again ###################

res <- dbSendQuery(conn = dcon, "
                   SELECT *
                   FROM (
                   SELECT * FROM Full_CAD_1 
                   UNION ALL 
                   SELECT * FROM Full_CAD_2
                   UNION ALL 
                   SELECT * FROM Full_CAD_3
                   UNION ALL 
                   SELECT * FROM Full_CAD_4
                   UNION ALL 
                   SELECT * FROM Full_CAD_5
                   UNION ALL 
                   SELECT * FROM Full_CAD_6
                   UNION ALL 
                   SELECT * FROM Full_CAD_7
                   UNION ALL 
                   SELECT * FROM Full_CAD_8
                   UNION ALL 
                   SELECT * FROM Full_CAD_9
                   UNION ALL 
                   SELECT * FROM Full_CAD_10
                   UNION ALL 
                   SELECT * FROM Full_CAD_11
                   UNION ALL 
                   SELECT * FROM Full_CAD_12
                   );")

cad_time <- dbFetch(res, -1)
dbWriteTable(conn = dcon, name = "cad_time", cad_time,
             append = TRUE, row.names = FALSE)
#head(cad_time)
dbClearResult(res)

####################Don't need to run again(Collect the location data)######################
#Change' to "
library(stringr)
table1 = read.csv("- 6")
str_replace(table1, "'", "''");
dbWriteTable(conn = dcon, name = "CAD_full_1", value = table1,
             row.names = FALSE, header = FALSE)
table1 = read.csv("- 7")
str_replace(table1, "'", "''")
dbWriteTable(conn = dcon, name = "CAD_full_2", value = table1,
             row.names = FALSE, header = FALSE)
table1 = read.csv("- 8")
str_replace(table1, "'", "''")
dbWriteTable(conn = dcon, name = "CAD_full_3", value = table1,
             row.names = FALSE, header = FALSE)
table1 = read.csv("- 9")
str_replace(table1, "'", "''")
dbWriteTable(conn = dcon, name = "CAD_full_4", value = table1,
             row.names = FALSE, header = FALSE)
table1 = read.csv("c:franknHFD_Rice_Researchincident_120701_121231.csv")
str_replace(table1, "'", "''")
dbWriteTable(conn = dcon, name = "CAD_full_5", value = table1,
             row.names = FALSE, header = FALSE)
table1 = read.csv("c:franknHFD_Rice_Researchincident_130101_130630.csv")
str_replace(table1, "'", "''")
dbWriteTable(conn = dcon, name = "CAD_full_6", value = table1,
             row.names = FALSE, header = FALSE)
table1 = read.csv("c:franknHFD_Rice_Researchincident_130701_131231.csv")
str_replace(table1, "'", "''")
dbWriteTable(conn = dcon, name = "CAD_full_7", value = table1,
             row.names = FALSE, header = FALSE)
table1 = read.csv("c:franknHFD_Rice_Researchincident_140101_140630.csv")
str_replace(table1, "'", "''")
dbWriteTable(conn = dcon, name = "CAD_full_8", value = table1,
             row.names = FALSE, header = FALSE)
table1 = read.csv("c:franknHFD_Rice_Researchincident_140701_141231.csv")
str_replace(table1, "'", "''")
dbWriteTable(conn = dcon, name = "CAD_full_9", value = table1,
             row.names = FALSE, header = FALSE)
table1 = read.csv("c:franknHFD_Rice_Researchincident_150101_150630.csv")
str_replace(table1, "'", "''")
dbWriteTable(conn = dcon, name = "CAD_full_10", value = table1,
             row.names = FALSE, header = FALSE)
table1 = read.csv("c:franknHFD_Rice_Researchincident_150701_151231.csv")
str_replace(table1, "'", "''")
dbWriteTable(conn = dcon, name = "CAD_full_11", value = table1,
             row.names = FALSE, header = FALSE)
table1 = read.csv("c:franknHFD_Rice_Researchincident_160101_160630.csv")
str_replace(table1, "'", "''")
dbWriteTable(conn = dcon, name = "CAD_full_12", value = table1,
             row.names = FALSE, header = FALSE)
table1 = read.csv("c:franknHFD_Rice_Researchincident_160701_161231.csv")
str_replace(table1, "'", "''")
dbWriteTable(conn = dcon, name = "CAD_full_13", value = table1,
             row.names = FALSE, header = FALSE)
table1 = read.csv("c:franknHFD_Rice_Researchincident_170101_170630.csv")
str_replace(table1, "'", "''")
dbWriteTable(conn = dcon, name = "CAD_full_14", value = table1,
             row.names = FALSE, header = FALSE)
table1 = read.csv("c:franknHFD_Rice_Researchincident_170701_171231.csv")
str_replace(table1, "'", "''")
dbWriteTable(conn = dcon, name = "CAD_full_15", value = table1,
             row.names = FALSE, header = FALSE)

###################### Write into SOLite File. Don't need to run ##########################
res <- dbSendQuery(conn = dcon, "
                   SELECT *
                   FROM (
                   SELECT * FROM CAD_full_1
                   UNION ALL 
                   SELECT * FROM CAD_full_2
                   UNION ALL 
                   SELECT * FROM CAD_full_3
                   UNION ALL 
                   SELECT * FROM CAD_full_4
                   UNION ALL 
                   SELECT * FROM CAD_full_5
                   UNION ALL 
                   SELECT * FROM CAD_full_6
                   UNION ALL 
                   SELECT * FROM CAD_full_7
                   UNION ALL 
                   SELECT * FROM CAD_full_8
                   UNION ALL 
                   SELECT * FROM CAD_full_9
                   UNION ALL 
                   SELECT * FROM CAD_full_10
                   UNION ALL 
                   SELECT * FROM CAD_full_11
                   UNION ALL 
                   SELECT * FROM CAD_full_12
                   UNION ALL 
                   SELECT * FROM CAD_full_13
                   UNION ALL 
                   SELECT * FROM CAD_full_14
                   UNION ALL 
                   SELECT * FROM CAD_full_15
                   );")

cad_loc <- dbFetch(res, -1)
#head(cad_loc)
dbClearResult(res)
dbWriteTable(conn = dcon, name = "cad_loc", cad_loc,
             append = TRUE, row.names = FALSE)
query <- "DROP TABLE CAD_full_1"
dbSendQuery(dcon, query)
query <- "DROP TABLE CAD_full_2"
dbSendQuery(dcon, query)
query <- "DROP TABLE CAD_full_3"
dbSendQuery(dcon, query)
query <- "DROP TABLE CAD_full_4"
dbSendQuery(dcon, query)
query <- "DROP TABLE CAD_full_5"
dbSendQuery(dcon, query)
query <- "DROP TABLE CAD_full_6"
dbSendQuery(dcon, query)
query <- "DROP TABLE CAD_full_7"
dbSendQuery(dcon, query)
query <- "DROP TABLE CAD_full_8"
dbSendQuery(dcon, query)
query <- "DROP TABLE CAD_full_9"
dbSendQuery(dcon, query)
query <- "DROP TABLE CAD_full_10"
dbSendQuery(dcon, query)
query <- "DROP TABLE CAD_full_11"
dbSendQuery(dcon, query)
query <- "DROP TABLE CAD_full_12"
dbSendQuery(dcon, query)
query <- "DROP TABLE CAD_full_13"
dbSendQuery(dcon, query)
query <- "DROP TABLE CAD_full_14"
dbSendQuery(dcon, query)
query <- "DROP TABLE CAD_full_15"
dbSendQuery(dcon, query)
query <- "DROP TABLE Full_CAD_1"
dbSendQuery(dcon, query)
query <- "DROP TABLE Full_CAD_2"
dbSendQuery(dcon, query)
query <- "DROP TABLE Full_CAD_3"
dbSendQuery(dcon, query)
query <- "DROP TABLE Full_CAD_4"
dbSendQuery(dcon, query)
query <- "DROP TABLE Full_CAD_5"
dbSendQuery(dcon, query)
query <- "DROP TABLE Full_CAD_6"
dbSendQuery(dcon, query)
query <- "DROP TABLE Full_CAD_7"
dbSendQuery(dcon, query)
query <- "DROP TABLE Full_CAD_8"
dbSendQuery(dcon, query)
query <- "DROP TABLE Full_CAD_9"
dbSendQuery(dcon, query)
query <- "DROP TABLE Full_CAD_10"
dbSendQuery(dcon, query)
query <- "DROP TABLE Full_CAD_11"
dbSendQuery(dcon, query)
query <- "DROP TABLE Full_CAD_12"
dbSendQuery(dcon, query)
dbDisconnect(dcon)

################################## Dealing with cad_time data set #############################
library(tidyverse)
head(cad_time)
# Calculate the duration of each each dispatch
cad_time$Duration = difftime(strptime(cad_time$V6, "'%Y%m%d %H:%M:%S'"),
                             strptime(cad_time$V5, "'%Y%m%d %H:%M:%S'"),units='secs')
cad_time$Duration = abs(cad_time$Duration)
head(cad_time)
names(cad_time) = c("EventNum", "UnitNum", "StationNum", "SNum", "EnrouteTime", "OnSceneTime", "Duration")
head(cad_time)
str(cad_time$Duration)
cad_time$Duration = as.numeric(as.character(cad_time$Duration))
summary(cad_time$Duration)

## There's Na's, so check them
cad_time[is.na(cad_time$Duration),]
## We can see that those records of incidents are missing, so we just drop these data to do next step
cad_time = na.omit(cad_time)

################################## Dealing with cad_loc data set #############################
head(cad_loc)
names(cad_loc) = c("EventNum", "EventType", "Location", "ZipCode", "Longitude", "Latitude")
# cad_loc$Location = paste(cad_loc$Location,cad_loc$ZipCode,collapse=",")
# head(cad_loc$Location)
summary(cad_loc)
cad_loc$EventNum = as.numeric(cad_loc$EventNum)
summary(cad_loc)
## There're Na produced for EventNum, so just drop them
cad_loc = na.omit(cad_loc)
summary(cad_loc)

######################### Combine time data and location data #####################################
cad = merge(cad_time, cad_loc, by = "EventNum", all.x = TRUE, all.y = TRUE)
cad = left_join(cad_time, cad_loc, by = "EventNum")
head(cad)
# Add more columns for future analysis
cad$Year = as.numeric(str_sub(cad$EventNum, 1, 2))
cad$Month = str_sub(cad$EventNum, 3, 4)
cad$Day = str_sub(cad$EventNum, 5, 6)
summary(cad$EventType)
head(cad)
is.na(cad) # There're 105 Na
cad = na.omit(cad)
cad[is.na(cad),]
head(cad)
# Extract the unitType from unitNum
loc = str_locate(cad$UnitNum, "[0-9]")
cad$UnitType <- str_sub(cad$UnitNum, end = loc[,"start"]-1)
# export cad data
# write.csv(cad,file="cad.csv",col.names = T)
dbWriteTable(conn = dcon, name = "cad_full", cad,
             append = TRUE, row.names = FALSE)

################################### Group the EventType #####################################
library(dplyr)
library(sqldf)
ET = str_sub(cad$EventType, 1, 4) 
n = n_distinct(cad$EventNum)
ET = data.frame(ET, EventNum = cad$EventNum, stringsAsFactors=F)
head(ET)
ET = sqldf("SELECT ET, COUNT(DISTINCT(EventNum)) as Total
           FROM ET
           GROUP BY ET
           ORDER BY Total DESC;")
ET$fre = ET$Total/n
for(i in 1:length(ET$ET)){
  ET$cum_fre[i] = sum(ET$fre[1:i])
}
head(ET)
write.csv(ET,file="ET.csv",col.names = T)

## Follow-up:
### For location data, compute the distance of each dispatch
### Can do some analysis with the priority
### Pick some stations with high frequency to do the detailed analysis
### Pick some eventType with high frequency to do the detailed analysis


