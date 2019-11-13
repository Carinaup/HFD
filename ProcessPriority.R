############################## Write Priority Data and Join with Full Data ################################
library(RSQLite)
library(sqldf)
library(stringr)
library(tidyverse)
library(ggplot2)
library("readxl")
setwd("~/Documents/graduate/Classes/D2k/Data/CAD")
dcon <- dbConnect(SQLite(), dbname = "CAD_data.sqlite")

############################### Join with the new over_event ##########################################
# query <- "DROP TABLE priority"
# dbSendQuery(dcon, query)
# table = read_excel("Transport Priority 2017-2019.xlsx")
# dbWriteTable(conn = dcon, name = "priority", value = table,
#             row.names = FALSE, header = TRUE)
res <- dbSendQuery(conn = dcon, "
                   SELECT *
                   FROM priority;
                   ")
pri <- dbFetch(res, -1)
dbClearResult(res)
head(pri)

res <- dbSendQuery(conn = dcon, "
                   SELECT *
                   FROM over_event;
                   ")
ov <- dbFetch(res, -1)
dbClearResult(res)
head(ov)

names(pri) = c("EventNum", "IncTime", "UnitCallSign", "TransportPriority", "DestHosp", "HospCpde")
head(pri)
priority = left_join(pri, ov, by = "EventNum")
head(priority)

########################################## One Hot Encoding ########################################
priority$PriorityOne = 0
priority$PriorityOne[which(priority$TransportPriority == 'Priority One')] = 1
priority$PriorityTwo= 0
priority$PriorityTwo[which(priority$TransportPriority == 'Priority Two')] = 1
priority$PriorityThree= 0
priority$PriorityThree[which(priority$TransportPriority == 'Priority Three')] = 1
summary(priority$TransportPriority)
head(priority)
length(priority$EventNum)
sum(priority$PriorityThree) + sum(priority$PriorityTwo) + sum(priority$PriorityOne)
priority$TransportPriority[which(sum(priority[,9:11])==0)] ## There're some NA
## Set the priority one as high priority
priority$HighPriority = 0
priority$HighPriority[which(priority$TransportPriority == 'Priority One')] = 1
head(priority)
## Write into sqlite
dbWriteTable(conn = dcon, name = "OneHotPriority", value = priority,
             row.names = FALSE, header = TRUE)
write.csv(priority,file="OneHotPriority.csv",col.names = T)

