###################################### Read the Data with Markerd Over-Dispatch ###############################
library(RSQLite)
library(sqldf)
library(stringr)
library(tidyverse)
library(ggplot2)
library("readxl")
setwd("~/Documents/graduate/Classes/D2k/Data/CAD")

dcon <- dbConnect(SQLite(), dbname = "CAD_data.sqlite")
res <- dbSendQuery(conn = dcon, "
                   SELECT *
                   FROM im_ems;
                   ")
ou <- dbFetch(res, -1)
dbClearResult(res)
head(ou)

## Define the cost for over-dispatching as the wasted time on travel ( to money cost )
#### But maybe all the time this unit wasted on this event is the all time cost??
ou$cost = 0
ou$travel = 0
ou$du_back = 0
a = which(ou$over_dis == 1 &
          ou$Dim_Incident___Incident_Unit_En_Route_Date_Time != "NULL" &
          ou$Dim_Incident___Incident_Unit_Arrived_On_Scene_Date_Time != "NULL" &
          ou$Dim_Incident___Incident_Unit_Back_In_Service_Date_Time != "NULL" &
          ou$Dim_Incident___Incident_Unit_Left_Scene_Date_Time != "NULL")
## compute the travel time for each unit who had arrived the scene
ou$travel[a] = difftime(strptime(ou$Dim_Incident___Incident_Unit_Arrived_On_Scene_Date_Time, format="%Y-%m-%d %H:%M:%OS"), 
                     strptime(ou$Dim_Incident___Incident_Unit_En_Route_Date_Time, format="%Y-%m-%d %H:%M:%OS"), 
                     units='secs')
ou$travel = as.numeric(ou$travel)
ou[which(ou$travel < 0),]
ou$du_back[a] = difftime(strptime(ou$Dim_Incident___Incident_Unit_Back_In_Service_Date_Time, format="%Y-%m-%d %H:%M:%OS"), 
                      strptime(ou$Dim_Incident___Incident_Unit_Left_Scene_Date_Time, format="%Y-%m-%d %H:%M:%OS"), 
                      units='secs')
ou$du_back = abs(as.numeric(ou$du_back))

ou$cost = ou$travel + ou$du_back
head(ou)

a = which(ou$over_dis == 1 &
          ou$Dim_Incident___Incident_Unit_En_Route_Date_Time != "NULL" & 
          ou$Dim_Incident___Incident_Unit_Arrived_On_Scene_Date_Time != "NULL" &
          ou$Dim_Incident___Incident_Unit_Back_In_Service_Date_Time != "NULL"&
          ou$Dim_Incident___Incident_Unit_Left_Scene_Date_Time != "NULL")

## compute the time they didn't get on scene
ou[which(ou$Dim_Incident___Incident_Unit_En_Route_Date_Time != "NULL" & 
                ou$Dim_Incident___Incident_Unit_Arrived_On_Scene_Date_Time == "NULL"),]
#### After looking at the data, all the units have the EnRoute time without a OnScene time
#### seem like have the Canceled time, so I compute the difference and double it as the cost
a = which(ou$over_dis == 1 & ou$Dim_Incident___Incident_Unit_En_Route_Date_Time != "NULL" & 
           ou$Dim_Incident___Incident_Unit_Canceled_Date_Time != "NULL")
ou$travel[a] =
  difftime(strptime(ou$Dim_Incident___Incident_Unit_Canceled_Date_Time, format="%Y-%m-%d %H:%M:%OS"), 
           strptime(ou$Dim_Incident___Incident_Unit_En_Route_Date_Time, format="%Y-%m-%d %H:%M:%OS"), 
           units='secs')
ou$travel = as.numeric(ou$travel)
ou$du_back = ou$travel
ou[a,]
