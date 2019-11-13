###################### Read and Write imputed EMS data ############################
library(RSQLite)
library(sqldf)
library(stringr)
library(tidyverse)
library(ggplot2)
setwd("~/Documents/graduate/Classes/D2k/Data/CAD")
dcon <- dbConnect(SQLite(), dbname = "CAD_data.sqlite")
# im_ems = read.csv("ems_imputed.csv")
# dbWriteTable(conn = dcon, name = "EMS_imputed", value = im_ems,
#             row.names = FALSE, header = FALSE)
res <- dbSendQuery(conn = dcon, "
                   SELECT *
                   FROM EMS_imputed;
                   ")
ems <- dbFetch(res, -1)
dbClearResult(res)
head(ems)
length(ems)
ori_n = length(ems$row);ori_n
im_ems = ems[,1:15]
im_ems$code4 = ems[,25]
head(im_ems)
## Get rid of the FF code 
im_ems <- filter(im_ems, substr(code4, 1, 2) == "FE")
new_n = length(im_ems$row);new_n
ori_n - new_n ## Deleted 82627 rows

############################### Detect Over-Dispatch of Units ######################################
## Initialize the over-dispatch as 0
im_ems$over_dis = 0

## For each incident, omit the earliest unit, the first unit, because it cannot be an over-dispatch
uni_first = im_ems%>%
  group_by(Dim_Incident_One_To_One___Response_Incident_Number)%>%
  summarise(Min=min(Dim_Incident___Incident_Unit_Arrived_On_Scene_Date_Time))

im_ems = left_join(im_ems, uni_first, by = "Dim_Incident_One_To_One___Response_Incident_Number")

unit_nofirst = sqldf("SELECT * 
                     FROM im_ems
                     WHERE Dim_Incident___Incident_Unit_Arrived_On_Scene_Date_Time > Min")

length(unit_nofirst$row)
head(im_ems)

## Do the following analysis for unit_nofirst
#### 1. Short OnScene duration
unit_nofirst$du_onScene = difftime(strptime(unit_nofirst$Dim_Incident___Incident_Unit_Left_Scene_Date_Time, format="%Y-%m-%d %H:%M:%OS"), 
                                   strptime(unit_nofirst$Dim_Incident___Incident_Unit_Arrived_On_Scene_Date_Time, format="%Y-%m-%d %H:%M:%OS"), 
                                   units='secs')
unit_nofirst$du_onScene = as.numeric(unit_nofirst$du_onScene)
# write.csv(im_ems$du_onScene,file="du_onScene.csv")
summary(unit_nofirst$du_onScene)
nofirst_du_nona = unit_nofirst[-which(is.na(unit_nofirst$du_onScene)),] ## remove Na's, 119936 of Na's
summary(nofirst_du_nona$du_onScene) 
nofirst_du_nona[which(nofirst_du_nona$du_onScene == min(nofirst_du_nona$du_onScene)),]
#### After looking at the rows with negative duration, the left_secene column is very weirred, so remove them

length(nofirst_du_nona[which(nofirst_du_nona$du_onScene <0),1]) ## there're 32 were removed
nofirst_du = nofirst_du_nona[-which(nofirst_du_nona$du_onScene <0),]
duOnScene_l = ggplot(nofirst_du, aes(du_onScene)) + geom_density(alpha =0.8) +
  xlim(0,2000)+
  labs(title ="Density of Duration for OnScene",x="Duration",y="Density")
duOnScene_l

## Find the first local minimum as the threshold
test_vec <- nofirst_du$du_onScene[nofirst_du$du_onScene >= 0 & nofirst_du$du_onScene <= 2000]
test_vec <- na.omit(test_vec)
den_y <- density(test_vec)$y
den_x <- density(test_vec)$x
min_den_y <- min(den_y[den_x > 20 & den_x < 500])
min_x <- den_x[which(den_y == min_den_y)]
min_x
duOnScene_l + geom_vline(xintercept = min_x, lwd = 2, color="red")

## Choose the duration of 107.1393 as threshold
head(im_ems)
id_over = nofirst_du$row[which(nofirst_du$du_onScene <= min_x & nofirst_du$du_onScene >= 0)]
head(id_over)

for (i in id_over){
  im_ems$over_dis[which(im_ems$row==i)] = 1
}

N = length(im_ems$row);N
over_percent = length(which(im_ems$over_dis == 1)) / N ; over_percent ## 0.02364858 of over-dispatching

#### 2.Only En_route time, no On_Scene time
im_ems$over_dis[which(im_ems$Dim_Incident___Incident_Unit_En_Route_Date_Time != "NULL" &
                im_ems$Dim_Incident___Incident_Unit_Arrived_On_Scene_Date_Time == "NULL")] = 1
length(which(im_ems$over_dis == 1))
over_percent = length(which(im_ems$over_dis == 1)) / N ; over_percent ## over-dispatch increases from 0.02364858 to 0.130791

#### 3. Asign the column of canceled time which is not NULL as over-dispatching
im_ems$over_dis[which(im_ems$Dim_Incident___Incident_Unit_Canceled_Date_Time != "NULL")] = 1
over_percent = length(which(im_ems$over_dis == 1)) / N ; over_percent ## over-dispatch increases from 0.130791 to 0.2189485
head(im_ems)
# dbWriteTable(conn = dcon, name = "im_ems", value = im_ems,
#             row.names = FALSE, header = FALSE)
# write.csv(im_ems,file="im_ems.csv")
########################## Detect Over-Dispatch of Events #############################
head(im_ems)
over_event = sqldf("SELECT code4, Dim_Incident_One_To_One___Response_Incident_Number as EventNum, 
                   SUM(over_dis) as over_event
                   FROM im_ems
                   GROUP BY EventNum
                   ORDER BY over_event DESC;")
head(over_event)
over_event$over_event[which(over_event$over_event >= 1)] = 1
N_event = length(over_event$over_event)
over_event_per = length(which(over_event$over_event == 1)) / N_event ; over_event_per 
# the percentage of over-dispatched evnets is 0.2964025
# write.csv(over_event,file="over_event.csv")
# dbWriteTable(conn = dcon, name = "over_event", value = over_event,
#              row.names = FALSE, header = TRUE)
########################## Analysis Over-dispatching by EventType #############################
## EventType that contribute most to the total Over-dispatching
over_type = sqldf("SELECT code4, COUNT(EventNum) as count, SUM(over_event) as countOver
                  FROM over_event
                  GROUP BY code4
                  ORDER BY countOver DESC;")
head(over_type)
N_over = length(which(over_event$over_event == 1))
over_type$percent_all = over_type$countOver/N_over
head(over_type)
top_type_all = over_type[1:5,]
## Plot
top_type_all
top_type_all$code4 = factor(top_type_all$code4, levels=c('FERE','FEMA','FEHT','FEUC','FEUN')) 
top_type5_all = ggplot(top_type_all, aes(x=code4,y=percent_all)) +
                geom_col() +
                labs(title = "Top 5 Contributions to all Over-Dispatching", 
                     x="Event Type", y="Percentage") +
                theme(title=element_text(size=8))
top_type5_all
## EventType that has the most percentage of Over-dispatching
over_type$percent = over_type$countOver/ over_type$count
head(over_type)
summary(over_type$count)
over_type_percent = sqldf("SELECT code4, count, countOver, percent_all, percent
                           FROM over_type
                           WHERE count BETWEEN 27 and 4745
                           ORDER BY percent DESC;")
head(over_type_percent)
top_type = over_type_percent[1:5,]
top_type$code4 = factor(top_type$code4, levels=c('FEHG','FECA','FEDR','FESH','FEET')) 
top_type5 = ggplot(top_type, aes(x=code4,y=percent)) +
            geom_col() +
            labs(title = "Top 5 Event Types with \n the Highest Frequency of Over-Dispatching", 
                 x="Event Type", y="Percentage") +
            theme(title=element_text(size=8))
top_type5

### with logitude, latitude and the first time stamp
res <- dbSendQuery(conn = dcon, "
                   SELECT *
                   FROM cad_loc;
                   ")
cad <- dbFetch(res, -1)
dbClearResult(res)
head(cad)
cad = sqldf("SELECT EventNum, Longitude, Latitude
            FROM cad
            ;")
head(cad)

res <- dbSendQuery(conn = dcon, "
                   SELECT *
                   FROM over_event;
                   ")
over_event <- dbFetch(res, -1)
dbClearResult(res)
head(over_event)

over_event = left_join(over_event, cad, by = "EventNum")
head(over_event)

res <- dbSendQuery(conn = dcon, "
                   SELECT *
                   FROM EMS_imputed;
                   ")
ems <- dbFetch(res, -1)
dbClearResult(res)
head(ems)
NotiTime = ems%>%
  group_by(Dim_Incident_One_To_One___Response_Incident_Number)%>%
  summarise(Min=min(Dim_Incident___Incident_Dispatch_Notified_Date_Time))
head(NotiTime)
names(NotiTime) = c('EventNum', 'NotiTime')
over_event = left_join(over_event, NotiTime, by = "EventNum")
head(over_event)
view(over_event)
# write.csv(over_event,file="over_event.csv")
