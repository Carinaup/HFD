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
im_ems = ems[,1:15]
im_ems$code4 = ems[,25]
head(im_ems)

########################## Detect Over-Dispatch of Units #############################
im_ems$over_dis = 0
#### 1.Only En_route time, no On_Scene time
im_ems$over_dis[which(im_ems$Dim_Incident___Incident_Unit_En_Route_Date_Time != "NULL" &
                im_ems$Dim_Incident___Incident_Unit_Arrived_On_Scene_Date_Time == "NULL")] = 1
length(which(im_ems$over_dis == 1))
N = length(im_ems$row);N
over_percent = length(which(im_ems$over_dis == 1)) / N ; over_percent ## 0.1107447 of dispatches are over-dispatch

#### 2. Short OnScene duration
im_ems$du_onScene = difftime(strptime(im_ems$Dim_Incident___Incident_Unit_Left_Scene_Date_Time, format="%Y-%m-%d %H:%M:%OS"), 
                             strptime(im_ems$Dim_Incident___Incident_Unit_Arrived_On_Scene_Date_Time, format="%Y-%m-%d %H:%M:%OS"), 
                             units='secs')
im_ems$du_onScene = as.numeric(im_ems$du_onScene)
write.csv(im_ems$du_onScene,file="du_onScene.csv")
summary(im_ems$du_onScene) ## Question: why there's negative number...
duOnScene_l = ggplot(im_ems, aes(du_onScene)) + geom_density(alpha =0.8) +
              xlim(0,2000)+
              labs(title ="Density of Duration for OnScene",x="Duration",y="Density",
              fill = " No of Cylinders")
duOnScene_l

duOnScene_s = ggplot(im_ems, aes(du_onScene)) + geom_density(alpha =0.8) +
  xlim(0,1300)+
  labs(title ="Histogram with Normal Curve",x="Duration of OnScene",y="Density",
       fill = " No of Cylinders")
duOnScene_s

## Find the first local minimum as the threshold
# x = im_ems$du_onScene
# x = na.omit(x)
# y = density(x)$y
# x = density(x)$x
# MinDensity <- min(y[x > 20 & x < 1200])
# x[which(y == MinDensity)]

## Choose the duration around 0 as overdispatch
im_ems$du_onScene[which(density(im_ems$du_onScene[which(im_ems$du_onScene > 20 & im_ems$du_onScene < 250)])) == 
                    min(density(im_ems$du_onScene[which(im_ems$du_onScene > 20 & im_ems$du_onScene < 250)]))]
im_ems$over_dis[which(round(im_ems$du_onScene) == 0)] = 1
over_percent = length(which(im_ems$over_dis == 1)) / N ; over_percent ## over-dispatch increases from 0.110747 to 0.1157747

#### 3. Asign the column of canceled time which is not NULL as over-dispatching
im_ems$over_dis[which(im_ems$Dim_Incident___Incident_Unit_Canceled_Date_Time != "NULL")] = 1
over_percent = length(which(im_ems$over_dis == 1)) / N ; over_percent ## over-dispatch increases from 0.1157747 to 0.2189292
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
# the percentage of over-dispatched evnets is 0.2949549

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
                     x="Event Type", y="Percentage")

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
top_type$code4 = factor(top_type$code4, levels=c('FFAL','FEHG','FFCS','FECA','FFWR')) 
top_type5 = ggplot(top_type, aes(x=code4,y=percent)) +
            geom_col() +
            labs(title = "Top 5 Event Types with the Highest Frequency of Over-Dispatching", 
                 x="Event Type", y="Percentage")


