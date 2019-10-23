####################################### Connect to SQLite ######################################
library(RSQLite)
library(sqldf)
library(stringr)
library(tidyverse)
setwd("~/Documents/graduate/Classes/D2k/Data/CAD")
dcon <- dbConnect(SQLite(), dbname = "CAD_data.sqlite")
res <- dbSendQuery(conn = dcon, "
                   SELECT *
                   FROM cad_full")
cad <- dbFetch(res, -1)
dbClearResult(res)
###################################### Understand the data #####################################
head(cad)
cad %>% summarise(
  n_unit = n_distinct(cad$UnitNum), ## We have 682 units operating in total
  n_ve = n_distinct(cad$SNum), ## We have 374 vehicles operating in total, so most of them have multiple duties
  n_sta = n_distinct(cad$StationNum), ## We have 97 stations in total
  n_year = n_distinct(cad$Year), ## We have 6 years of data in total
  n_event = n_distinct(cad$EventType), # We have 775 EventType in total
  n_zip = n_distinct(cad$ZipCode), # Now we have 494 unique ZipCode
  n_unitType = n_distinct(cad$UnitType) # Now we have 75 unique Unit types
)

################################### Analyze the Station #####################################
n = n_distinct(cad$EventNum)
ST = data.frame(ST = cad$StationNum, EN = cad$EventNum, stringsAsFactors=F)
head(ST)
ST = sqldf("SELECT ST, COUNT(DISTINCT(EN)) as Total
           FROM ST
           GROUP BY ST
           ORDER BY Total DESC;")
ST$fre = ST$Total/n
for(i in 1:length(ST$ST)){
  ST$cum_fre[i] = sum(ST$fre[1:i])
}
head(ST)
summary(ST$Total) ## So differnt stations have different level of workload, and the range is very large

ST_Num = ggplot(ST, aes(x=ST, y=Total)) +
         geom_point() +
         labs(title = "Number of Events for each Station", 
         x="Station", y="Number of Events")
ST_Num
## Omit the station of 300
ST_Num = ggplot(ST, aes(x=ST, y=Total)) +
         xlim(0,100) +
         geom_point()
ST_Num

ST$ST[which(ST$Total == max(ST$Total))] # Station of 8 deals with the most incidents
ST$ST[which(ST$Total == min(ST$Total))] # Station of 99 deals with the least incidents

ST$ST[which(ST$Total > quantile(ST$Total,0.25) & ST$Total < quantile(ST$Total,0.75))]
length(ST$ST[which(ST$Total > quantile(ST$Total,0.25) & ST$Total < quantile(ST$Total,0.75))])
# There're 47 stations centered around the mean value

################################## Analyzing Duration ###########################################
res <- dbSendQuery(conn = dcon, "
                   SELECT *
                   FROM cad_full;")

cad <- dbFetch(res, -1)
dbClearResult(res)

Du = cad$Duration
summary(Du) ## The range is pretty huge
density(Du)

x <- Du
h<-hist(x, breaks=10, col="blue", xlab="Duration",
        main="Histogram with Normal Curve", xlim = c(0,300000))
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="red", lwd=2)

du_dens_large = ggplot(cad, aes(Duration)) + geom_density(alpha =0.8) +
  xlim(0,3000) +
  labs(title ="Density of Duration",x="Duration",y="Density",fill = " No of Cylinders")
du_dens_large

du_dens_small = ggplot(cad, aes(Duration)) + geom_density(alpha =0.8) +
  xlim(0,1000) +
  labs(title ="Histogram with Normal Curve",x="Duration",y="Density",fill = " No of Cylinders")
du_dens_small

## From the plot we can see that there're two pick values, one is around 0 (include reasons), which means 
## those dispatches are vary fast, the other is around 260, which is between 1st Quantile and Median.

### plot the Duration by Year/ Month/ Day
Du_year = ggplot(cad, aes(Duration)) + geom_density(aes(fill=factor(Year)), alpha =0.3) +
  xlim(0,1000) +
  labs(title ="Duration Density by Year",x="Duration",y="Density",fill = "Year")
Du_year

cad$Year = as.character(cad$Year)
Du_year_box = ggplot(cad, aes(x=Year, y=Duration)) +
              labs(title ="Duration by Year Box Plot",x="Year",y="Duration") +
              geom_boxplot()
Du_year_box ## threshold
## The distribution of duration almost didn't change in the past five years
## 2017 seems like has a lof of extreme values here, 15 is the most centered year

Du_month = ggplot(cad, aes(Duration)) + geom_density(aes(fill=factor(Month)), alpha =0.3) +
  xlim(0,1000) +
  labs(title ="Duration Density by Month",x="Duration",y="Density",fill = "Month")
Du_month

## The distribution of duration almost didn't change a lot in different months

Du_month_box = ggplot(cad, aes(x=Month, y=Du)) +
               labs(title ="Duration by Month Box Plot",x="Year",y="Duration") +
               geom_boxplot()
Du_month_box

## However, we can see that the ranges of duration in different months change a lot
## August and September seem like the months that are the most inefficient
## Maybe because of the number of incidents in those two months is super large

Du_day = ggplot(cad, aes(Duration)) + geom_density(aes(fill=factor(Day)), alpha =0.3) +
  xlim(0,1000) +
  labs(title ="Duration Density plot",x="Duration",y="Density",fill = " No of Cylinders")
Du_day
### week
Du_day_box = ggplot(cad, aes(x=Day, y=Du)) +
  geom_boxplot()
Du_day_box

## Also no much difference by day. However, several days like 8 and 28 have more extreme values than other days

####################### plot the num of incidents by Year/ Month/ Day ############################
## plot the num of incidents by Year (without 2019, because the data is incompleted)
cad$Year = as.numeric(cad$Year)
plot (cad %>%
      group_by(Year) %>%  
      filter(Year != 19) %>%  summarise(
      IncNum_Year = n_distinct(EventNum)))
### the number of incidents is increasing by year

## Plot the num of incidents by month dropping year of 2019
plot (cad %>%
        group_by(Month) %>%  
        filter(Year != 19) %>% summarise(
          IncNum_Month = n_distinct(EventNum)))
### Febrary has less incidents, others are distributed evenly.

plot (cad %>%
        group_by(Day) %>% summarise(
          IncNum_Day = n_distinct(EventNum)))
### Based on that every month has different days, the distribution is even.

#################################### Detect under-dispatch ###################################
Time = data.frame(cad$EventNum,cad$OnSceneTime,cad$EnrouteTime,
                  stringsAsFactors=F)
names(Time) = c("EventNum", "OnSceneTime", "EnrouteTime")
Time$OnScenceTime = strptime(Time$OnSceneTime, format="'%Y%m%d %H:%M:%S'")
Time$EnrouteTime = strptime(Time$EnrouteTime, format="'%Y%m%d %H:%M:%S'")
min_OS = strptime(tapply(Time$OnSceneTime, Time$EventNum, min), format="'%Y%m%d %H:%M:%S'")
max_ER = strptime(tapply(Time$OnSceneTime, Time$EventNum, max), format="'%Y%m%d %H:%M:%S'")
diff = difftime(min_OS, max_ER, units='secs')
head(diff)
summary(as.numeric(diff))

