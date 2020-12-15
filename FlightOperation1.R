FlightsDelay<-read.csv(file ="R training/FlightDelays.csv",header=TRUE)
FlightsDelay
library(lubridate)
library(dplyr)
#lubridate
#df$enrollmentdate<-as.date(df$enrollmentdate)
#ymd-hms
#mdy
#ydm
#dmy
leo<-"28-03-2019 18:36:20" 
str(leo)
leo2<-dmy_hms(leo)
str(leo2)
#extracting day
leo2_day<-day(leo2)
leo2_day
#extracting month
leo2_month<-month(leo2,label =TRUE,abbr = FALSE )
leo2_month
#extracting year
leo2_year<-year(leo2)
leo2_year
#extracting hour
leo2_hour<-hour(leo2)
leo2_hour
#extracting minute
leo2_minutes<-minute(leo2)
leo2_minutes
#extracting second
leo2_seconds<-second(leo2)
leo2_seconds
#extracting week day using wdy
leo2_weekday<-wday(leo2,label=TRUE,abbr =TRUE)
leo2_weekday


#changing date colum to mdy
FlightsDelay$date <- as.character(FlightsDelay$date)
FlightsDelay$date<-mdy(FlightsDelay$date)
str(FlightsDelay)

#find out number of flights for all weekdays
FlightsDelay<-FlightsDelay%>% #Generating weekdays and weekend column
  mutate(summaryOfDays=ifelse(dayweek==1,"Weekday",
                              ifelse(dayweek==2,"Weekday",
                                     ifelse(dayweek==3,"Weekday",
                                            ifelse(dayweek==4,"Weekday",
                                                   ifelse(dayweek==5,"Weekday","Weekend"))))))

FlightsDelay1<-FlightsDelay %>%
  filter(delay=="delayed"&summaryOfDays=="weekday")

FlightsDelay2<-FlightsDelay %>% filter(delay=="delayed"&weekdays(date)!="Saturday"&weekdays(date)!="Sunday")
nrow(Flightsdelay2)
#find average distance,total distance and count of delayed flights on friday
Flightsdelay3<-FlightsDelay %>% 
  filter(weekdays(date)=="Friday"&delay=="delayed")
nrow(Flightsdelay3)