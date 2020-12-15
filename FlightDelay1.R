FlightDelay<-read.csv(file ="R training/FlightDelays1.csv",header=TRUE)
FlightDelay
library(lubridate)
library(dplyr)
#changing date colum to mdy
FlightDelay$date <- as.character(FlightDelay$date)
FlightDelay$date<-mdy(FlightDelay$date)
str(FlightDelay)
#find out number of flights for all weekdays
FlightDelay1<-FlightDelay %>% filter(delay=="delayed"&weekdays(date)!="Saturday"&weekdays(date)!="Sunday")
nrow(FlightDelay1)
#find average distance,total distance and count of delayed flights on friday
FlightDelay2<-FlightDelay %>% 
  filter(weekdays(date)=="Friday"&delay=="delayed") %>% 
  group_by(distance) %>% 
  summarise(total_distance=sum(FlightDelay2$distance),mean_distance=mean(FlightDelay2$distance),count_distance=length(FlightDelay2$distance))
nrow(FlightDelay2)
#find how many flights were on time on weekdays and week ends
FlightDelay3<-FlightDelay %>%
  filter(delay=="ontime") %>% 
  select(dayweek)
nrow(FlightDelay3)
#find out number of filghts for each destinations across all week days
FlightDelay4<-FlightDelay %>%
  group_by(dest)%>% 
  filter(weekdays(date)!="Saturday"&weekdays(date)!="Sunday")
nrow(FlightDelay4) 
#Find out the number of times weather was bad across
#all weekdays.
unique(FlightDelay$weather)
FlightDelay5<-FlightDelay %>%
  filter(weekdays(date)!="Saturday"&weekdays(date)!="Sunday"&weather==1)
nrow(FlightDelay5)