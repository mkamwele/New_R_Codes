library(ggplot2)
library(dplyr)
library(lubridate)
Audit <- read.csv(file = "R training/Audit.csv")
Audit
#has arguments
#1-data
#2-aesthetics(variable in x and y axis)
#
ggplot(data=Audit,aes(x=Gender,y=count))
geom_bar(stat = "identity",fill="cc163d")
labs(title="my first graph",x="Gender",y="count")
theme_bw()
#generating a table that counts gender
Gender_count <- Audit %>% 
  group_by(Gender) %>% 
  summarise(count=n())
#plotting a bar graph
Gender_plot <- ggplot(data=Gender_count,
                      aes(x=Gender,y=count))+
  geom_bar(stat = "identity",fill="#cc163d")+
  labs(title="Distribution of Gender",x="Gender",y="count") +
  theme(plot.title =element_text(hjust = 0.5),
        axis.text.x=element_text(angle=45,vjust=0.5) )
Gender_plot
#employment,education,marital,account
#count of marital
#generating a table that counts Marital
Marital_count <- Audit %>% 
  group_by(Marital) %>% 
  summarise(count=n())
Marital_count
#plotting a bar graph
Marital_plot <- ggplot(data=Marital_count,
                       aes(x=Marital,y=count))+
  geom_bar(stat="identity",fill="green")+
  labs(title = "Distribution of Marital",x="Marital",y="count")+
  theme_classic()+
  theme(plot.title = element_text(hjust=0.5),
        axis.text.x = element_text(angle=45,vjust=0.5))
Marital_plot
#count of education
Education_count<-Audit %>%
  group_by(Education) %>% 
  summarise(count=n())
#plot a bar graph
Education_plot <- ggplot(data=Education_count,
                         aes(x=Education,y=count))+
  geom_bar(stat="identity",fill="blue")+
  labs(title = "Distribution of Education",x="Education",y="count")+
  theme_grey()+
  theme(plot.title = element_text(hjust=0.5),
        axis.text.x = element_text(angle=45,vjust=0.5))
Education_plot
#grouped bar graphs
Maritalgender <- Audit %>% 
  group_by(Marital,Gender) %>% 
  summarise(count=n())
Maritalgender_plot <- ggplot(data=Maritalgender,aes(x=Marital,y=count,fill=Gender))+
  geom_bar(stat="identity",position = "dodge")+
  labs(title = "Distribution by marital and gender",x="Marital",y="count")+
  theme_classic()+
  theme(plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values = c("brown","yellow"))
Maritalgender_plot

#Avarage income of Marital status

Maritalincome <- Audit %>% 
  group_by(Marital,Gender) %>% 
  summarise(Avearge=mean(Income))
Maritalincome
Maritalincome_plot <- ggplot(data=Maritalincome,aes(x=Marital,y=count,fill=Gender))+
  geom_bar(stat="identity",position = "stack")+
  labs(title = "Distributionof income by Marital and Gender",x="Marital",y="Average")+
  theme_classic()+
  theme(plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values = c("brown","yellow"))
Maritalincome_plot




###line graph
FlightDelay<-read.csv(file ="R training/FlightDelays1.csv",header=TRUE)
FlightDelay
#changing date colum to mdy
FlightDelay$date <- as.character(FlightDelay$date)
FlightDelay$date<-mdy(FlightDelay$date)
str(FlightDelay)


FlightDelay_count <- FlightDelay %>%
  mutate(wday2=wday(date,label = TRUE)) %>% 
  group_by(wday2) %>% 
  summarise(count=n())


FlightDelay_plot <- ggplot(data=FlightDelay_count,aes(x=wday2,y=count,group=1,color=1))+
  geom_line(color="red")+
  labs(title="Distribution of flights by day",x="Day",y="count")+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5),
        axis.text.x = element_text(angle=45,vjust=0.5))

FlightDelay_plot



#mutate wday from date column
FlightDelay10 <- FlightDelay %>% 
  mutate(wday=wday(date,label=TRUE))
FlightDelay10

#count of weather and date
FlightDelay_count <- FlightDelay10 %>%
  mutate(weather=as.character(weather)) %>% 
  group_by(weather,wday) %>% 
  summarise(count=n()) 


FlightDelay_plot2 <- ggplot(data=FlightDelay_count,aes(x=wday,y=count,group=weather,color=weather))+
  geom_line()+
  labs(title="Distribution of weather by day",x="Day",y="count")+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5),
        axis.text.x = element_text(angle=45,vjust=0.5))+
  scale_colour_manual(values = c("brown","yellow"))

FlightDelay_plot2


