#Groupby and summarise
Name<-c("Shelmith","Steve","Mike","Kyalo","Victor","Dennis","Tabitha","Betty","John","Cindy","Mokundi","Kimberly","Edward","Juliet")
County<-c("Nyeri","Homabay","Nyeri","Makueni","Kampala","Meru","Kiambu","Vihiga","Kiambu","Machakos","Nakuru","Makueni","Kiambu","Kericho")
Gender<-c("Female","Male","Male","Male","Male","Male","Female","Female","Male","Female","Male","Female","Male","Female")
Salary<-c(10000,27000,15000,82000,64500,75000,37000,99999,50000,70000,99000,80000,10000,20000)
Age<-c(30,25,42,16,32,19,31,27,58,24,12,100,22,17)
YOE<-c(7,5,3,10,2,7,3,1,1,8,4,3,7,9)
Height <-c(141,157,160,169,158,142,153,156,159,119,117,122,141,137) 
ourclass<-data.frame(Name, County, Gender, Salary,Age,YOE,Height)
ourclass
library(dplyr)
#group gender and find their mean salaries
payroll3<- payroll%>%
  group_by(Gender)%>%
  summarise(mean_income=mean(Salary),mean_age=mean(age))
#mean income,mean years of experience, mean height per county
ourclass1<-ourclass%>%
  group_by(County)%>%
  summarise(mean_income=mean(Salary),mean_years=mean(YOE),mean_height=mean(Height),mean_age=mean(Age))
#highest  mean income is vihiga count