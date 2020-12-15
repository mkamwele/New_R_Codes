#mutate is used to generate new variable
#ifelse takes three variables
# a-condition
#b-condition if met
#c-condition not met
#nested if function
#rurla-r,urban-u,peri-urban
Name<-c("Shelmith","Steve","Mike","Kyalo","Victor","Dennis","Tabitha","Betty","John","Cindy","Mokundi","Kimberly","Edward","Juliet")
County<-c("Nyeri","Homabay","Nyeri","Makueni","Kampala","Meru","Kiambu","Vihiga","Kiambu","Machakos","Nakuru","Makueni","Kiambu","Kericho")
Gender<-c("Female","Male","Male","Male","Male","Male","Female","Female","Male","Female","Male","Female","Male","Female")
Salary<-c(10000,27000,15000,82000,64500,75000,37000,99999,50000,70000,99000,80000,10000,20000)

Payroll<-data.frame(Name, County, Gender, Salary)
Payroll
library(dplyr)

Payroll<-Payroll%>%
  mutate(Jinsia=ifelse(Gender=="Female","Mwanamke","Mwanamme"))
# create variable if salary is less than 50K the value is low or else high
payroll<-Payroll%>% 
  mutate(status=ifelse(Salary<50000,"Low","High"))
#generate new variable female_low it should contain yes or no where  yes-gender is female and low everything else should be no
payroll<-payroll%>%
  mutate(femaleLow=ifelse(Gender=="Female"&status=="Low","yes","no"))
