#Data Analysis on Amazon data set

#1.Get data

amazon <- read.csv("amazon_kenya_data.csv")
library(dplyr)

#2.wrangle the data into an RFM table
RFM <- amazon %>% 
  mutate(sales=QUANTITY.PURCHASED*ITEM.PRICE) %>% #amount spent
  group_by(CUSTOMER.ID) %>% 
  summarise(monetry=sum(sales),#value of customer
            frequency=n(),
            last_purchasedate=max(as.Date(transaction_date)))# last purchased date



RFM <- RFM%>% 
  
  mutate(recency=as.Date("2019-07-01") - last_purchasedate)

RFM <- RFM %>% 
  select(-4)


RFM$recency <- as.numeric(RFM$recency)

#3.normalize the dataset 
#we do this to fit it on a scale
#data<-scale(.data)#for numerical
RFM_scaled <- RFM %>% 
  select(-1) %>% 
  scale()
set.seed(12345)
glimpse(RFM)  
class(RFM_scaled)

#4.convert it into a matrix
RFM_Matrix <- as.matrix(RFM_scaled)
row.names(RFM_Matrix) <- RFM$CUSTOMER.ID

#5.k.means algorithm 
customer_kmeans <- kmeans(RFM_Matrix, centers = 4)
RFM$kcluster <- customer_kmeans$cluster


#results
#each centroids is about 70% success
#convert to factor
RFM$kcluster <- as.factor(RFM$kcluster)
library(ggplot2)
#recency vs monetary fill=cluster
rfm_plot <- ggplot(data=RFM,aes(x=recency,y=monetry,color=kcluster))+
  geom_point()
rfm_plot
#frequency vs recency
library(plotly)
a <- ggplot(data=RFM,aes(x=frequency,y=recency,color=kcluster))+
  geom_point()
ggplotly(a)
a
#frequency vs monetry
b <- ggplot(data=RFM,aes(x=frequency,y=monetry,color=kcluster))+
  geom_point()
ggplotly(b)
b
#correlation test to see the relationship between variables
cor.test(RFM$monetry,RFM$frequency,method = "kendall",alternative = "greater")

#Questions.
#1. For each cluster find the median recency, frequency and monetary.

#2. Which cluster represents the best customers and the lost customers respectively.
RFM <- RFM %>% 
  select(-1)
p <- RFM %>%
  group_by(kcluster) %>% 
  summarise_all(funs(median))
p
#3. Plot recency against monetary with clusters as the fill.
#4. What can you advise the supermarket based on the insights from the analysis above.