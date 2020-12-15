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
#calc. distance matrix from your scaled matrix
RFM_dist <- dist(RFM_scaled)
#Find cluster
RFM_clust <-hclust(RFM_dist)
#Plot
plot(RFM_clust)

