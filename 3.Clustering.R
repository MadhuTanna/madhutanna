library(dplyr)

#setwd("C:/Users/mtanna108360/Downloads/Downloads/Data Science/Proschool/Term 3 Recordings/Project")
setwd("C:/Users/MADHU/Downloads/Data Science/Term 3 Project")
custdata=read.csv("term3.csv")
head(custdata)
glimpse(custdata) ## like str
summary(custdata)

cust_data_table = tbl_df(custdata)
cust_data_table = na.omit(cust_data_table)
cust_data_table = cust_data_table[,-1]
new_cust_table = cust_data_table[cust_data_table$Reached.on.Time_Y.N == 1,]
new_cust_table$Warehouse_block = as.numeric(new_cust_table$Warehouse_block)
new_cust_table$Mode_of_Shipment = as.numeric(new_cust_table$Mode_of_Shipment)
new_cust_table$Product_importance = as.numeric(new_cust_table$Product_importance)
new_cust_table$Gender = as.numeric(new_cust_table$Gender)
head(new_cust_table)
str(new_cust_table)

scaledata = scale(new_cust_table[,-11])
head(scaledata)

finddist = dist(scaledata,method = "euclidean")

#prepare clusters using hclust
clus = hclust(finddist,method = "complete")
clus

#plot clusters
plot(clus)

# rectangular cluster = dendogram
rect.hclust(clus,k=3,border = 1:3)

# list or cut the clusters
cuttree = cutree(clus,k=3)
table(cuttree)
mydata = cbind.data.frame(new_cust_table,cuttree)
mydata = mydata[order(cuttree),]
head(mydata)

cor = seq(1,11,1)
#Below plot shows some variables are heavily
for (i in seq(1,11)){
  cor[i]=cor(mydata[i],cuttree)
}

cor
sort(cor)
plot(mydata,color =factor(cuttree))

#Customer_care_calls,Cost_of_the_Product,Prior_purchases,Weight_in_gms, Reached in Time

library(ggplot2)
#Group 2&3 mainly belogns to loyal and high value customer. Customer rataing and prior purchse impacts
ggplot(mydata,aes(Customer_rating,Prior_purchases,color =factor(cuttree))) + geom_point()

#Group 1 has mostly light weight and those who is calling frequently.
ggplot(mydata,aes(Customer_care_calls,Weight_in_gms,color =factor(cuttree))) + geom_point()

# KMEANS method
# find K : find within sum of squre

set.seed(2)
wss=seq(1,100)

for (i in 1:100)
{
  wss[i] = sum(kmeans(scaledata,centers = i)$withinss)
}
plot(wss,type="b")

# K=3 Find cluster

finalculster = kmeans(scaledata,centers = 3)
finalculster
clust = finalculster$cluster
table(finalculster$cluster)
mydata1=cbind.data.frame(new_cust_table,clust)
mydata1 = mydata1[order(clust),]

#Group 1&2 has very high impact from Discount offered and cost of product
ggplot(mydata1,aes(Cost_of_the_Product,Discount_offered,Weight_in_gms,warehouse_block,color =factor(cuttree))) + geom_point()


ggplot(mydata1,aes(Cost_of_the_Product,color =factor(cuttree),group = interaction(cuttree))) + geom_line(stat='count') 
ggplot(mydata1,aes(Weight_in_gms,color =factor(cuttree),group = interaction(cuttree))) + geom_line(stat='count') 
ggplot(mydata1,aes(Customer_rating,color =factor(cuttree),group = interaction(cuttree))) + geom_line(stat='count') 
ggplot(mydata1,aes(Product_importance,color =factor(cuttree),group = interaction(cuttree))) + geom_line(stat='count') 
ggplot(mydata1,aes(Warehouse_block,color =factor(cuttree),group = interaction(cuttree))) + geom_line(stat='count') 
ggplot(mydata1,aes(Customer_care_calls,color =factor(cuttree),group = interaction(cuttree))) + geom_line(stat='count') 
ggplot(mydata1,aes(Prior_purchases,color =factor(cuttree),group = interaction(cuttree))) + geom_line(stat='count') 
ggplot(mydata1,aes(Discount_offered,color =factor(cuttree),group = interaction(cuttree))) + geom_line(stat='count') 
ggplot(mydata1,aes(Customer_rating,color =factor(cuttree),group = interaction(cuttree))) + geom_line(stat='count') 

