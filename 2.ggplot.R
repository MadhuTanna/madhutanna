library(ggplot2)
library(dplyr)
library(glmnet)
library(caret)
library(ggpubr)

setwd("C:/Users/mtanna108360/Downloads/Downloads/Data Science/Proschool/Term 3 Recordings/Project")
custdata=read.csv("term3.csv")
cust_data_table = tbl_df(custdata)
cust_data_table = na.omit(cust_data_table)
cust_data_table = cust_data_table[,-1]
NROW(cust_data_table)
str(cust_data_table)

#some difference due to customer rating 1
cust_data_table = cust_data_table %>%filter(cust_data_table$Discount_offered<10)
ggplot(cust_data_table,aes(Customer_rating,fill =factor(Reached.on.Time_Y.N))) + geom_bar(position="dodge2")

cust_data_table = cust_data_table %>%filter(cust_data_table$Discount_offered>10)
ggplot(cust_data_table,aes(Customer_rating,fill =factor(Reached.on.Time_Y.N))) + geom_bar(position="dodge2")

#Benefits on prior purchse if discount offered is less
ggplot(cust_data_table,aes(Prior_purchases,fill =factor(Reached.on.Time_Y.N))) + geom_bar(position="dodge2") + scale_x_continuous(limit=c(1,11))

#Highest Payment Order - zero line is crossing one line for high priced order. For orders having less discount, zero line is above one
ggplot(cust_data_table,aes(Cost_of_the_Product,fill =factor(Reached.on.Time_Y.N))) + geom_bar(position="dodge2")
ggplot(cust_data_table,aes(Cost_of_the_Product,color =factor(Reached.on.Time_Y.N),group = interaction(Reached.on.Time_Y.N))) + geom_line(stat='count') 

# impact of discount offered
ggplot(cust_data_table,aes(Discount_offered,fill =factor(Reached.on.Time_Y.N))) + geom_bar(position="dodge2") + scale_x_continuous(limit=c(0,13)) 
# order having discount >10
ggplot(cust_data_table,aes(as.factor(Discount_offered>10),fill=Discount_offered>10)) + geom_bar()







#NULL Hypothesis "The null hypothesis states that there is no difference between the two distributions"
#p value less than 0.05 rejecting null hypothesis
x3 = rnorm(1000)
hist(x3)
ks.test(cust_data_table$Discount_offered,x3)
hist(cust_data_table$Discount_offered)

