rm(list=ls())
load("agil1.Rdata")

require(dplyr)
require(ggplot2)
library(plotrix)

fulldata<-merge(orders,customers,by="CustomerID")

str(fulldata)

fulldata$firstpurchase<- as.Date(fulldata$OrderDate,"%Y-%m-%d")-as.Date(fulldata$JoinDate,"%Y-%m-%d")

fulldata<- fulldata %>% group_by(CustomerID) %>% mutate(th_order=dense_rank(OrderDate))

newcust<-fulldata %>% group_by(CustomerID) %>% summarize(firstorder=min(firstpurchase))
str(newcust)
newcust$firstorder<-as.numeric(newcust$firstorder)
hist(newcust$firstorder)

q<- as.data.frame(select(fulldata,CustomerID,OrderDate,firstpurchase,th_order))
q<-unique(q)
q$OrderDate<-as.Date(q$OrderDate,"%Y-%m-%d")
q<- q[order(q$OrderDate),] 
q<- as.data.frame(q %>% group_by(CustomerID) %>% mutate(daydiff= c(0,diff(OrderDate))))





# Days between Joining date and first purchase date

table(newcust$firstorder)  

qplot(newcust$firstorder)

pie3D(x=c(52475/62980,1-(52475/62980)),explode=0.25,shade=0.4,theta=pi/4,col=c("brown","#ddaa00"),labels=c("83.32%\nCustomers ordered on\nthe same date as\nthe Joining date","16.68%\nCustomers ordered on\na date different from\nthe Joining date"),labelcex = 0.8,radius = 0.8,main= "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\nFirst Order from customers since Joining")

ggplot(newcust[newcust$firstorder!=0,],aes(firstorder))+geom_bar(colour = "lightgrey",fill = '#ddaa00',alpha=0.8)+scale_x_continuous(breaks = round(seq(1, 12, by = 1),1),label=function(x){return(paste(x, "Days"))}) + labs(x= "Days since joining", y= "Customers",title="Number of days taken to place the first order\n if a order is not placed on the Joining Date")

# Days between first purchase date and second purchase date
ggplot(q[q$th_order=="2",],aes(daydiff)) +geom_histogram(binwidth = 360)

ggplot(q[q$th_order=="2",],aes(daydiff))+geom_histogram(binwidth = 360,colour = "lightgrey",fill = 'darkgoldenrod4',alpha=0.8,position = "dodge")+scale_x_continuous(breaks = round(seq(0, 2700, by = 360),0),label=function(x){return(paste(round(x/360,0), "Years"))}) + labs(x= "Years since first order", y= "Customers",title="Number of Years of gap for a returning customers\n(Returning for a second order)")

# Days between second purchase date and third purchase date

ggplot(q[q$th_order=="3",],aes(daydiff)) +geom_histogram(binwidth = 360)

ggplot(q[q$th_order=="3",],aes(daydiff))+geom_histogram(binwidth = 360,colour = "lightgrey",fill = 'darkorange4',alpha=0.8,position = "dodge")+scale_x_continuous(breaks = round(seq(0, 2700, by = 360),0),label=function(x){return(paste(round(x/360,0), "Years"))}) + labs(x= "Years since second order", y= "Customers",title="Number of Years of gap for second time returning customers\n(Returning for a third order)")

