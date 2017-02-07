rm(list=ls())
load("agil1.Rdata")

require(dplyr)
require(ggplot2)
library(plotrix)


orders$UnitDiscountPercent<-as.numeric(orders$UnitDiscountPercent)
orders$ProductID<-as.factor(orders$ProductID)

orders$UnitDiscountPercent<-ifelse(is.na(orders$UnitDiscountPercent),0,orders$UnitDiscountPercent)

############## Ordered Products ###############

OrderedProducts<- orders %>% group_by(ProductID) %>% summarise(maxdiscount = max(UnitDiscountPercent),mindiscount = min(UnitDiscountPercent),avgdiscount=mean(UnitDiscountPercent),totalrev = sum(ExtRevenue), totalqty= sum(Qty),ordercount=length(unique(OrderID)),customercount=length(unique(CustomerID)))


OrderedProducts<-merge(OrderedProducts,products,by="ProductID" )

rm("products")


#Absence of product re-order from customers - Only 4 products that have a re-order value from customer
Reorderedproducts<- OrderedProducts[(OrderedProducts$customercount-OrderedProducts$ordercount)!=0,]

# % of products always sold on discount -- ignore
nrow(OrderedProducts[OrderedProducts$mindiscount != 0,])/8000
# % of products never sold at discount
nrow(OrderedProducts[OrderedProducts$maxdiscount != 0,])/8000

ppielabels=c(
  paste(100- (round(nrow(OrderedProducts[OrderedProducts$mindiscount != 0,])/8000*100,2)+round(nrow(OrderedProducts[OrderedProducts$maxdiscount != 0,])/8000*100,2)),"% of products are sold\nwith or without a discount"),
  paste(round(nrow(OrderedProducts[OrderedProducts$maxdiscount != 0,])/8000*100,2),"% of products are\nnever sold at discount"),
  paste("\n",round(nrow(OrderedProducts[OrderedProducts$mindiscount != 0,])/8000*100,2),"% of products\nare always sold at discount")) 

pie3D(c(
  nrow(OrderedProducts[OrderedProducts$maxdiscount != 0,])/8000,
  100- (round(nrow(OrderedProducts[OrderedProducts$mindiscount != 0,])/8000*100,2)+round(nrow(OrderedProducts[OrderedProducts$maxdiscount != 0,])/8000*100,2)),
  round(nrow(OrderedProducts[OrderedProducts$mindiscount != 0,])/8000*100,2)),
  shade=0.4,explode = 0,theta=pi/4,col=c("#ddaa00","brown","#dd00dd"),labels=ppielabels,labelcex = 1.2,radius = 0.8,main= "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\nProducts sold at discount")


# Flaw in discounting policy - Products sold at loss
OrderedProducts$avgproductprice<-OrderedProducts$totalrev/OrderedProducts$totalqty
OrderedProducts$avgprofit<- OrderedProducts$avgproductprice - OrderedProducts$ProductCost

pielabels4=c(paste(round(nrow(OrderedProducts[OrderedProducts$avgprofit<=0,])/8000*100,2),"% of products\nare sold at a loss or\n at a profit of 0"),  #1292 products upto $55 loss per product
             paste(round(nrow(OrderedProducts[OrderedProducts$avgprofit>0,])/8000*100,2),"% of products\nare sold at profit")) #6708 products up to $122 per product

pie3D(c(nrow(OrderedProducts[OrderedProducts$avgprofit<=0,])/8000,nrow(OrderedProducts[OrderedProducts$avgprofit>0,])/8000),explode=0.18,shade=0.4,theta=pi/4,col=c("#ddaa00","brown"),labels=pielabels4,labelcex = 0.8,radius = 0.8,main= "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\nAvearage profit per product")

ggplot(OrderedProducts,aes(avgprofit,fill=as.factor(avgprofit>0))) + geom_histogram(breaks = seq(-60,130,by=5),color = "lightgrey")+  scale_fill_manual("",values = c("#ddaa00","brown"),labels= c("Loss","Profit")) +labs(x= "Average Profit", y="Number of Products",title = "Distribution of Average Profit among 8000 products")+scale_x_continuous(breaks = seq(-70, 130, by = 10))




############## Ordered Customers ###############

OrderedCusts<- orders %>% group_by(CustomerID) %>% summarise(maxdiscount = max(UnitDiscountPercent),mindiscount = min(UnitDiscountPercent),avgdiscount=mean(UnitDiscountPercent),totalrev = sum(ExtRevenue), totalqty= sum(Qty),ordercount=length(unique(OrderID)),productcount=length(unique(ProductID)))
OrderedCusts$revperorder<- OrderedCusts$totalrev/OrderedCusts$ordercount

#Absence of product re-order from customers - Only 4 products that have a re-order value from customer

pielabels<- c(
  paste(round(prop.table(table(OrderedCusts$ordercount))[1]*100,2),"% customers \n ordered only 1 time"),
  paste(round(prop.table(table(OrderedCusts$ordercount))[2]*100,2),"% customers\n ordered 2 times"),
  paste(round(prop.table(table(OrderedCusts$ordercount))[3]*100,2),"% customers\n ordered 3 times"))

pie3D(table(OrderedCusts$ordercount),labels = pielabels,labelcex=1.1,explode=0.1,col=c("brown","#ddaa00","#dd00dd"),shade = 0.4,radius=0.8, main = "\n\n\n\n\n% of Returning Customers")


# Customers in CRM who have not ordered yet - 19 Customers - All of whom have joined in the last 1 week after Dec 23rd 2011

newcsuts<-customers$CustomerID[!customers$CustomerID %in% OrderedCusts$CustomerID]

newcsutdate<- customers$JoinDate[customers$CustomerID %in% newcsuts]
min(newcsutdate)

# Distribution of revenue on customers

p = c(.01,.03,.05,.1,.15,.20,.50,.70,.80,.90,.95,0.96,0.97,0.98,0.99,1)
dat = data.frame(Revenue = quantile(OrderedCusts$totalrev, probs = p),CustomerShare = p)
ggplot(aes(x = CustomerShare, y = Revenue), data = dat) + geom_area(fill = "#ddaa00") + ggtitle("Distribution of revenue\non customers") + labs(x= "% of Customers",y= "Revenue per customer") +  scale_x_continuous(label=function(x){return(paste0(x*100, "%"))})

# Distribution of revenue per order on customer
dat2 = data.frame(Revenueperorder = quantile(OrderedCusts$revperorder, probs = p),CustomerShare = p)
ggplot(aes(x = CustomerShare, y = Revenueperorder), data = dat2) + geom_area(fill = "darkgoldenrod4") + ggtitle("Distribution of revenue per order\non customers") + labs(x= "% of Customers",y= "Revenue per order\nper customer") + scale_x_continuous(label=function(x){return(paste0(x*100, "%"))})


#No of customers who buy with no discount at all
nodiscusts<-nrow(OrderedCusts[OrderedCusts$maxdiscount==0,])/62980
yesdiscusts<-nrow(OrderedCusts[OrderedCusts$mindiscount!=0,])/62980
other<-1-(nodiscusts+yesdiscusts)

pielabels2 = c(
  paste0(round(nodiscusts*100,2),"% customers\nbought without\n any discount"),
  paste0("\n",round(yesdiscusts*100,2),"% customers\nbought only with discount"),
  paste0(round(other*100,2),"%\ncustomers\nare indifferent"))
  
pie3D(c(nodiscusts,yesdiscusts,other),explode=0.14,shade=0.4,theta=pi/4,col=c("brown","#ddaa00","#dd00dd"),labels=pielabels2,labelcex = 0.8,radius = 0.9,main= "\n\n\n\n\n\n\n\n\n\n\n\n\n\nCustomer's purchase sensitivity\nbased on discount")

# Revenue split based on discount
nodisc<-OrderedCusts[OrderedCusts$maxdiscount==0,]
nodisrev<-sum(nodisc$totalrev)/sum(OrderedCusts$totalrev)
yesdisc<-OrderedCusts[OrderedCusts$mindiscount!=0,]
yesdisrev<-sum(yesdisc$totalrev)/sum(OrderedCusts$totalrev)
otherrev<-1-(nodisrev+yesdisrev)

pielabels3 = c(
  paste0(round(nodisrev*100,2),"% revenue\ncomes without\n any discount"),
  paste0("\n",round(yesdisrev*100,2),"% revenue\ncomes after discount"),
  paste0(round(otherrev*100,2),"%\nrevenue\nare indifferent"))

pie3D(c(nodisrev,yesdisrev,otherrev),explode=0.2,shade=0.4,theta=pi/4,col=c("brown","#ddaa00","#dd00dd"),labels=pielabels3,labelcex = 0.8,radius = 0.8,main= "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\nRevenue comparision\nbased on discount")


