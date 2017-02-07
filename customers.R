setwd("~/Documents/UCD/BA Prac/Agilone")

require(dplyr)
require(ggplot2)


#Load Saved environment

load("agil1.Rdata")

str(customers)
fctrs<-c("SourceID","State","Country")
customers[fctrs]<- lapply(customers[fctrs], as.factor)
#customers$JoinDate<-as.Date(customers$JoinDate,"%Y-%m-%d")

length(unique(customers$CustomerID))
length(unique(customers$Email))
length(unique(customers$Zipcode))

# Issue in portal checking duplicate emails - 550
length(unique(customers$CustomerID))
length(unique(customers$Email))
nrow((customers[customers$Address1=="UNKNOWN",]))
nrow((customers[customers$City=="NULL",]))

#Customers from country
length(unique(customers$Country))
round(prop.table(table(customers$Country))*100,2)

custgrowth<- customers %>% dplyr::group_by(JoinDate) %>% summarize(custacq=length(CustomerID))

ggplot(custgrowth,aes(JoinDate,cumsum(custacq)))+geom_area(stat="identity",fill="goldenrod4",alpha=0.8) +theme_grey()+xlab("Date") +ylab("Customers Acquired") +ggtitle("Market growth since 2005")

ggplot(custgrowth,aes(JoinDate,custacq))+geom_line(stat="identity")

custgrowth$month<-factor(months(custgrowth$JoinDate),ordered = F)

ggplot(custgrowth) + geom_boxplot(aes(y=custgrowth$custacq,x=reorder(format(custgrowth$JoinDate,'%b'),custgrowth$JoinDate)),fill="goldenrod1",color="darkred") + xlab("Months") + ylab("No. of Customers Acquired") +ggtitle("Average Customers Acquired each month")

ggplot(custgrowth) + geom_boxplot(aes(y=custgrowth$custacq,x=reorder(format(custgrowth$JoinDate,'%Y'),custgrowth$JoinDate)),fill="goldenrod1",color="darkred") + xlab("Year") + ylab("No. of Customers Acquired") +ggtitle("Average Customers Acquired each year")


custgrowth$yr<- format(custgrowth$JoinDate,'%Y')
custgrowth$wkday<-weekdays(custgrowth$JoinDate,abbreviate = T)
ggplot(custgrowth) + geom_boxplot(aes(y=custgrowth$custacq,x=wkday,shape=NA) )+ facet_wrap(~yr,scales = "free")



