rm(list=ls())
load("agil1.Rdata")

require(dplyr)
require(ggplot2)

Sourcewise<- customers %>% 
  group_by(SourceID,format(customers$JoinDate,'%Y')) %>% 
  summarize((custacq=length(CustomerID)))
names(Sourcewise)<- c("SourceID", "Year", "CustomerCount")

Sourcewise<- merge(Sourcewise, marketingc, by=c("SourceID", "Year"))
Sourcewise<- merge(Sourcewise, sources, by=c("SourceID"))
Sourcewise$SourceID<-NULL

# Sourcewise<-cbind(Sourcewise[,c(1,4)],scale(Sourcewise[,2:3],scale = F))
# str(Sourcewise)
# ggplot(Sourcewise) + geom_line(aes(x=Year,y=MarketingCost,group=1,color="MarketingCost")) + geom_line(aes(x=Year,y=CustomerCount,group=1,color="CC")) +facet_wrap(~SourceName,scales="free") +
#   scale_color_manual(name = "combined legend",
#                      values=c(MarketingCost = "red", CC = "blue"))
# 
# ggplot(Sourcewise) + geom_line(aes(y=CustomerCount,x=MarketingCost,group=1,color="MarketingCost")) + #+ geom_line(aes(x=Year,y=CustomerCount,group=1,color="CC")) 
# facet_wrap(~SourceName,scales="free") +
#   scale_color_manual(name = "combined legend",
#                      values=c(MarketingCost = "red", CC = "blue"))

Sourcewise <- Sourcewise %>% group_by(SourceName) %>% mutate(scaledcc=scale(CustomerCount),scaledmc=scale(MarketingCost))

qplot(customers$SourceID)
ggplot(Sourcewise) + geom_bar(stat = "identity",aes(x= SourceName,y = MarketingCost,fill=as.factor(SourceName)),show.legend = FALSE)+ facet_wrap(~Year,scales="free") + theme(axis.text.x = element_text(angle=45, vjust=1, size=8, hjust=1)) + scale_x_discrete(label=function(x) abbreviate(x, minlength=9))+labs(x=NULL,y="Marketing Spend",title= "Marketing Spend each year")+ scale_fill_brewer(palette="PuOr")

ggplot(Sourcewise) + geom_bar(stat = "identity",aes(x= SourceName,y = CustomerCount,fill=as.factor(SourceName)),show.legend = FALSE) + facet_wrap(~Year,scales = "free")+ theme(axis.text.x = element_text(angle=45, vjust=1, size=8, hjust=1)) + scale_x_discrete(label=function(x) abbreviate(x, minlength=9))+labs(x=NULL,y="Marketing Spend",title= "Customers acquired each year") + scale_fill_brewer(palette="PuOr")



# Cheapest source to acquire customers
Sourcewise$mcpercust<- Sourcewise$MarketingCost/Sourcewise$CustomerCount
head(Sourcewise)
ggplot(Sourcewise) + geom_bar(stat = "identity",aes(x= SourceName,y = mcpercust,fill=as.factor(SourceName)),show.legend = F)+ facet_wrap(~Year,scales="free_x") +ggtitle("Marketing cost per Customers acquired") +xlab("Source Name") + ylab("Marketing Cost") + guides(fill=guide_legend(title="Sources"))+ theme(axis.text.x = element_text(angle=45, vjust=1, size=8, hjust=1)) + scale_x_discrete(label=function(x) abbreviate(x, minlength=9))+labs(x=NULL,y="Marketing Spend\nper customer acquired",title= "Marketing spend to acquire a customer") + scale_fill_brewer(palette="PuOr")

# No Dimininshing returns yet. So keep investing!
ggplot(Sourcewise) + geom_line(aes(Year,scaledmc,color="Marketing Cost"),group=1) + geom_line(aes(Year,scaledcc,color="Customer Count"),group=1) +facet_wrap(~SourceName,scales = "free_x")+labs(y="Scaled Values",title="Scaled comparision of Marketing cost & Customers acquired",x=NULL) + scale_colour_manual("", values = c("Marketing Cost"="cadetblue", "Customer Count"="brown"))
