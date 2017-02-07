#Load the Excel file, convert to csvs and setup and environment

require(readxl)

n<- 9

dataset <- lapply(2:n, function(i)  read_excel("DataSet_20141008.xlsx",i) )

names(dataset) <- c("customers","products","orders2011","orders2010","orders2009","historders","marketingc","sources")

lapply(1:length(dataset), function(x) write.csv(dataset[[x]],file = paste0(names(dataset[x]),".csv"),row.names = FALSE))

list2env(dataset,.GlobalEnv)

rm("dataset","n")

orders<-rbind(historders,orders2009,orders2010,orders2011)

rm(list = c("historders","orders2009","orders2010","orders2011"))

save(list = ls(all.names = TRUE),file="agil1.Rdata")


