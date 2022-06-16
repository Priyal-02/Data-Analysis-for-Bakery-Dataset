getwd()
set.seed(123)
setwd("C:/Users/LENOVO/Downloads")
data<-read.csv("BreadBasket_DMS.csv")
data<-data.frame(data)
##Data Preprocessing
head(data,10)
tail(data,10)
dim(data)
str(data)
#check if missing values are present
colSums(is.na(data))
#removing duplicate rows
data<-distinct(data)
dim(data)
#There are values such as 'NONE' and 'Adjustment' for the Item attribute which must be cancellation or errors,
#hence we remove them
head(data[data$Item=='Adjustment',])
head(data[data$Item=='NONE',])
length(which(data$Item=="NONE"))
data=data[data$Item!='NONE',]
dim(data)
data=data[data$Item!='Adjustment',]
dim(data)

##Extracting hour from the time column
#install.packages("hms")
library(lubridate)
library("hms")  
time_hms<- as_hms(data$Time)
Hour<-format(as.POSIXct(time_hms), format = "%H")
data$Hour<-Hour
data$Hour<-as.numeric(Hour)
data<-data %>% mutate(Session =ifelse(Hour %in% 7:12,"Morning", ifelse(Hour %in% 13:18,"Afternoon",ifelse(Hour %in% 18:23,"Evening","Night"))))


##Market Basket Analysis for the bakery dataset
library(dplyr)
library(plyr)
#install.packages("arules")
library(arules)
transactionData <- ddply(data,c("Date","Transaction"),
                         function(df1)paste(df1$Item,
                                            collapse = ","))
transactionData$Transaction<-NULL
transactionData$Date<-NULL
colnames(transactionData)<-c('Items')
head(transactionData)
write.csv(transactionData,"E:/Bakery_DA/bakery_transaction.csv",quote=FALSE,row.names = FALSE)
tr <- read.transactions("E:/Bakery_DA/bakery_transaction.csv", format = 'basket', sep=',')

tr
summary(tr)
library("RColorBrewer")
itemFrequencyPlot(tr,topN=10,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")
itemFrequencyPlot(tr,topN=20,type="relative",col=brewer.pal(8,'Pastel2'),main="Relative Item Frequency Plot")

rules<-apriori(tr,
               parameter=list(minlen=2,
                              maxlen=10,
                              supp=0.01,
                            conf=0.7))

rules<-apriori(tr, parameter=list(minlen=2, maxlen=10, supp=0.01,conf=0.7),
               appearance = list()) 
inspect(rules)

#install.packages("arulesViz")
library(arulesViz)
plot(rules)
#plot(rules, method = "graph",  engine = "htmlwidget") #for interactive graph
plot(rules,method="paracoord")

##Plot to find the time of the day the sales are most influential
library(ggplot2)
library(forcats)
df<-dplyr::count(data, Session, sort = TRUE)
ggplot(data,aes(x=Session)) +
  geom_bar(fill='steelblue',width=0.75,stat='count')+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")+
  facet_wrap(facets = vars(Session), scales = "free_y")+
  labs(title="Total Purchases at Each Time_of_day",y='Purchases',x='Time of the day')

##Plot to figure out least selling items that can be replaced
itemList<-table(data$Item)
itemList<- data.frame(itemList[order(itemList)])
colnames(itemList)<-c('Item','Frequency')
items<-itemList[1:10,]
ggplot(items,aes(x=Item,y=Frequency))+
  geom_bar(fill='steelblue',width=0.75,stat='identity')+
  labs(title='10 Least Selling Items.',x='Item',y='Purchases')

##Plot to find the busiest hour of the sales.
ggplot(data, aes(x=Hour)) + 
  geom_histogram(binwidth=.5,
                 color="black",fill="#FFAE42")+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black")+
  labs(title="Total Purchases by Each Hour.",y='Purchases')
