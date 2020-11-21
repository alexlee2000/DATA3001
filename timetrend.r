library(dplyr) 
library(readxl) 
library(lubridate) #for dates 

# -------------------------------- Using biddayoffer and bidperofffer ----------------------- #
# We are importing the data provided to us, dropping irrelevant variables and merging the     #
# 2 tables into 1 by DUID and settlement date. We then partially clean the data for the       #
# variables Priceband and Bandavail. I also obtained an external source for region and        #
# fuel_type information. NOTE *the sql stuff is going to take some time to run.               #
# ------------------------------------------------------------------------------------------- #

### -------------------------------------------------- Initial Steps ---------------------------------------------------###

# Reading in initial data provided from client
biddayoffer <- read.csv("~/Desktop/biddayoffer_fy2009-2019.csv")
bidperoffer <- read.csv("~/Desktop/bidperoffer_fy2009-2019_v2.csv")

# Filtering for ENERGY bidtype only
biddayoffer <- filter(biddayoffer, biddayoffer$BIDTYPE == "ENERGY")
bidperoffer <- filter(bidperoffer, bidperoffer$BIDTYPE == "ENERGY")

# Dropping irrelevant variables from biddayoffer and bidperoffer
biddayoffer_final <- select(biddayoffer, c("DUID", "SETTLEMENTDATE", "OFFERDATE", "VERSIONNO", "PARTICIPANTID", 
                                           "DAILYENERGYCONSTRAINT", "REBIDEXPLANATION", "LASTCHANGED", "ENTRYTYPE", 
                                           "PRICEBAND1", "PRICEBAND2", "PRICEBAND3", "PRICEBAND4", "PRICEBAND5", 
                                           "PRICEBAND6", "PRICEBAND7", "PRICEBAND8", "PRICEBAND9", "PRICEBAND10"))

bidperoffer_final <- select(bidperoffer, c("DUID", "BIDSETTLEMENTDATE", "OFFERDATE", "VERSIONNO", "PERIODID", 
                                           "MAXAVAIL", "BANDAVAIL1", "BANDAVAIL2", "BANDAVAIL3", "BANDAVAIL4", 
                                           "BANDAVAIL5", "BANDAVAIL6", "BANDAVAIL7", "BANDAVAIL8", "BANDAVAIL9", 
                                           "BANDAVAIL10", "LASTCHANGED", "PASAAVAILABILITY", "INTERVAL_DATETIME"))

# Merging the two bid tables 
bid_merged <- merge(biddayoffer_final, bidperoffer_final, 
                    by.x = c("DUID", "SETTLEMENTDATE", "VERSIONNO", "OFFERDATE"), 
                    by.y = c("DUID", "BIDSETTLEMENTDATE", "VERSIONNO","OFFERDATE"))

# Removing these 4 chungus tables from your global environment  
rm(biddayoffer_final,bidperoffer_final)

### -------------------------------------------------- Data Cleaning ---------------------------------------------------###

# Tells us how many missing values there are in the data sets (should return 0)
sum(is.na(bid_merged))


# Re-formatting the variable INTERVAL_DATETIME and creating 3 new time variables (Day, Hour, Minute)
bid_merged$INTERVAL_DATETIME <- as.POSIXct(strptime(bid_merged$INTERVAL_DATETIME, "%Y-%m-%d %H:%M:%S"))
bid_merged$Day <- format(bid_merged$INTERVAL_DATETIME, format="%j") 
bid_merged$Hour <- format(bid_merged$INTERVAL_DATETIME, format="%H")
bid_merged$Minute <- format(bid_merged$INTERVAL_DATETIME, format="%M") 

### --------------------------------------------------          4.1         ---------------------------------------------------###

### -------------------------------------------------- Price times quantity ---------------------------------------------------###
bid_merged = bid_merged %>% 
  mutate(pq1 = PRICEBAND1 * BANDAVAIL1)
bid_merged = bid_merged %>% 
  mutate(pq2 = PRICEBAND2 * BANDAVAIL2)
bid_merged = bid_merged %>% 
  mutate(pq3 = PRICEBAND3 * BANDAVAIL3)
bid_merged = bid_merged %>% 
  mutate(pq4 = PRICEBAND4 * BANDAVAIL4)
bid_merged = bid_merged %>% 
  mutate(pq5 = PRICEBAND5 * BANDAVAIL5)
bid_merged = bid_merged %>% 
  mutate(pq6 = PRICEBAND6 * BANDAVAIL6)
bid_merged = bid_merged %>% 
  mutate(pq7 = PRICEBAND7 * BANDAVAIL7)
bid_merged = bid_merged %>% 
  mutate(pq8 = PRICEBAND8 * BANDAVAIL8)
bid_merged = bid_merged %>% 
  mutate(pq9 = PRICEBAND9 * BANDAVAIL9)
bid_merged = bid_merged %>% 
  mutate(pq10 = PRICEBAND10 * BANDAVAIL10)


### -------------------------------------------------- total money spend ---------------------------------------------------###
bid_merged = bid_merged %>% 
  mutate(pq = pq1+pq2+pq3+pq4+pq5+pq6+pq7+pq8+pq9+pq10)

### -------------------------------------------------- average price for each generator---------------------------------------------------###
bid_merged = bid_merged %>% 
  mutate(a_price = pq/(BANDAVAIL1+BANDAVAIL2
                       +BANDAVAIL3+BANDAVAIL4
                       +BANDAVAIL5+BANDAVAIL6
                       +BANDAVAIL7+BANDAVAIL8
                       +BANDAVAIL9+BANDAVAIL10))

### -------------------------------------------------- average price for each 5 min---------------------------------------------------###

newset =bid_merged %>% 
  group_by(INTERVAL_DATETIME) %>%
  summarize(mean_price = mean(a_price))

newset$INTERVAL_DATETIME <- as.POSIXct(strptime(newset$INTERVAL_DATETIME, "%Y-%m-%d %H:%M:%S"))
newset$Day <- format(newset$INTERVAL_DATETIME, format="%j") 
newset$Hour <- format(newset$INTERVAL_DATETIME, format="%H")
newset$Year <- format(newset$INTERVAL_DATETIME, format="%Y") 



### --------------------------------------------------        plot                      ---------------------------------------------------###

newset <- na.omit(newset)


png(file="saving_plot1.jpeg",width=1080, height=1080)
plot(newset$Hour, newset$mean_price, main="Scatterplot",
     xlab="hours", ylab="mean price ", pch=10)
dev.off()

png(file="saving_plot2.jpeg",width=1080, height=1080)
ex1 =newset %>% 
  group_by(Hour) %>%
  summarize(mean_price = mean(mean_price))
plot(ex1$Hour, ex1$mean_price, main="Scatterplot",
     xlab="hours", ylab="mean price ", pch=10, col="blue")
dev.off()

png(file="saving_plotexp1.jpeg",width=1080, height=1080)
newdata <- subset(newset, Year==2019 & Day == 100)

plot(newdata$Hour, newdata$mean_price, main="Year2019 Day 100",
     xlab="hours", ylab="mean price ", pch=10, col="blue")
dev.off()


png(file="saving_plot3.jpeg",width=1080, height=1080)
plot(newset$Day, newset$mean_price, main="Scatterplot",
     xlab="Day", ylab="mean price ", pch=10)
dev.off()

png(file="saving_plot4.jpeg",width=1080, height=1080)
plot(lowess(newset$Day,newset$mean_price,f=0.2), col="blue",
     xlab="Day", ylab="mean price ")
dev.off()

png(file="saving_plotexp2.jpeg",width=1080, height=1080)
newdata <- subset(newset, Year==2018 & Hour == 13)
plot(newdata$Day, newdata$mean_price, main="Year==2018 & Hour == 13",
     xlab="Day", ylab="mean price ", pch=10, col="blue")
dev.off()


png(file="saving_plot5.jpeg",width=1080, height=1080)
plot(newset$INTERVAL_DATETIME, newset$mean_price, main="Scatterplot",
     xlab="year ", ylab="mean price ", pch=10)
dev.off()

png(file="saving_plot6.jpeg",width=1080, height=1080)
ex1 =newset %>% 
  group_by(Year) %>%
  summarize(mean_price = mean(mean_price))
plot(ex1$Year, ex1$mean_price, main="Scatterplot",
     xlab="year", ylab="mean price ", pch=10, col="blue")
dev.off()

png(file="saving_plotexp3.jpeg",width=1080, height=1080)
newdata <- subset(newset, Day==126 & Hour == 16)
plot(newdata$Year, newdata$mean_price, main="Day==126 & Hour == 16",
     xlab="Year", ylab="mean price ", pch=10, col="blue")
dev.off()







x <- bid_merged[,c(10:19)]
x = cbind(x,bid_merged[,c(24:33)])
cl <- kmeans(x, 3, nstart=25, iter.max=1000)
cl$centers
new = cbind(bid_merged,cluster = cl$cluster)

