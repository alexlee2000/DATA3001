library(ggplot2)

merged = read.csv("bid_merged_percentiles.csv", header = TRUE)

states = c(rep("NSW" , 4) , rep("QLD" , 4) , rep("TAS" , 4) , rep("SA" , 4), rep("VIC" , 4))
percentiles = factor(rep(c("25th" , "50th" , "75th", "100th") , 5),levels = c("25th" , "50th" , "75th", "100th"))

# All Generators
values = c(mean(merged$PERCENTILE25),mean(merged$PERCENTILE50),mean(merged$PERCENTILE75),mean(merged$PERCENTILE100))
data = data.frame(percentiles,values)
ggplot(data, aes(y=values, x=percentiles)) + 
    geom_bar(stat="identity", fill="navy") +
    ggtitle("Price by Percentile of Energy Offered") +
    xlab("Percentile of Energy Amount Offered") +
    ylab("Price per MWh ($)")


# By State (NSW, QLD, TAS, SA, VIC)
NSWdata <- merged[which(merged$Region == 'NSW1'),]
QLDdata <- merged[which(merged$Region == 'QLD1'),]
TASdata <- merged[which(merged$Region == 'TAS1'),]
SAdata <- merged[which(merged$Region == 'SA1'),]
VICdata <- merged[which(merged$Region == 'VIC1'),]

values <- c(mean(NSWdata$PERCENTILE25),mean(NSWdata$PERCENTILE50),mean(NSWdata$PERCENTILE75),mean(NSWdata$PERCENTILE100),
                mean(QLDdata$PERCENTILE25),mean(QLDdata$PERCENTILE50),mean(QLDdata$PERCENTILE75),mean(QLDdata$PERCENTILE100),
                mean(TASdata$PERCENTILE25),mean(TASdata$PERCENTILE50),mean(TASdata$PERCENTILE75),mean(TASdata$PERCENTILE100),
                mean(SAdata$PERCENTILE25),mean(SAdata$PERCENTILE50),mean(SAdata$PERCENTILE75),mean(SAdata$PERCENTILE100),
                mean(VICdata$PERCENTILE25),mean(VICdata$PERCENTILE50),mean(VICdata$PERCENTILE75),mean(VICdata$PERCENTILE100))

data = data.frame(percentiles, values, states)

ggplot(data, aes(fill=states, y=values, x=percentiles)) + 
    geom_bar(position="dodge", stat="identity") +
    ggtitle("Price by Percentile of Energy Offered by State") +
    xlab("Percentile of Energy Amount Offered") +
    ylab("Price per MWh ($)")

# Every Generator, by year
data2014 <- merged[which(merged$Year == 2014),]
data2017 <- merged[which(merged$Year == 2017),]
values = c(mean(data2014$PERCENTILE25),mean(data2014$PERCENTILE50),
            mean(data2014$PERCENTILE75),mean(data2014$PERCENTILE100),
            mean(data2017$PERCENTILE25),mean(data2017$PERCENTILE50),
            mean(data2017$PERCENTILE75),mean(data2017$PERCENTILE100))



percentiles = factor(c("25th" , "50th" , "75th", "100th"),levels = c("25th" , "50th" , "75th", "100th"))

data = data.frame(years,percentiles,values)

ggplot(data, aes(y=values,x=percentiles,group = years)) + 
    geom_line(aes(color=years)) +
    geom_point(aes(color = years)) +
    ggtitle("Price by Percentile of Energy Offered") +
    xlab("Percentile of Energy Amount Offered") +
    ylab("Price per MWh ($)")



years = c(rep(2008,4),rep(2009,4),rep(2010,4),rep(2011,4),
          rep(2012,4),rep(2013,4),rep(2014,4),rep(2015,4),
          rep(2016,4),rep(2017,4),rep(2018,4),rep(2019,4))
data2008 <- merged[which(merged$Year == 2008),]
data2009 <- merged[which(merged$Year == 2009),]
data2010 <- merged[which(merged$Year == 2010),]
data2011 <- merged[which(merged$Year == 2011),]
data2012 <- merged[which(merged$Year == 2012),]
data2013 <- merged[which(merged$Year == 2013),]
data2014 <- merged[which(merged$Year == 2014),]
data2015 <- merged[which(merged$Year == 2015),]
data2016 <- merged[which(merged$Year == 2016),]
data2017 <- merged[which(merged$Year == 2017),]
data2018 <- merged[which(merged$Year == 2018),]
data2019 <- merged[which(merged$Year == 2019),]

values = c(mean(data2008$PERCENTILE25),mean(data2008$PERCENTILE50),
           mean(data2008$PERCENTILE75),mean(data2008$PERCENTILE100),
           mean(data2009$PERCENTILE25),mean(data2009$PERCENTILE50),
           mean(data2009$PERCENTILE75),mean(data2009$PERCENTILE100),
           mean(data2010$PERCENTILE25),mean(data2010$PERCENTILE50),
           mean(data2010$PERCENTILE75),mean(data2010$PERCENTILE100),
           mean(data2011$PERCENTILE25),mean(data2011$PERCENTILE50),
           mean(data2011$PERCENTILE75),mean(data2011$PERCENTILE100),
           mean(data2012$PERCENTILE25),mean(data2012$PERCENTILE50),
           mean(data2012$PERCENTILE75),mean(data2012$PERCENTILE100),
           mean(data2013$PERCENTILE25),mean(data2013$PERCENTILE50),
           mean(data2013$PERCENTILE75),mean(data2013$PERCENTILE100),
           mean(data2014$PERCENTILE25),mean(data2014$PERCENTILE50),
           mean(data2014$PERCENTILE75),mean(data2014$PERCENTILE100),
           mean(data2015$PERCENTILE25),mean(data2015$PERCENTILE50),
           mean(data2015$PERCENTILE75),mean(data2015$PERCENTILE100),
           mean(data2016$PERCENTILE25),mean(data2016$PERCENTILE50),
           mean(data2016$PERCENTILE75),mean(data2016$PERCENTILE100),
           mean(data2017$PERCENTILE25),mean(data2017$PERCENTILE50),
           mean(data2017$PERCENTILE75),mean(data2017$PERCENTILE100),
           mean(data2018$PERCENTILE25),mean(data2018$PERCENTILE50),
           mean(data2018$PERCENTILE75),mean(data2018$PERCENTILE100),
           mean(data2019$PERCENTILE25),mean(data2019$PERCENTILE50),
           mean(data2019$PERCENTILE75),mean(data2019$PERCENTILE100))

percentiles = factor(rep(c("25th" , "50th" , "75th", "100th"),12),levels = c("25th" , "50th" , "75th", "100th"))

data = data.frame(years,percentiles,values)

ggplot(data, aes(y=values,x=percentiles,group = years)) + 
    geom_line(aes(color=years)) +
    geom_point(aes(color = years)) +
    ggtitle("Price by Percentile of Energy Offered") +
    xlab("Percentile of Energy Amount Offered") +
    ylab("Price per MWh ($)")


#NSW (SITHE01)
#QLD - SMCFS1
#SA - QPS1,...,QPS5
#TAS - BBTHREE1,...BBTHREE3
#TAS - MUSSELR1, TVCC201, TVPP104
#VIC - JLA01,...,JLA04