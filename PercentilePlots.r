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
values = c(mean(merged$PERCENTILE25),mean(merged$PERCENTILE50),mean(merged$PERCENTILE75),mean(merged$PERCENTILE100))
data = data.frame(percentiles,values)
years = c('2014','2019')


ggplot(data, aes(x=values,y=percentiles)) + 
    geom_line(size = 2, color="blue")


ggplot(data, aes(y=values,x=percentiles)) + 
    geom_line(aes(color="blue")) +
    geom_point() +
    ggtitle("Price by Percentile of Energy Offered") +
    xlab("Percentile of Energy Amount Offered") +
    ylab("Price per MWh ($)")

#NSW (SITHE01)
#QLD - SMCFS1
#SA - QPS1,...,QPS5
#TAS - BBTHREE1,...BBTHREE3
#TAS - MUSSELR1, TVCC201, TVPP104
#VIC - JLA01,...,JLA04