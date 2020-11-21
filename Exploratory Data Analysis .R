library(dplyr) 
library(ggplot2)
library(reshape)
library(gridExtra)

### -------------------------------------- 7.6 Initial Analysis Pricebands ---------------------------------------###

# Priceband Boxplot Fig 4.1.1
boxplot(bid_merged_pb$PRICEBAND1, bid_merged_pb$PRICEBAND2, bid_merged_pb$PRICEBAND3, bid_merged_pb$PRICEBAND4, 
        bid_merged_pb$PRICEBAND5, bid_merged_pb$PRICEBAND6, bid_merged_pb$PRICEBAND7, bid_merged_pb$PRICEBAND8, 
        bid_merged_pb$PRICEBAND9, bid_merged_pb$PRICEBAND10, 
        names = c("PB1", "PB2", "PB3", "PB4", "PB5", "PB6", "PB7", "PB8", "PB9", "PB10"),
        main = "Price Distribution", xlab = "Pricebands", ylab = "Price ($AUD)")

# Priceband Boxplot (without outliers + subset 1) Fig 4.1.2
par(mfrow= c(2,2))
boxplot(bid_merged_pb$PRICEBAND1, main = "Price Distribution", xlab = "Priceband1", ylab = "Price ($AUD)", outline = FALSE)
boxplot(bid_merged_pb$PRICEBAND8, main = "Price Distribution", xlab = "Priceband8", ylab = "Price ($AUD)", outline = FALSE)
boxplot(bid_merged_pb$PRICEBAND9, main = "Price Distribution", xlab = "Priceband9", ylab = "Price ($AUD)", outline = FALSE)
boxplot(bid_merged_pb$PRICEBAND10, main = "Price Distribution", xlab = "Priceband10", ylab = "Price ($AUD)", outline = FALSE)


# Priceband Boxplot (without outliers + subset 2) Fig 4.1.3
par(mfrow= c(1,1))
boxplot(bid_merged_pb$PRICEBAND2, bid_merged_pb$PRICEBAND3, bid_merged_pb$PRICEBAND4, 
        bid_merged_pb$PRICEBAND5, bid_merged_pb$PRICEBAND6, bid_merged_pb$PRICEBAND7, 
        names = c("PB2", "PB3", "PB4", "PB5", "PB6", "PB7"),
        main = "Price Distribution", xlab = "Pricebands", ylab = "Price ($AUD)", outline = FALSE)

# Summaries of priceband quartiles Out 4.1.1
summary(bid_merged_pb$PRICEBAND1)
summary(bid_merged_pb$PRICEBAND2)
summary(bid_merged_pb$PRICEBAND3)
summary(bid_merged_pb$PRICEBAND4)
summary(bid_merged_pb$PRICEBAND5)
summary(bid_merged_pb$PRICEBAND6)
summary(bid_merged_pb$PRICEBAND7)
summary(bid_merged_pb$PRICEBAND8)
summary(bid_merged_pb$PRICEBAND9)
summary(bid_merged_pb$PRICEBAND10)

# find the upper inner fence value for each priceband
maxval <- apply(bid_merged_pb[,2:11], MARGIN = 2, 
                function(x) {quantile(x, probs = 0.75) + (IQR(x) * 1.5)})
print(maxval)

# find number of outliers in each priceband (above upper inner fence)
maxvals <- data.frame("PB1" = 0, "PB2" = 0, "PB3" = 0, "PB4" = 0, "PB5" = 0, 
                      "PB6" = 0, "PB7" = 0, "PB8" = 0, "PB9" = 0, "PB10" = 0)
maxvals[,1] <- sum(bid_merged_pb$PRICEBAND1 > maxval[1])
maxvals[,2] <- sum(bid_merged_pb$PRICEBAND2 > maxval[2])
maxvals[,3] <- sum(bid_merged_pb$PRICEBAND3 > maxval[3])
maxvals[,4] <- sum(bid_merged_pb$PRICEBAND4 > maxval[4])
maxvals[,5] <- sum(bid_merged_pb$PRICEBAND5 > maxval[5])
maxvals[,6] <- sum(bid_merged_pb$PRICEBAND6 > maxval[6])
maxvals[,7] <- sum(bid_merged_pb$PRICEBAND7 > maxval[7])
maxvals[,8] <- sum(bid_merged_pb$PRICEBAND8 > maxval[8])
maxvals[,9] <- sum(bid_merged_pb$PRICEBAND9 > maxval[9])
maxvals[,10] <- sum(bid_merged_pb$PRICEBAND10 > maxval[10])

# find the lower inner fence value for each priceband
minval <- apply(bid_merged_pb[,2:11], MARGIN = 2, 
                function(x) {quantile(x, probs = 0.25) - (IQR(x) * 1.5)})
print(minval)

# find number of outliers in each priceband (below lower inner fence)
minvals <- data.frame("PB1" = 0, "PB2" = 0, "PB3" = 0, "PB4" = 0, "PB5" = 0, 
                      "PB6" = 0, "PB7" = 0, "PB8" = 0, "PB9" = 0, "PB10" = 0)
minvals[,1] <- sum(bid_merged_pb$PRICEBAND1 < minval[1])
minvals[,2] <- sum(bid_merged_pb$PRICEBAND2 < minval[2])
minvals[,3] <- sum(bid_merged_pb$PRICEBAND3 < minval[3])
minvals[,4] <- sum(bid_merged_pb$PRICEBAND4 < minval[4])
minvals[,5] <- sum(bid_merged_pb$PRICEBAND5 < minval[5])
minvals[,6] <- sum(bid_merged_pb$PRICEBAND6 < minval[6])
minvals[,7] <- sum(bid_merged_pb$PRICEBAND7 < minval[7])
minvals[,8] <- sum(bid_merged_pb$PRICEBAND8 < minval[8])
minvals[,9] <- sum(bid_merged_pb$PRICEBAND9 < minval[9])
minvals[,10] <- sum(bid_merged_pb$PRICEBAND10 < minval[10])

outliers <- data.frame(cbind(maxval, t(maxvals), minval, t(minvals)))
colnames(outliers) <- c("Upper inner fence value", "Number outliers Upper",
                        "Lower inner fence value", "Number outliers Lower")
print(outliers)
write.csv(outliers,"outliers_table_pb.csv")

# Find the Variances of each band 
var <- data.frame("PB1" = 0, "PB2" = 0, "PB3" = 0, "PB4" = 0, "PB5" = 0, 
                  "PB6" = 0, "PB7" = 0, "PB8" = 0, "PB9" = 0, "PB10" = 0)
var[,1] <- var(bid_merged_pb$PRICEBAND1)
var[,2] <- var(bid_merged_pb$PRICEBAND2)
var[,3] <- var(bid_merged_pb$PRICEBAND3)
var[,4] <- var(bid_merged_pb$PRICEBAND4)
var[,5] <- var(bid_merged_pb$PRICEBAND5)
var[,6] <- var(bid_merged_pb$PRICEBAND6)
var[,7] <- var(bid_merged_pb$PRICEBAND7)
var[,8] <- var(bid_merged_pb$PRICEBAND8)
var[,9] <- var(bid_merged_pb$PRICEBAND9)
var[,10] <- var(bid_merged_pb$PRICEBAND10)

# Histograms of each priceband Fig 4.1.4
grid.arrange(ggplot(bid_merged_pb, aes(bid_merged_pb$PRICEBAND1)) + 
            geom_histogram(col = "black", fill = "light blue") +
            theme(axis.text = element_text(size = 11),
                  axis.title = element_text(size = 13, face = "bold")) +
            labs(x = "PRICEBAND1"),
          
          ggplot(bid_merged_pb, aes(bid_merged_pb$PRICEBAND2)) + 
            geom_histogram(col = "black", fill = "light blue") +
            theme(axis.text = element_text(size = 11),
                  axis.title = element_text(size = 13, face = "bold")) +
            labs(x = "PRICEBAND2"),
          
          ggplot(bid_merged_pb, aes(bid_merged_pb$PRICEBAND3)) + 
            geom_histogram(col = "black", fill = "light blue") +
            theme(axis.text = element_text(size = 11),
                  axis.title = element_text(size = 13, face = "bold")) +
            labs(x = "PRICEBAND3"),
          
          ggplot(bid_merged_pb, aes(bid_merged_pb$PRICEBAND4)) + 
            geom_histogram(col = "black", fill = "light blue") +
            theme(axis.text = element_text(size = 11),
                  axis.title = element_text(size = 13, face = "bold")) +
            labs(x = "PRICEBAND4"),
  
          ggplot(bid_merged_pb, aes(bid_merged_pb$PRICEBAND5)) + 
            geom_histogram(col = "black", fill = "light blue") +
            theme(axis.text = element_text(size = 11),
                  axis.title = element_text(size = 13, face = "bold")) +
            labs(x = "PRICEBAND5"),
          
          ggplot(bid_merged_pb, aes(bid_merged_pb$PRICEBAND6)) + 
            geom_histogram(col = "black", fill = "light blue") +
            theme(axis.text = element_text(size = 11),
                  axis.title = element_text(size = 13, face = "bold")) +
            labs(x = "PRICEBAND6"),
          
          ggplot(bid_merged_pb, aes(bid_merged_pb$PRICEBAND7)) + 
            geom_histogram(col = "black", fill = "light blue") +
            theme(axis.text = element_text(size = 11),
                  axis.title = element_text(size = 13, face = "bold")) +
            labs(x = "PRICEBAND7"),
          
          ggplot(bid_merged_pb, aes(bid_merged_pb$PRICEBAND8)) + 
            geom_histogram(col = "black", fill = "light blue") +
            theme(axis.text = element_text(size = 11),
                  axis.title = element_text(size = 13, face = "bold")) +
            labs(x = "PRICEBAND8"),
          
          ggplot(bid_merged_pb, aes(bid_merged_pb$PRICEBAND9)) + 
            geom_histogram(col = "black", fill = "light blue") +
            theme(axis.text = element_text(size = 11),
                  axis.title = element_text(size = 13, face = "bold")) +
            labs(x = "PRICEBAND9"),
          
          ggplot(bid_merged_pb, aes(bid_merged_pb$PRICEBAND10)) + 
            geom_histogram(col = "black", fill = "light blue") +
            theme(axis.text = element_text(size = 11),
                  axis.title = element_text(size = 13, face = "bold")) +
            labs(x = "PRICEBAND10"),
  ncol = 5, nrow = 2)

### ---------------------------------------- 7.7 Initial Analysis Bandavails --------------------------------------###

# Band Available Boxplot Fig 4.1.5
boxplot(bid_merged_ba$BANDAVAIL1, bid_merged_ba$BANDAVAIL2, bid_merged_ba$BANDAVAIL3, bid_merged_ba$BANDAVAIL4, 
        bid_merged_ba$BANDAVAIL5, bid_merged_ba$BANDAVAIL6, bid_merged_ba$BANDAVAIL7, bid_merged_ba$BANDAVAIL8, 
        bid_merged_ba$BANDAVAIL9, bid_merged_ba$BANDAVAIL10, 
        names = c("BA1", "BA2", "BA3", "BA4", "BA5", "BA6", "BA7", "BA8", "BA9", "BA10"),
        main = "Quantity Distribution", xlab = "BandAvails", ylab = "Quantity (mwh)")

# BANDAVAIL Boxplot (without outliers + subset 1) Fig 4.1.6
par(mfrow= c(2,2))
boxplot(bid_merged_ba$BANDAVAIL1, main = "Quantity Distribution", xlab = "BANDAVAIL1", ylab = "Quantity (mwh)", outline = FALSE)
boxplot(bid_merged_ba$BANDAVAIL8, main = "Quantity Distribution", xlab = "BANDAVAIL8", ylab = "Quantity (mwh)", outline = FALSE)
boxplot(bid_merged_ba$BANDAVAIL9, main = "Quantity Distribution", xlab = "BANDAVAIL9", ylab = "Quantity (mwh)", outline = FALSE)
boxplot(bid_merged_ba$BANDAVAIL10, main = "Quantity Distribution", xlab = "BANDAVAIL10", ylab = "Quantity (mwh)", outline = FALSE)


# BANDAVAIL Boxplot (without outliers + subset 2) Fig 4.1.7
par(mfrow= c(1,1))
boxplot(bid_merged_ba$BANDAVAIL2, bid_merged_ba$BANDAVAIL3, bid_merged_ba$BANDAVAIL4, 
        bid_merged_ba$BANDAVAIL5, bid_merged_ba$BANDAVAIL6, bid_merged_ba$BANDAVAIL7, 
        names = c("BA2", "BA3", "BA4", "BA5", "BA6", "BA7"),
        main = "Quantity Distribution", xlab = "BANDAVAILs", ylab = "Quantity (mwh)", outline = FALSE)

# Summaries of BANDAVAIL quartiles Out 4.1.5
summary(bid_merged_ba$BANDAVAIL1)
summary(bid_merged_ba$BANDAVAIL2)
summary(bid_merged_ba$BANDAVAIL3)
summary(bid_merged_ba$BANDAVAIL4)
summary(bid_merged_ba$BANDAVAIL5)
summary(bid_merged_ba$BANDAVAIL6)
summary(bid_merged_ba$BANDAVAIL7)
summary(bid_merged_ba$BANDAVAIL8)
summary(bid_merged_ba$BANDAVAIL9)
summary(bid_merged_ba$BANDAVAIL10)

# find the upper inner fence value for each BANDAVAIL
maxval <- apply(bid_merged_ba[,2:11], MARGIN = 2, 
                function(x) {quantile(x, probs = 0.75) + (IQR(x) * 1.5)})
print(maxval)

# find number of outliers in each BANDAVAIL (above upper inner fence)
maxvals <- data.frame("BA1" = 0, "BA2" = 0, "BA3" = 0, "BA4" = 0, "BA5" = 0, 
                      "BA6" = 0, "BA7" = 0, "BA8" = 0, "BA9" = 0, "BA10" = 0)
maxvals[,1] <- sum(bid_merged_ba$BANDAVAIL1 > maxval[1])
maxvals[,2] <- sum(bid_merged_ba$BANDAVAIL2 > maxval[2])
maxvals[,3] <- sum(bid_merged_ba$BANDAVAIL3 > maxval[3])
maxvals[,4] <- sum(bid_merged_ba$BANDAVAIL4 > maxval[4])
maxvals[,5] <- sum(bid_merged_ba$BANDAVAIL5 > maxval[5])
maxvals[,6] <- sum(bid_merged_ba$BANDAVAIL6 > maxval[6])
maxvals[,7] <- sum(bid_merged_ba$BANDAVAIL7 > maxval[7])
maxvals[,8] <- sum(bid_merged_ba$BANDAVAIL8 > maxval[8])
maxvals[,9] <- sum(bid_merged_ba$BANDAVAIL9 > maxval[9])
maxvals[,10] <- sum(bid_merged_ba$BANDAVAIL10 > maxval[10])

# find the lower inner fence value for each BANDAVAIL
minval <- apply(bid_merged_ba[,2:11], MARGIN = 2, 
                function(x) {quantile(x, probs = 0.25) - (IQR(x) * 1.5)})
print(minval)

# find number of outliers in each BANDAVAIL (below lower inner fence)
minvals <- data.frame("BA1" = 0, "BA2" = 0, "BA3" = 0, "BA4" = 0, "BA5" = 0, 
                      "BA6" = 0, "BA7" = 0, "BA8" = 0, "BA9" = 0, "BA10" = 0)
minvals[,1] <- sum(bid_merged_ba$BANDAVAIL1 < minval[1])
minvals[,2] <- sum(bid_merged_ba$BANDAVAIL2 < minval[2])
minvals[,3] <- sum(bid_merged_ba$BANDAVAIL3 < minval[3])
minvals[,4] <- sum(bid_merged_ba$BANDAVAIL4 < minval[4])
minvals[,5] <- sum(bid_merged_ba$BANDAVAIL5 < minval[5])
minvals[,6] <- sum(bid_merged_ba$BANDAVAIL6 < minval[6])
minvals[,7] <- sum(bid_merged_ba$BANDAVAIL7 < minval[7])
minvals[,8] <- sum(bid_merged_ba$BANDAVAIL8 < minval[8])
minvals[,9] <- sum(bid_merged_ba$BANDAVAIL9 < minval[9])
minvals[,10] <- sum(bid_merged_ba$BANDAVAIL10 < minval[10])

outliers <- data.frame(cbind(maxval, t(maxvals), minval, t(minvals)))
colnames(outliers) <- c("Upper inner fence value", "Number outliers Upper",
                        "Lower inner fence value", "Number outliers Lower")
print(outliers)
write.csv(outliers,"outliers_table_BA.csv")

# Find the Variances of each band 
var <- data.frame("BA1" = 0, "BA2" = 0, "BA3" = 0, "BA4" = 0, "BA5" = 0, 
                  "BA6" = 0, "BA7" = 0, "BA8" = 0, "BA9" = 0, "BA10" = 0)
var[,1] <- var(bid_merged_ba$BANDAVAIL1)
var[,2] <- var(bid_merged_ba$BANDAVAIL2)
var[,3] <- var(bid_merged_ba$BANDAVAIL3)
var[,4] <- var(bid_merged_ba$BANDAVAIL4)
var[,5] <- var(bid_merged_ba$BANDAVAIL5)
var[,6] <- var(bid_merged_ba$BANDAVAIL6)
var[,7] <- var(bid_merged_ba$BANDAVAIL7)
var[,8] <- var(bid_merged_ba$BANDAVAIL8)
var[,9] <- var(bid_merged_ba$BANDAVAIL9)
var[,10] <- var(bid_merged_ba$BANDAVAIL10)

# Histograms of each BANDAVAIL Fig 4.1.8
grid.arrange(ggplot(bid_merged_ba, aes(bid_merged_ba$BANDAVAIL1)) + 
               geom_histogram(col = "black", fill = "light blue") +
               theme(axis.text = element_text(size = 11),
                     axis.title = element_text(size = 13, face = "bold")) +
               labs(x = "BANDAVAIL1"),
             
             ggplot(bid_merged_ba, aes(bid_merged_ba$BANDAVAIL2)) + 
               geom_histogram(col = "black", fill = "light blue") +
               theme(axis.text = element_text(size = 11),
                     axis.title = element_text(size = 13, face = "bold")) +
               labs(x = "BANDAVAIL2"),
             
             ggplot(bid_merged_ba, aes(bid_merged_ba$BANDAVAIL3)) + 
               geom_histogram(col = "black", fill = "light blue") +
               theme(axis.text = element_text(size = 11),
                     axis.title = element_text(size = 13, face = "bold")) +
               labs(x = "BANDAVAIL3"),
             
             ggplot(bid_merged_ba, aes(bid_merged_ba$BANDAVAIL4)) + 
               geom_histogram(col = "black", fill = "light blue") +
               theme(axis.text = element_text(size = 11),
                     axis.title = element_text(size = 13, face = "bold")) +
               labs(x = "BANDAVAIL4"),
             
             ggplot(bid_merged_ba, aes(bid_merged_ba$BANDAVAIL5)) + 
               geom_histogram(col = "black", fill = "light blue") +
               theme(axis.text = element_text(size = 11),
                     axis.title = element_text(size = 13, face = "bold")) +
               labs(x = "BANDAVAIL5"),
             
             ggplot(bid_merged_ba, aes(bid_merged_ba$BANDAVAIL6)) + 
               geom_histogram(col = "black", fill = "light blue") +
               theme(axis.text = element_text(size = 11),
                     axis.title = element_text(size = 13, face = "bold")) +
               labs(x = "BANDAVAIL6"),
             
             ggplot(bid_merged_ba, aes(bid_merged_ba$BANDAVAIL7)) + 
               geom_histogram(col = "black", fill = "light blue") +
               theme(axis.text = element_text(size = 11),
                     axis.title = element_text(size = 13, face = "bold")) +
               labs(x = "BANDAVAIL7"),
             
             ggplot(bid_merged_ba, aes(bid_merged_ba$BANDAVAIL8)) + 
               geom_histogram(col = "black", fill = "light blue") +
               theme(axis.text = element_text(size = 11),
                     axis.title = element_text(size = 13, face = "bold")) +
               labs(x = "BANDAVAIL8"),
             
             ggplot(bid_merged_ba, aes(bid_merged_ba$BANDAVAIL9)) + 
               geom_histogram(col = "black", fill = "light blue") +
               theme(axis.text = element_text(size = 11),
                     axis.title = element_text(size = 13, face = "bold")) +
               labs(x = "BANDAVAIL9"),
             
             ggplot(bid_merged_ba, aes(bid_merged_ba$BANDAVAIL10)) + 
               geom_histogram(col = "black", fill = "light blue") +
               theme(axis.text = element_text(size = 11),
                     axis.title = element_text(size = 13, face = "bold")) +
               labs(x = "BANDAVAIL10"),
             ncol = 5, nrow = 2)

# Histograms but domain 0 to 500

grid.arrange(ggplot(bid_merged_ba, aes(bid_merged_ba$BANDAVAIL1)) + 
               geom_histogram(col = "black", fill = "light blue") +
               theme(axis.text = element_text(size = 11),
                     axis.title = element_text(size = 13, face = "bold")) +
               labs(x = "BANDAVAIL1") + 
               xlim(0,500),
             
             ggplot(bid_merged_ba, aes(bid_merged_ba$BANDAVAIL2)) + 
               geom_histogram(col = "black", fill = "light blue") +
               theme(axis.text = element_text(size = 11),
                     axis.title = element_text(size = 13, face = "bold")) +
               labs(x = "BANDAVAIL2") + 
               xlim(0,500),
             
             ggplot(bid_merged_ba, aes(bid_merged_ba$BANDAVAIL3)) + 
               geom_histogram(col = "black", fill = "light blue") +
               theme(axis.text = element_text(size = 11),
                     axis.title = element_text(size = 13, face = "bold")) +
               labs(x = "BANDAVAIL3") + 
               xlim(0,300),
             
             ggplot(bid_merged_ba, aes(bid_merged_ba$BANDAVAIL4)) + 
               geom_histogram(col = "black", fill = "light blue") +
               theme(axis.text = element_text(size = 11),
                     axis.title = element_text(size = 13, face = "bold")) +
               labs(x = "BANDAVAIL4") + 
               xlim(0,200), 
             
             ggplot(bid_merged_ba, aes(bid_merged_ba$BANDAVAIL5)) + 
               geom_histogram(col = "black", fill = "light blue") +
               theme(axis.text = element_text(size = 11),
                     axis.title = element_text(size = 13, face = "bold")) +
               labs(x = "BANDAVAIL5") + 
               xlim(0,200),
             
             ggplot(bid_merged_ba, aes(bid_merged_ba$BANDAVAIL6)) + 
               geom_histogram(col = "black", fill = "light blue") +
               theme(axis.text = element_text(size = 11),
                     axis.title = element_text(size = 13, face = "bold")) +
               labs(x = "BANDAVAIL6") + 
               xlim(0,200),
             
             ggplot(bid_merged_ba, aes(bid_merged_ba$BANDAVAIL7)) + 
               geom_histogram(col = "black", fill = "light blue") +
               theme(axis.text = element_text(size = 11),
                     axis.title = element_text(size = 13, face = "bold")) +
               labs(x = "BANDAVAIL7") + 
               xlim(0,200),
             
             ggplot(bid_merged_ba, aes(bid_merged_ba$BANDAVAIL8)) + 
               geom_histogram(col = "black", fill = "light blue") +
               theme(axis.text = element_text(size = 11),
                     axis.title = element_text(size = 13, face = "bold")) +
               labs(x = "BANDAVAIL8") + 
               xlim(0,200),
             
             ggplot(bid_merged_ba, aes(bid_merged_ba$BANDAVAIL9)) + 
               geom_histogram(col = "black", fill = "light blue") +
               theme(axis.text = element_text(size = 11),
                     axis.title = element_text(size = 13, face = "bold")) +
               labs(x = "BANDAVAIL9") + 
               xlim(0,200),
             
             ggplot(bid_merged_ba, aes(bid_merged_ba$BANDAVAIL10)) + 
               geom_histogram(col = "black", fill = "light blue") +
               theme(axis.text = element_text(size = 11),
                     axis.title = element_text(size = 13, face = "bold")) +
               labs(x = "BANDAVAIL10") + 
               xlim(0,500),
             
             ncol = 5, nrow = 2)


### ---------------------------------------- 7.9 Scree Plots --------------------------------------###

plot(p, type = "lines", main = "Pricebands")
sp # summary of p
plot(b, type = "lines", main = "BandAvails")
sb # summary of b

### ---------------------------------------- 7.10 Loadings Plots --------------------------------------###
# Pricebands

par(mfrow = c(2,1))

par(mar=c(8,3,2,1)) # Set margins
n.pc1_pb <- ifelse(p$rotation[,1] > 0, yes = -0.01, no = p$rotation[,1] - 0.01)
c.pc1_pb <- ifelse(p$rotation[,1] > 0, yes = "green2", no = "red2")
pc1_pb <- barplot(p$rotation[,1], main="PC 1 Loadings Plot Pricebands", 
        las = 2, col = c.pc1_pb, axisnames = FALSE)
        abline(h = 0) # Add horizontal line
text(x = pc1_pb, y = n.pc1_pb, labels = names(p$rotation[,1]), adj = 1, srt = 90, xpd = TRUE) # Add variable names

rm(pc1_pb, n.pc1_pb, c.pc1_pb)

par(mar=c(8,3,2,1)) # Set margins
n.pc2_pb <- ifelse(p$rotation[,2] > 0, yes = -0.01, no = p$rotation[,2] - 0.01)
c.pc2_pb <- ifelse(p$rotation[,2] > 0, yes = "green2", no = "red2")
pc2_pb <- barplot(p$rotation[,2], main="PC 2 Loadings Plot Pricebands", 
                  las = 2, col = c.pc2_pb, axisnames = FALSE)
abline(h = 0) # Add horizontal line
text(x = pc2_pb, y = n.pc2_pb, labels = names(p$rotation[,2]), adj = 1, srt = 90, xpd = TRUE) # Add variable names

rm(pc2_pb, n.pc2_pb, c.pc2_pb)

# Band Avail
par(mar=c(8,3,2,1)) # Set margins
n.pc1_ba <- ifelse(b$rotation[,1] > 0, yes = -0.01, no = b$rotation[,1] - 0.01)
c.pc1_ba <- ifelse(b$rotation[,1] > 0, yes = "green2", no = "red2")
pc1_ba <- barplot(b$rotation[,1], main = "PC 1 Loadings Plot BandAvail", 
                  las = 2, col = c.pc1_ba, axisnames = FALSE)
abline(h=0) # Add horizontal line
text(x = pc1_ba, y = n.pc1_ba, labels = names(b$rotation[,1]), adj = 1, srt = 90, xpd = TRUE) # Add variable names

rm(pc1_ba, n.pc1_ba, c.pc1_ba)

par(mar=c(8,3,2,1)) # Set margins
n.pc2_ba <- ifelse(b$rotation[,2] > 0, yes = -0.01, no = b$rotation[,2] - 0.01)
c.pc2_ba <- ifelse(b$rotation[,2] > 0, yes = "green2", no = "red2")
pc2_ba <- barplot(b$rotation[,2], main="PC 2 Loadings Plot BandAvail", 
                  las = 2, col = c.pc2_ba, axisnames = FALSE)
abline(h = 0) # Add horizontal line
text(x = pc2_ba, y = n.pc2_ba, labels = names(b$rotation[,2]), adj = 1, srt = 90, xpd = TRUE) # Add variable names

rm(pc2_ba, n.pc2_ba, c.pc2_ba)


