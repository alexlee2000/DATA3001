library(dplyr) 
library(readxl) 
library(lubridate) #for dates 

# -------------------------------- Using biddayoffer and bidperofffer ----------------------- #
# We are importing the data provided to us, dropping irrelevant variables and merging the     #
# 2 tables into 1 by DUID and settlement date. We then partially clean the data for the       #
# variables Priceband and Bandavail. I also obtained an external source for region and        #
# fuel_type information. NOTE *the sql stuff is going to take some time to run.               #
# ------------------------------------------------------------------------------------------- #

### -------------------------------------------------- 7.1 Initial Steps ---------------------------------------------------###

# Reading in initial data provided from client
biddayoffer <- read.csv("~/Desktop/biddayoffer_fy2009-2019.csv")
bidperoffer <- read.csv("~/Desktop/bidperoffer_fy2009-2019_v2.csv")

# Filtering for ENERGY bidtype only
biddayoffer <- filter(biddayoffer, biddayoffer$BIDTYPE == "ENERGY")
bidperoffer <- filter(bidperoffer, bidperoffer$BIDTYPE == "ENERGY")

# Dropping irrelevant variables from biddayoffer and bidperoffer
biddayoffer_final <- select(biddayoffer, c("DUID", "SETTLEMENTDATE", "OFFERDATE", "VERSIONNO", "REBIDEXPLANATION", 
                                           "ENTRYTYPE", "PRICEBAND1", "PRICEBAND2", "PRICEBAND3", "PRICEBAND4", 
                                           "PRICEBAND5", "PRICEBAND6", "PRICEBAND7", "PRICEBAND8", "PRICEBAND9", 
                                           "PRICEBAND10"))

bidperoffer_final <- select(bidperoffer, c("DUID", "SETTLEMENTDATE", "INTERVAL_DATETIME", "OFFERDATE", "VERSIONNO", 
                                           "PERIODID", "PASAAVAILABILITY", "MAXAVAIL", "BANDAVAIL1", "BANDAVAIL2", 
                                           "BANDAVAIL3", "BANDAVAIL4", "BANDAVAIL5", "BANDAVAIL6", "BANDAVAIL7", 
                                           "BANDAVAIL8", "BANDAVAIL9", "BANDAVAIL10"))
                               
# Merging the two bid tables 
bid_merged <- merge(biddayoffer_final, bidperoffer_final, 
                    by.x = c("DUID", "SETTLEMENTDATE", "VERSIONNO", "OFFERDATE"), 
                    by.y = c("DUID", "SETTLEMENTDATE", "VERSIONNO","OFFERDATE"))

# Re-formatting the variable INTERVAL_DATETIME and creating new time variables 
bid_merged$INTERVAL_DATETIME <- as.POSIXct(strptime(bid_merged$INTERVAL_DATETIME, "%Y-%m-%d %H:%M:%S"))
bid_merged$Year <- format(bid_merged$INTERVAL_DATETIME, format="%Y") 
bid_merged$Month <- format(bid_merged$INTERVAL_DATETIME, format="%m") # 01, ..., 12
bid_merged$Month_name <- format(bid_merged$INTERVAL_DATETIME, format="%B") # January, ..., December
bid_merged$Day_of_Month <- format(bid_merged$INTERVAL_DATETIME, format="%d") # 01, ..., 31
bid_merged$Day_of_Year <- format(bid_merged$INTERVAL_DATETIME, format="%j") # 001, ..., 366
bid_merged$Day_name <- format(bid_merged$INTERVAL_DATETIME, format="%A") # Monday, ... , Sunday
bid_merged$Week <- format(bid_merged$INTERVAL_DATETIME, format="%W") #0, ..., 53 (with Monday as first day of week)
bid_merged$Hour <- format(bid_merged$INTERVAL_DATETIME, format="%H") 
bid_merged$Minute <- format(bid_merged$INTERVAL_DATETIME, format="%M") 

# Reading in AEMO Generator List obtained online 
AEMO_Generator_List <- read_excel("Desktop/AEMO_Generator_List.xlsx", col_types = c("text", "text", "numeric", 
                                                                                    "text", "text"))
AEMO_Generator_List <- select(AEMO_Generator_List, c("DUID", "Region", "Fuel_Type"))

# Adding in 2 new variables Region and Fuel Type to bid_merged (note that there are some missing values in the cols)
bid_merged <- left_join(bid_merged, AEMO_Generator_List)
bid_merged <- distinct(bid_merged)

# Removing irrelevant tables from your global environment  
rm(bidperoffer, biddayoffer, bidperoffer_final, biddayoffer_final, AEMO_Generator_List)

### -------------------------------------------------- 7.2 Data Cleaning ---------------------------------------------------###

# Checks to see if there is any missing data 
if(sum(is.na(bid_merged)) != 0) {
  bid_merged <- bid_merged[!is.na(bid_merged$INTERVAL_DATETIME), ] # Removing NA rows 
} 

# making sure that we don't have any duplicate rows 
bid_merged <- distinct(bid_merged) 

### -------------------------------------------------- 7.3 Assumptions ---------------------------------------------------###


### ------------------------------------------------------ 7.4 PCA Setup -------------------------------------------------------###
p <- prcomp(bid_merged[,c(7:16)], scale=TRUE) # Priceband (7:16) PCA
b <- prcomp(bid_merged[,c(21:30)], scale=TRUE) # Bandavail (21:30) PCA

sp <- summary(p)
sb <- summary(b)

# MAKE SURE TO ONLY RUN THE NEXT 4 LINES ONCE 
bid_merged_pb <- bid_merged[,c(1,7:16, 17, 40, 41)] # Selecting DUID (1) and pricebands (7:16) and INTERVAL_DATETIME (17) and Fuel_Type (40) + Region (41)
bid_merged_ba <- bid_merged[,c(1,21:30, 17, 40, 41)] # Selecting DUID (1) and bandavails (21:30) and INTERVAL_DATETIME (17) and Fuel_TYpe (40) + Region (41)
bid_merged_pb <- cbind(bid_merged_pb, p$x[,1:2]) # Appending the PCA coordinates for PC1 (1) and PC2 (2) to bid_merged_pb
bid_merged_ba <- cbind(bid_merged_ba, b$x[,1:2]) # Appending the PCA coordinates for PC1 (1) and PC2 (2) to bid_merged_pb

# Percentiles Code
bid_merged_percentiles <- read_csv("Desktop/bid_merged_percentiles.csv")

bid_merged_percentiles <- bid_merged_percentiles[,c(3 ,19, 33:47)]

percentiles.pca <- prcomp(bid_merged_percentiles[,14:17], scale = TRUE)
summary_percentiles <- summary(percentiles.pca)

bid_merged_percentiles <- cbind(bid_merged_percentiles, percentiles.pca$x[,1:2])

### ----------------------------------------------- 7.5 Year Colours ------------------------------------------------###
# We want to add colours to bid_merged_pb and bid_merged_ba by their years (2008 ~ 2019) 
colVec <- rainbow(12) # a vector of 12 colours generated
pch.group <- c(rep(21, times = nrow(bid_merged)))

col.group <- ifelse(bid_merged$Year==2008, colVec[1], 
             ifelse(bid_merged$Year==2009, colVec[2],
             ifelse(bid_merged$Year==2010, colVec[3],
             ifelse(bid_merged$Year==2011, colVec[4],
             ifelse(bid_merged$Year==2012, colVec[5],
             ifelse(bid_merged$Year==2013, colVec[6],
             ifelse(bid_merged$Year==2014, colVec[7],
             ifelse(bid_merged$Year==2015, colVec[8],
             ifelse(bid_merged$Year==2016, colVec[9],
             ifelse(bid_merged$Year==2017, colVec[10],
             ifelse(bid_merged$Year==2018, colVec[11],
             ifelse(bid_merged$Year==2019, colVec[12],
             "NO COL"
             ))))))))))))
### ------------------------------------------------ 7.8 Diagnostic for PCA Plot  -------------------------------------------------###
# Create a new row that is the average of all observations. This point have PCA coordinates approximately at origin (0,0)
test_row <- data.frame("TEST", "2000-01-01 00:00:00", 1, "2000-01-01 00:00:00", "TEST", "TEST", 
                       mean(bid_merged$PRICEBAND1), mean(bid_merged$PRICEBAND2), mean(bid_merged$PRICEBAND3),
                       mean(bid_merged$PRICEBAND4), mean(bid_merged$PRICEBAND5), mean(bid_merged$PRICEBAND6), 
                       mean(bid_merged$PRICEBAND7), mean(bid_merged$PRICEBAND8), mean(bid_merged$PRICEBAND9), 
                       mean(bid_merged$PRICEBAND10), 
                       "2000-01-01 00:00:00", 1, 1, 1, 
                       mean(bid_merged$BANDAVAIL1), mean(bid_merged$BANDAVAIL2), mean(bid_merged$BANDAVAIL3),
                       mean(bid_merged$BANDAVAIL4), mean(bid_merged$BANDAVAIL5), mean(bid_merged$BANDAVAIL6), 
                       mean(bid_merged$BANDAVAIL7), mean(bid_merged$BANDAVAIL8), mean(bid_merged$BANDAVAIL9), 
                       mean(bid_merged$BANDAVAIL10), 
                       2000, 1, "TEST", 1, 1, "TEST", 1, 1, 1, "TEST", "TEST")  

names(test_row) <- c("DUID", "SETTLEMENTDATE", "VERSIONNO", "OFFERDATE", "REBIDEXPLANATION", "ENTRYTYPE", 
                     "PRICEBAND1", "PRICEBAND2", "PRICEBAND3", "PRICEBAND4", "PRICEBAND5", "PRICEBAND6",
                     "PRICEBAND7", "PRICEBAND8","PRICEBAND9", "PRICEBAND10", "INTERVAL_DATETIME", "PERIODID",
                     "PASAAVAILABILITY", "MAXAVAIL", "BANDAVAIL1", "BANDAVAIL2", "BANDAVAIL3", "BANDAVAIL4", 
                     "BANDAVAIL5", "BANDAVAIL6", "BANDAVAIL7", "BANDAVAIL8","BANDAVAIL9", "BANDAVAIL10",
                     "Year", "Month", "Month_name", "Day_of_Month", "Day_of_Year", "Day_name", "Week", "Hour",
                     "Minute", "Region", "Fuel_Type")  

test_df <- rbind(bid_merged, test_row) #Using rbind() function to insert above observation  

p_test <- prcomp(test_df[,c(7:16)], scale=TRUE)
bid_merged_pb_test <- test_df[,c(1, 7:16, 17, 40, 41)] # Selecting DUID (1) and pricebands (7:16) and Fuel_Type (40) + Region (41)
bid_merged_pb_test <- cbind(bid_merged_pb, p_test$x[,1:2]) # Appending the PCA coordinates for PC1 (1) and PC2 (2) to bid_merged_pb
tail(bid_merged_pb_test, 1) # This row should have PC1 ~ 0 and PC2 ~ 0
rm(p_test)

b_test <- prcomp(test_df[,c(21:30)], scale=TRUE)
bid_merged_ba_test <- test_df[,c(1, 21:30, 17, 40, 41)] # Selecting DUID (1) and bandavails (21:30) and Fuel_TYpe (40) + Region (41)
bid_merged_ba_test <- cbind(bid_merged_ba, b_test$x[,1:2]) # Appending the PCA coordinates for PC1 (1) and PC2 (2) to bid_merged_pb
tail(bid_merged_ba_test, 1) # This row should have PC1 ~ 0 and PC2 ~ 0
rm(b_test)

rm(test_df, test_row, bid_merged_ba_test, bid_merged_pb_test)



