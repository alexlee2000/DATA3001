
merged_clean <- read.csv("C:/Users/pc-user/Desktop/Work/DATA3001/bid_merged.csv", header = TRUE)

# Price is y-axis, Quantity is y-axis. 
# New variable is 10th, 20th... percentile of quantity offered.

# What percentage of its output is offered at different levels
# Instead of pricebands 1-10, look at percentiles 1-10 of available generation



# PRICEBAND 1-10 in names(
PRICEBANDS = merged_clean[,8:17]
BANDAVAILS = merged_clean[,22:31]
PERCENTILES = data.frame(matrix(0,ncol=4, nrow = nrow(merged_clean)))
CUMULATIVE = data.frame(matrix(ncol=10, nrow = nrow(merged_clean)))

CUMULATIVE[1] = BANDAVAILS[1]
for (i in 2:10){
    CUMULATIVE[i] = CUMULATIVE[i-1] + BANDAVAILS[i]
}

# Iterate over 10 Percentiles
# TODO change to 5 percentiles, dont calculate last one again. 
for (x in 1:nrow(merged_clean)){
    #print(x)
    sumAgain = FALSE
    i = 1
    j = 1
    ith_percentile_quant = as.double(merged_clean$MAXAVAIL[x]/4)
    # For i percentile values and j bands
    while (i < 5 & j < 11){
        # Only iterate if we move to the next band
        if (sumAgain == TRUE) j = j + 1
        if (CUMULATIVE[x,j] >= ith_percentile_quant){
            PERCENTILES[x,i] = PRICEBANDS[x,j]
            sumAgain = FALSE
            i = i + 1
            ith_percentile_quant = ith_percentile_quant / (i-1) * i
        }
        else {
            sumAgain = TRUE
        } 
    
    }
}

write.csv(PERCENTILES, "percentiles.csv")

merged_clean$PERCENTILE25 = PERCENTILES[,1]
merged_clean$PERCENTILE50 = PERCENTILES[,2]
merged_clean$PERCENTILE75 = PERCENTILES[,3]
merged_clean$PERCENTILE100 = PERCENTILES[,4]

write.csv(merged_clean, "bid_merged_percentiles.csv")


