rm(list = ls())
setwd("~/Google Drive/UofR/R/Assignment 1")

# Part 1 - Data Cleaning

# a) loading the consumer purchases data set
consumerDataFrame <- read.csv('consumerDataFrame.csv')

# b) renaming the columns

# see the current names
names(consumerDataFrame)

# data cleaning

# removing unnecessary columns
consumerDataFrame$X <- NULL

# renaming columns
names(consumerDataFrame) [4] <- "storeID"
names(consumerDataFrame) [5] <- "panelistID"
names(consumerDataFrame) [6] <- "flavorID"

# c) merging the datasets

# preparation
# loading the product descriptions data set
itemDataFrame <- read.csv('itemDataFrame.csv')

# renaming  columns
itemDataFrame$X <- NULL
names(itemDataFrame) [3] <- 'privateLabel'
names(itemDataFrame) [4] <- 'flavorName'
itemDataFrame[itemDataFrame$flavorName != "missing", 5] # removing "missing"
# gsub('[[:digit:]]+', '', itemDataFrame$flavorName) # removing numbers from food names
names(itemDataFrame) [5] <- 'isTomato'
names(itemDataFrame) [6] <- 'flavorID'

# merging data using merge
df1 <- consumerDataFrame
df2 <- itemDataFrame

productsPurchased <- merge(df1, df2)

# merging data using for loop, but i prefer to use the MERGE function
# productsPurchased2 <- as.data.frame(matrix(0,nrow = 308135,ncol = 6))
# metaDataFrame <- cbind(consumerDataFrame, productsPurchased2)
# for(i in 1:nrow(consumerDataFrame))
#{
# productsPurchased <- itemDataFrame[itemDataFrame$flavorID == consumerDataFrame$flavorID[i],]
#  metaDataFrame[i,8:13] <- productsPurchased2[-7]
#}

# d) creating two new variables

# price per unit of purchase

pricePerUnit <- productsPurchased$dollars/productsPurchased$units
head(pricePerUnit) # check
tail(pricePerUnit) # check
productsPurchased$pricePerUnit <- pricePerUnit

# total volume purchased

totalVolume <- productsPurchased$units * productsPurchased$volumePerUnit
head(totalVolume) # check
tail(totalVolume) # check
productsPurchased$totalVolume <- totalVolume

# e) save the merged database
# Save data frame to r data file
save(productsPurchased, file="finalDB.Rdata")


# f) descriptive statistics
summary(productsPurchased)

# means, omit missing values
mean(productsPurchased$units, na.rm = TRUE) # 1.643546
mean(productsPurchased$dollars, na.rm = TRUE) # 2.010288

# stdev
sd(productsPurchased$units, na.rm = TRUE) # 1.159806; units bought on average were more than 1.15 above the mean
sd(productsPurchased$dollars, na.rm = TRUE) # 1.497659; dollars spent on average were more than 1.49 above the mean

# market share 
mktShareStore <- aggregate(units ~ storeID, data = productsPurchased, FUN = sum)
mktShareStore$marketShare <- mktShareStore$units/sum(mktShareStore$units)
mktShareStore # market share breakdown of the stores based on units

# market share of the top ten flavors (flavorName)
mktShareFlavor <- aggregate(units~flavorName, data = productsPurchased, FUN = sum)
mktShareFlavor <- mktShareFlavor[order(-mktShareFlavor$units),]
mktShareFlavorTop10 <- mktShareFlavor[c(1:11),] # need to disregard "missing"
mktShareFlavorTop10$marketShare <- mktShareFlavorTop10$units / sum(mktShareFlavor$units)
mktShareFlavorTop10

# market share of privateLabel brands
mktSharePrivateLabel <- aggregate(units ~ privateLabel, data = productsPurchased, FUN = sum)
mktSharePrivateLabel$marketshare <- mktSharePrivateLabel$units/sum(mktSharePrivateLabel$units)
mktSharePrivateLabel # 0.130011 market share

# Part 2 - Metrics of Price Sensitivity

# 2a)
consumerData <- aggregate(. ~ panelistID, data = productsPurchased, sum)
avgPurchasePriceByConsumer <- data.frame(consumerData$panelistID,consumerData$dollars/consumerData$units, consumerData$units)
colnames(avgPurchasePriceByConsumer) <- c("panelistID", "avgPrice", "units")
head(avgPurchasePriceByConsumer)

# 2b)
volPerUnitByConsumer <- data.frame(consumerData$panelistID, consumerData$volumePerUnit, consumerData$units)
colnames(volPerUnitByConsumer) <- c("panelistID", "volPerUnit", "units")
head(volPerUnitByConsumer)

# 2c)
library(ggplot2)
df3 <- data.frame(avgPurchasePriceByConsumer$avgPrice, volPerUnitByConsumer$volPerUnit)
head(df3)
ggplot(df3, aes(avgPurchasePriceByConsumer$avgPrice, volPerUnitByConsumer$volPerUnit)) + geom_point() + ggtitle("Average Purchase Price by Consumers vs Volume Per Unit Purchased by Consumers")
fit <- lm(volPerUnitByConsumer$volPerUnit ~ avgPurchasePriceByConsumer$avgPrice, data = df3) 
fit
cor(avgPurchasePriceByConsumer$avgPrice, volPerUnitByConsumer$volPerUnit)
# 0.12

# 2f)
head(volPerUnitByConsumer)
top100 <- sort(volPerUnitByConsumer$volPerUnit, decreasing = F)[1:100]