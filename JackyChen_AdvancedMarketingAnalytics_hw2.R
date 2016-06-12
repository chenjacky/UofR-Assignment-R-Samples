###############################################################################
# Part A: Data Cleaning and Discrete Choice
###############################################################################

############################# Question 1 ###################################
rm(list=ls())
setwd("~/Google Drive/UofR/R/Advanced MA/Homework 2")

dataUnits <- read.csv("HW2 - Units.csv",check.names=FALSE)
dataPrices <- read.csv("HW2 - Prices.csv",check.names=FALSE)

names(dataUnits)[1] <- "consumerID"
names(dataPrices)[1] <- "consumerID"

library(reshape2)


mdataUnits <- melt(dataUnits, id.vars = c("consumerID"), variable.name = "weekNum", value.name = "units")
mdataPrices <- melt(dataPrices, id.vars = c("consumerID"), variable.name = "weekNum", value.name = "prices")

rowCount <- nrow(mdataUnits)

mat <- matrix(NA, rowCount, 5)

# consumerID
mat[, 1] <- mdataUnits$consumerID

# weekNum
mat[, 2] <- as.numeric(as.character(mdataUnits$weekNum))

# pricePerUnit
mat[, 3] <- mdataPrices$prices

# units
mat[, 4] <- mdataUnits$units

# isPurchase
mat[, 5] <- ifelse(mdataUnits$units == 0, 0, 1)

q1DB <- data.frame(mat)
colnames(q1DB) <- c("consumerID","weekNum", "pricePerUnit","units","isPurchase")

############################## Question 2 - PDF #############################

############################## Question 3 ##############################
# Part 3: Using LM and GLM, run and store the following models, 
# predicting isPurchase with pricePerUnit:

# A) A Linear Probability Model
linearModel <- lm(isPurchase ~ pricePerUnit, data = q1DB)
summary(linearModel)

# B) A Binary Logit Model
logitModel <- glm(isPurchase ~ pricePerUnit, data = q1DB, family = binomial(link = logit))
summary(logitModel)

# C) A Binary Probit Model
probitModel <- glm(isPurchase ~ pricePerUnit, data = q1DB, family = binomial(link= probit))
summary(probitModel)


# setup (for question 5 as well)
newData <- data.frame(matrix(seq(-5, 5.1, 0.1),nrow = 102, ncol = 1))
names(newData) [1] = 'pricePerUnit'
pricePerUnit <- newData$pricePerUnit

# Linear Probability Model Prediction
newData$linearModelPred <- predict(linearModel, newData, type = "response")
linearModelPred <- newData$linearModelPred
head(linearModelPred)

# Binary Logit Model Prediction
newData$logitModelPred <- predict.glm(logitModel, newData, type = "response")
logitModelPred <- newData$logitModelPred
head(logitModelPred)

# Binary Probit Model Prediction
newData$probitModelPred <- predict.glm(probitModel, newData, type = "response")
probitModelPred <- newData$probitModelPred
head(probitModelPred)

plot(pricePerUnit, linearModelPred, pch = 1, cex = 1, col = "red", main = "Linear, Logit, Probit Models",
     xlab = "Price per Unit", ylab = "Probability of a Skim Milk Purchase")
points(pricePerUnit, logitModelPred, pch = 2, cex = 1, col = "green", main = "Logit Model", 
       xlab = "Price per Unit", ylab = "Probability of a Skim Milk Purchase")
points(pricePerUnit, probitModelPred, pch = 3, cex = 1, col = "blue", main = "Probit Model", 
       xlab = "Price per Unit", ylab = "Probability of a Skim Milk Purchase")

############################## Question 4 - PDF ############################ 

############################## Question 5 #################################
# a) $1 - $1.10
subset(newData, newData$pricePerUnit == "1")
subset(newData, newData$pricePerUnit == "1.1")

# b) $3 - $3.10
subset(newData, newData$pricePerUnit == "3")
subset(newData, newData$pricePerUnit == "3.1")

# c) $5 - $5.10
subset(newData, newData$pricePerUnit == "5")
subset(newData, newData$pricePerUnit == "5.1")

##############################  Question 6 ################################# 
dataUnitsQA6 <- read.csv("HW2 - QA6 - Units.csv", check.names = FALSE)
dataPricesQA6 <- read.csv("HW2 - QA6 - Prices.csv", check.names = FALSE)

names(dataUnitsQA6)[1] <- "consumerID"
names(dataPricesQA6)[1] <- "consumerID"

mdataUnitsQA6 <- melt(dataUnitsQA6, id.vars=c("consumerID"), variable.name = "weekNum", value.name = "units")
mdataPricesQA6 <- melt(dataPricesQA6, id.vars=c("consumerID"), variable.name = "weekNum", value.name = "prices")

rowCountQA6 <- nrow(mdataUnitsQA6)

matQA6 <- matrix(NA, rowCountQA6, 6)

# panelID
matQA6[, 1] <- c(1:rowCountQA6)

# weekNum
matQA6[, 2] <- as.numeric(as.character(mdataUnitsQA6$weekNum))

# consumerID
matQA6[, 3] <- mdataUnitsQA6$consumerID

# pricePerUnit
matQA6[, 4] <- mdataPricesQA6$prices

# units
matQA6[, 5] <- mdataUnitsQA6$units

# isPurchase
matQA6[, 6] <- ifelse(mdataUnitsQA6$units==0, 0, 1)

qA6DB <- data.frame(matQA6)
colnames(qA6DB) = c("panelID","weekNum","consumerID","pricePerUnit","units","isPurchase")

# Linear Probability Model
linearModel2 <- lm(isPurchase ~ pricePerUnit, data = qA6DB)
summary(linearModel2)

# Binary Logit Model
logitModel2 <- glm(isPurchase ~ pricePerUnit, data = qA6DB, family = binomial(link = logit))
summary(logitModel2)

# Binary Probit Model
probitModel2 <- glm(isPurchase ~ pricePerUnit, data = qA6DB, family = binomial(link= probit))
summary(probitModel2)


############################## Question 7 #################################
newDataUsing5 <- data.frame(matrix(5, nrow = 1, ncol = 1)) 
names(newDataUsing5) [1] = 'pricePerUnit'

# using first data set (100 consumers)

newDataUsing5$linearModelPred1 <- predict(linearModel, newDataUsing5, type = "response")
linearModelPred1 <- newDataUsing5$linearModelPred1

newDataUsing5$logitModelPred1 <- predict(logitModel, newDataUsing5, type = "response")
logitModelPred1 <- newDataUsing5$logitModelPred1

newDataUsing5$probitModelPred1 <- predict(probitModel, newDataUsing5, type = "response")
probitModelPred1 <- newDataUsing5$probitModelPred1 

# using second data set (200 consumers)
newDataUsing5$linearModelPred2 <- predict(linearModel2, newDataUsing5, type = "response")
linearModelPred2 <- newDataUsing5$linearModelPred2
summary(linearModelPred2)

newDataUsing5$logitModelPred2 <- predict.glm(logitModel2, newDataUsing5, type = "response")
logitModelPred2 <- newDataUsing5$logitModelPred2
summary(logitModelPred2)

newDataUsing5$probitModelPred2 <- predict.glm(probitModel2, newDataUsing5, type = "response")
probitModelPred2 <- newDataUsing5$probitModelPred2
summary(probitModelPred2)

# Demand Prediction
linearModel1Demand <- print(100 * linearModelPred1) # 1.943378
logitModel1Demand <- print(100 * logitModelPred1) # 7.356906
probitModel1Demand <- print(100 * probitModelPred1) # 6.764691
  
linearModel2Demand <- print(200 * linearModelPred2) # 1.943378
logitModel2Demand <- print(200 * logitModelPred2) # 7.706914
probitModel2Demand <- print(200 * probitModelPred2) # 7.162821

differenceLinear <- print(1.943378 - 1.943378) # 0
differenceLogit <- print(7.356906 - 7.706914) # -0.350008
differenceProbit <- print(6.764691 - 7.162821) # -0.39813

###############################################################################
# Part B: Heterogeneity
###############################################################################

##############################  Question 1 ################################# 
library(plm)
withinModel <- plm(units ~ pricePerUnit, data = q1DB, model = "within")
summary(withinModel)

randomModel <- plm(units ~ pricePerUnit, data = q1DB, model="random")
summary(randomModel)

differModel <- plm(units ~ pricePerUnit, data = q1DB, model = "fd")
summary(differModel)

poolingModel <- plm(units ~ pricePerUnit, data=q1DB, model = "pooling")
summary(poolingModel)

############################## Question 2 ################################# 
altWithinModel <- lm(units ~ pricePerUnit + factor(consumerID), data = q1DB)
summary(altWithinModel)

altPoolingModel <- lm(units ~ pricePerUnit, data = q1DB)    
summary(altPoolingModel)

############################## Question 3 ################################# 
AIC(altWithinModel) # 65390.89
AIC(altPoolingModel) # 67727.86

############################## Question 4 ################################# 
consumerPurchased <- subset(q1DB, q1DB$units > 0) 
withinModel2 <- plm(units ~ pricePerUnit, data = consumerPurchased, model = "within")
summary(withinModel2)

############################## Question 5 ################################# 
# consumerTotalPurchase
consumerTotalPurchase <- aggregate(units ~ consumerID, data = q1DB, sum)

# comparing fixed effects from within model
fixedEffects <- as.data.frame(summary(altWithinModel)$coefficients[3:101, 1])
names(fixedEffects) <- c('Consumer Effect')

# consumer coefficients 
consumer <- as.data.frame(0,row.names = "factor(consumerID)")
names(consumer) <- c('Consumer Effect')
fixedEffects <- rbind(fixedEffects, consumer)
fixedEffects <- fixedEffects[order(fixedEffects$`Consumer Effect`), ]

###############################################################################
# Part C: Consumer Types
###############################################################################

############################## Question 1 ################################# 
totalPurchaseHistory <- aggregate(q1DB$units, list(q1DB$consumerID), sum)
colnames(totalPurchaseHistory) <- c('consumerID', 'units')
consumerRanking <- totalPurchaseHistory[order(totalPurchaseHistory$units,decreasing = T),]

# Separating into two consumer groups
top50 <- consumerRanking[1:50, ]
bottom50 <- consumerRanking[51:100, ]
consumerGroup1 <- q1DB[q1DB$consumerID %in% top50$consumerID, ]
consumerGroup2 <- q1DB[q1DB$consumerID %in% bottom50$consumerID, ]

consumerGroup1Model <- glm(isPurchase ~ pricePerUnit, family = binomial(link = logit), data = consumerGroup1)
summary(consumerGroup1Model) # coefficient estimate: -0.23463
consumerGroup2Model <- glm(isPurchase ~ pricePerUnit, family = binomial(link = logit), data = consumerGroup2)
summary(consumerGroup2Model) # coefficient estimate: -1.03036

############################## Question 2 - PDF ################################# 

############################## Question 3 ################################# 

# Calculating AIC

# Basic 1 Segment
AllConsumers <- consumerRanking[1:100, ]
AllConsumerGroup <- q1DB[q1DB$consumerID %in% AllConsumers$consumerID, ]
logitModelOneSeg <- glm(isPurchase ~ pricePerUnit, data = AllConsumerGroup, family = binomial(link = logit))

AIC(logitModelOneSeg) # 37012.04

# 2 Segments
AIC(consumerGroup1Model) # 21030.12
AIC(consumerGroup2Model) # 15119.84
21030.12 + 15119.83

# 5 Segments
consumerGroup1 <- consumerRanking[1:20, ]
consumerGroup2 <- consumerRanking[21:40, ]
consumerGroup3 <- consumerRanking[41:60, ]
consumerGroup4 <- consumerRanking[61:80, ]
consumerGroup5 <- consumerRanking[81:100, ]

segment1 <- q1DB[q1DB$consumerID %in% consumerGroup1$consumerID, ]
segment2 <- q1DB[q1DB$consumerID %in% consumerGroup2$consumerID, ]
segment3 <- q1DB[q1DB$consumerID %in% consumerGroup3$consumerID, ]
segment4 <- q1DB[q1DB$consumerID %in% consumerGroup4$consumerID, ]
segment5 <- q1DB[q1DB$consumerID %in% consumerGroup5$consumerID, ]

model1 <- glm(isPurchase ~ pricePerUnit, data = segment1, family = binomial(link = logit))
model2 <- glm(isPurchase ~ pricePerUnit, data = segment2, family = binomial(link = logit))
model3 <- glm(isPurchase ~ pricePerUnit, data = segment3, family = binomial(link = logit))
model4 <- glm(isPurchase ~ pricePerUnit, data = segment4, family = binomial(link = logit))
model5 <- glm(isPurchase ~ pricePerUnit, data = segment5, family = binomial(link = logit))

AIC(model1) # 9345.017
AIC(model2) # 7659.413
AIC(model3) # 7110.944
AIC(model4) # 6087.013
AIC(model5) # 5452.801
9345.017 + 7659.413 + 7110.944 + 6087.013 + 5452.801

# 10 Segments
consumerGroupV1 <- consumerRanking[1:10, ]
consumerGroupV2 <- consumerRanking[11:20, ]
consumerGroupV3 <- consumerRanking[21:30, ]
consumerGroupV4 <- consumerRanking[31:40, ]
consumerGroupV5 <- consumerRanking[41:50, ]
consumerGroupV6 <- consumerRanking[51:60, ]
consumerGroupV7 <- consumerRanking[61:70, ]
consumerGroupV8 <- consumerRanking[71:80, ]
consumerGroupV9 <- consumerRanking[81:90, ]
consumerGroupV10 <- consumerRanking[91:100, ]

segmentV1 <- q1DB[q1DB$consumerID %in% consumerGroupV1$consumerID, ]
segmentV2 <- q1DB[q1DB$consumerID %in% consumerGroupV2$consumerID, ]
segmentV3 <- q1DB[q1DB$consumerID %in% consumerGroupV3$consumerID, ]
segmentV4 <- q1DB[q1DB$consumerID %in% consumerGroupV4$consumerID, ]
segmentV5 <- q1DB[q1DB$consumerID %in% consumerGroupV5$consumerID, ]
segmentV6 <- q1DB[q1DB$consumerID %in% consumerGroupV6$consumerID, ]
segmentV7 <- q1DB[q1DB$consumerID %in% consumerGroupV7$consumerID, ]
segmentV8 <- q1DB[q1DB$consumerID %in% consumerGroupV8$consumerID, ]
segmentV9 <- q1DB[q1DB$consumerID %in% consumerGroupV9$consumerID, ]
segmentV10 <- q1DB[q1DB$consumerID %in% consumerGroupV10$consumerID, ]

modelV1 <- glm(isPurchase ~ pricePerUnit, data = segmentV1, family = binomial(link=logit))
modelV2 <- glm(isPurchase ~ pricePerUnit, data = segmentV2, family = binomial(link=logit))
modelV3 <- glm(isPurchase ~ pricePerUnit, data = segmentV3, family = binomial(link=logit))
modelV4 <- glm(isPurchase ~ pricePerUnit, data = segmentV4, family = binomial(link=logit))
modelV5 <- glm(isPurchase ~ pricePerUnit, data = segmentV5, family = binomial(link=logit))
modelV6 <- glm(isPurchase ~ pricePerUnit, data = segmentV6, family = binomial(link=logit))
modelV7 <- glm(isPurchase ~ pricePerUnit, data = segmentV7, family = binomial(link=logit))
modelV8 <- glm(isPurchase ~ pricePerUnit, data = segmentV8, family = binomial(link=logit))
modelV9 <- glm(isPurchase ~ pricePerUnit, data = segmentV9, family = binomial(link=logit))
modelV10 <- glm(isPurchase ~ pricePerUnit, data = segmentV10, family = binomial(link=logit))

AIC(modelV1) # 4870.927
AIC(modelV2) # 4357.022
AIC(modelV3) # 3943.065
AIC(modelV4) # 3713.413
AIC(modelV5) # 3610.393
AIC(modelV6) # 3498.963
AIC(modelV7) # 3076.328
AIC(modelV8) # 3013.696
AIC(modelV9) # 2884.102
AIC(modelV10) # 2561.907

4870.927 + 4357.022 + 3943.065 + 3713.413 + 3610.393 + 3498.963 + 3076.328 + 3013.696+ 2884.102 + 2561.907 # 32516.12

# saving the workspace
save.image("submit.Rdata")

