rm(list = ls())
setwd("~/Google Drive/UofR/R/Advanced MA/Homework 1")

###############################################################################
# Question 1: Demand Model
 
# a) load data file
data <- read.csv('Homework 1 Data.csv')
data$X <- NULL

# b) calculate price per unit in each week and store in data frame
data$pricePerUnit <- data$dollars/data$units

# c) working with price, weekNum, weekOfYear, poly to predict log(units)
model1 <- lm(log(units) ~ pricePerUnit, data = data)
model2 <- lm(log(units) ~ weekNum, data = data)
model3 <- lm(log(units) ~ weekOfYear, data = data)
model4 <- lm(log(units) ~ poly(pricePerUnit), data = data)
model5 <- lm(log(units) ~ pricePerUnit + weekNum + weekOfYear, data = data)

# d) unvariate linear regression
simpleDemandModel <- model1
simpleDemandModel

###############################################################################
# Question 2: Profit Maximization

# a) creating new data frame with single vector named pricePerUnit
# sequences all values from 0 to 2 in increments of 0.01
newData <- data.frame(matrix(seq(0, 2, 0.01), nrow = 201, ncol = 1))
names(newData) [1] = 'pricePerUnit'

# b) estimate demand for each potential price using simpleDemandModel
newData$estDemand <- predict(simpleDemandModel, newData)

# c) calculating expected profit
marginalCost <- 0.60
newData$expectedProfit <- (newData$pricePerUnit - marginalCost) * 
  exp(newData$estDemand)

# d) finding price that maximizes profit
maxPrice <- newData$pricePerUnit[newData$expectedProfit == max(newData$expectedProfit)]
maxPrice

# e) creating a function that takes the simpleDemandModel that calculates optimal price as output
optimalPrice = function(inputModel) {
  newData <- data.frame(matrix(seq(0, 2, 0.01), nrow = 201, ncol = 1))
  names(newData) [1] = 'pricePerUnit'
  newData$estDemand <- predict(inputModel, newData)
  marginalCost <- 0.60
  newData$expectedProfit <- (newData$pricePerUnit - marginalCost) * 
    exp(newData$estDemand)
  maxPrice <- newData$pricePerUnit[newData$expectedProfit == max(newData$expectedProfit)]
  maxPrice
}
optimalPrice(simpleDemandModel)

###############################################################################
# Question 3: Bootstrapping

# a) calculating bootstrap distribution of otpimal price
nBootstraps = 1000
nSamples = 360
bsPrice = rep(NA, nBootstraps)

for (i in 1:nBootstraps){
  bsSample = sample(nSamples, replace = TRUE)
  bsData = data[bsSample, ]
  bsDemand = lm(log(units) ~ pricePerUnit, data = bsData)
  bsPrice[i] <- optimalPrice(bsDemand)
}

# b) calculating standard error of otpimal price
optPriceSD <- sd(bsPrice)
optPriceSD # 0.03466973

# c) checking number of bootstrap samples
nBootstraps2 = 2000
nSamples2 = 360
bsPrice2 = rep(NA, nBootstraps2)

for (i in 1:nBootstraps2){
  bsSample2 = sample(nSamples2, replace = TRUE)
  bsData2 = data[bsSample2, ]
  bsDemand2 = lm(log(units) ~ pricePerUnit, data = bsData2)
  bsPrice2[i] <- optimalPrice(bsDemand2)
}

optPriceSD2 <- sd(bsPrice)
optPriceSD2 # 0.03704178

# d) plotting histogram of the distribution of optimal prices
hist(bsPrice)
hist(bsPrice2)


###############################################################################
# Question 4: Sample Size Calculation #4400
# hint: between 0 - 10000

# sample trials
# sample trial 1
nBootstrapsTry1 = 1000
nSamples = 360
bsPriceTry1 = rep(NA, nBootstraps)

for (i in 1:nBootstraps){
  bsSample = sample(nSamples, 3000, replace = TRUE)
  bsDataTry1 = data[bsSample, ]
  bsDemand = lm(log(units) ~ pricePerUnit, data = bsDataTry1)
  bsPriceTry1[i] <- optimalPrice(bsDemand)
}
sd(bsPriceTry1)

# sample trial 2
nBootstrapsTry2 = 1000
nSamples = 360
bsPriceTry2 = rep(NA, nBootstraps)

for (i in 1:nBootstraps){
  bsSample = sample(nSamples, 4000, replace = TRUE)
  bsDataTry2 = data[bsSample, ]
  bsDemand = lm(log(units) ~ pricePerUnit, data = bsDataTry2)
  bsPriceTry2[i] <- optimalPrice(bsDemand)
}
sd(bsPriceTry2)

# sample trial 3
nBootstrapsTry3 = 1000
nSamples = 360
bsPriceTry3 = rep(NA, nBootstraps)

for (i in 1:nBootstraps){
  bsSample = sample(nSamples, 5000, replace = TRUE)
  bsDataTry3 = data[bsSample, ]
  bsDemand = lm(log(units) ~ pricePerUnit, data = bsDataTry3)
  bsPriceTry3[i] <- optimalPrice(bsDemand)
}
sd(bsPriceTry3)

# sample trial 4
nBootstrapsTry4 = 1000
nSamples = 360
bsPriceTry4 = rep(NA, nBootstraps)

for (i in 1:nBootstraps){
  bsSample = sample(nSamples, 4400, replace = TRUE)
  bsDataTry4 = data[bsSample, ]
  bsDemand = lm(log(units) ~ pricePerUnit, data = bsDataTry4)
  bsPriceTry4[i] <- optimalPrice(bsDemand)
}
sd(bsPriceTry4)

###############################################################################
# Question 5: Potential Profit Loss

# a), b), c)
profitFunc = function(price, fit, cost) {
  coeff1 = fit$coefficients[1]
  coeff2 = fit$coefficients[2]
  quantity = exp(coeff1 + coeff2*price)
  profit = (price - cost) * quantity
}

profitStar <- rep(profitFunc(0.83, simpleDemandModel, 0.6), 1000)
profitStar


profitBS = c()
optPriceBS = rep(NA, 1000)
for(i in 1:1000) {
  bsSample = data[sample(1:360, replace = TRUE), ]
  regressionModel = lm(log(units) ~ pricePerUnit, data = bsSample)
  optPriceBS[i] = optimalPrice(regressionModel)
  profitBS[i] = profitFunc(optPriceBS[i], simpleDemandModel, 0.6)
}
sd(optPriceBS)

profitDiff <- profitStar - profitBS
mean(profitDiff)

# changing sample size to 1000
optPriceBS = rep(NA, 1000)
for(i in 1:1000) {
  bsSample = data[sample(1:360, 1000, replace = TRUE), ]
  regressionModel = lm(log(units) ~ pricePerUnit, data = bsSample)
  optPriceBS[i] = optimalPrice(regressionModel)
  profitBS[i] = profitFunc(optPriceBS[i], simpleDemandModel, 0.6)
}
sd(optPriceBS)
max(profitBS)

# comparing difference to get average profit loss
profitDiff <- profitStar - profitBS
mean(profitDiff) # expected profit would decrease (1.451438)

# saving the workspace
save.image("submit.Rdata")
