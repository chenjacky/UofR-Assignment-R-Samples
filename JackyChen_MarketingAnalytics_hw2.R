rm(list = ls())
setwd("~/Google Drive/UofR/R/Assignment 2 - Rocky")

############################################################################
# Part 1 - Data Management

# a) Load the rating data into R and save as a data frame

# X, ConsumerID, rating, rockyID
rockyCSV <- read.csv('recommendDB.csv')

# unique consumer IDs
uniqueConsumerIDs <- unique(rockyCSV$consumerID)

# count unique consumer IDs
consumerNum <- length(uniqueConsumerIDs)

# b) 
# construct a matrix with 6 cols;
# field #1-#5 is rocky #1-#5 ratings, field #6 is consumer ID, 
mat <- matrix(NA, consumerNum, 6)
mat[, 6] <- uniqueConsumerIDs

# loop through original data set
  for(i in 1:nrow(rockyCSV))
{
  # find the corresponding consumer row in matrix
  rowIndex <- match(rockyCSV[i,2], mat[, 6])
  # fill in the data
  rockyID <- rockyCSV[i, 4]
  mat[rowIndex,rockyID] <- rockyCSV[i, 3]
}

# getting the finalDB that is wanted
finalDB <- data.frame(mat[, 1:5])

# renaming finalDB column names
colnames(finalDB) <- c("rocky1", "rocky2", "rocky3", "rocky4", "rocky5")

# saving the dataframe as a .Rdata file
save(finalDB, file = "finalDB.Rdata") 

# saving the workspace
save.image("submit.Rdata") # saving the workspace

############################################################################
# Part 2 - Basic Explanatory Analysis
# if needed.
# load("finalDB.Rdata")
# load("submit.Rdata")

# a) using cor() to calculate the correlations between the 5 movies
cor(finalDB)
corRatingsComplete <- cor(finalDB, use = "complete.obs")
corRatingsPairwiseComplete <- cor(finalDB, use = "pairwise.complete.obs")

# b) using colMeans to calculate the mean ratings of each movie
meanRatings <- colMeans(finalDB, na.rm = TRUE)

# c) only want consumers who has rated rocky4, omit all the others who didn't rate rocky4
consumersRateRocky4 <- subset(finalDB, !is.na(rocky4)) # keep records without missing rocky4 ratings
# or subset(finalDb, complete.cases(finalDB$rocky4))
meanRatingsConsRocky4 <- colMeans(consumersRateRocky4, na.rm = TRUE) # calculating the mean ratings of each movie

# d) only want consumers who have rated all movies
finalDB2 <- subset(finalDB, complete.cases(finalDB)) # getting consumers who have rated all movies
# or finalDB3 <- na.omit(finalDB)
meanRatingsConsAll <- colMeans((finalDB2))

############################################################################
# Part 3- Using Explanatory and Predictive Metrics

# a) Explaining what the code does

# concatenate rocky1 and rocky1^2 into a vector; etc....
rocky1Vec <- c('rocky1','I(rocky1^2)') 
rocky2Vec <- c('rocky2','I(rocky2^2)') 
rocky3Vec <- c('rocky3','I(rocky3^2)') 
rocky4Vec <- c('rocky4','I(rocky4^2)')

# creates a data frame from all combinations of factors from previous rocky1234 vectors
fullSet <- expand.grid(rocky1Vec,rocky2Vec,rocky3Vec,rocky4Vec) 

# apply functions over array margins; the models 
formulaSet <- paste("rocky5 ~",apply(fullSet,1,paste,collapse='+')) 

# using a for loop to print linear models from "formulaSet", 
# using the finalDB data set
for(i in 1:nrow(fullSet)){
  print(lm(as.formula(formulaSet[i]),data = finalDB2))
} 

# b) Making the for loop also treat rocky1 as a categorical variable

# add in as.factor(rocky1) to make it a categorical variable to rocky1Vec
rocky1Vec <- c('rocky1','I(rocky1^2)','as.factor(rocky1)')

# others remain the same
rocky2Vec <- c('rocky2','I(rocky2^2)')
rocky3Vec <- c('rocky3','I(rocky3^2)')
rocky4Vec <- c('rocky4','I(rocky4^2)')

fullSet <- expand.grid(rocky1Vec,rocky2Vec,rocky3Vec,rocky4Vec)
formulaSet <- paste("rocky5 ~",apply(fullSet,1,paste,collapse='+'))

for(i in 1:nrow(fullSet)){
  print(lm(as.formula(formulaSet[i]),data=finalDB2))
}

# c) calculates and stores AIC and BIC for each model the for loop estimates
rocky1Vec <- c('rocky1','I(rocky1^2)','as.factor(rocky1)')
rocky2Vec <- c('rocky2','I(rocky2^2)')
rocky3Vec <- c('rocky3','I(rocky3^2)')
rocky4Vec <- c('rocky4','I(rocky4^2)')

fullSet <- expand.grid(rocky1Vec, rocky2Vec,rocky3Vec,rocky4Vec)
formulaSet <- paste("rocky5 ~",apply(fullSet,1,paste,collapse='+'))

# construct a matrix with 24 rows 2 cols; one for AIC, one for BIC
mat2 <- matrix(0, 24, 2)

# naming the columns
colnames(mat2) <- c('AIC', 'BIC')

# loop through fullSet
for(i in 1:nrow(fullSet)){
  # calculate AIC and BIC
  AICResults <- AIC(lm(as.formula(formulaSet[i]),data = finalDB2))
  BICResults <- BIC(lm(as.formula(formulaSet[i]),data = finalDB2))
  
  # store AIC & BIC results in mat2
  mat2[i,1] <- AICResults
  mat2[i,2] <- BICResults
}

# show AIC and BIC results
AICBICTable <- mat2
print(AICBICTable)

# d) MSE for loop of each model

# for reproducible results
set.seed(321)
#Calculate MSE using validationData
randOrder <- order(runif(nrow(finalDB2)))

# create the hold of sample and the out of sample 
trainingData <- subset(finalDB2,randOrder < .9 * nrow(finalDB2))
validationData <-  subset(finalDB2, randOrder >= .9 * nrow(finalDB2))

mse <- c()
for(i in 1:nrow(fullSet)){ 
  # fit using the training data
  lmS <- lm(as.formula(formulaSet[i]),data=trainingData)
  # calculate MSE using the testing data
  mse[i] <- mean((validationData$rocky5 - predict(lmS,validationData))^2)
}
print(mse)

# combined for loop for AIC, BIC, MSE

# create empty matrix to store AIC, BIC, MSE results
mat3 <- matrix(0, 24, 3)
colnames(mat3) <- c('AIC', 'BIC', 'MSE')

# for reproducible results
set.seed(321)

#Calculate MSE using validationData
randOrder <- order(runif(nrow(finalDB2)))

# create the hold of sample and the out of sample 
trainingData <- subset(finalDB2,randOrder < .9 * nrow(finalDB2))
validationData <-  subset(finalDB2, randOrder >= .9 * nrow(finalDB2))
mse <- c()

# running the loop
for(i in 1:nrow(fullSet)){ 
  # calculate AIC and BIC
  AICResults <- AIC(lm(as.formula(formulaSet[i]),data = finalDB2))
  BICResults <- BIC(lm(as.formula(formulaSet[i]),data = finalDB2))
  # store AIC & BIC results in mat3
  mat3[i,1] <- AICResults
  mat3[i,2] <- BICResults
  # fit using the training data
  lmS <- lm(as.formula(formulaSet[i]),data=trainingData)
  # calculate MSE using the testing data
  mse[i] <- mean((validationData$rocky5 - predict(lmS,validationData))^2)
  # store mse results in mat3
  mat3[i,3] <- mse[i]
}

# e) Comparing which models perform best; choose the smallest numbers
# display the table
print(mat3) 

# find the smallest numbers using min()
min(mat3[,1]) # 12111.27, AIC - model 10
min(mat3[,2]) # 12149.57, BIC - model 10
min(mat3[,3]) # 0.9577973, MSE - model 8