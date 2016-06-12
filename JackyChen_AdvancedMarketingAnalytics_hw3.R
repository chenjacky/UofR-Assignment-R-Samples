rm(list = ls())
setwd("~/Google Drive/UofR/R/Advanced MA/Homework 3")

###############################################################################
# Model Fitting - Part 1
###############################################################################

trainingData <- read.csv("HW3 - Training Data.csv", fileEncoding = 'latin1', stringsAsFactors = F)
trainingData$X <- NULL
trainingData$X.1 <- NULL

testingData <- read.csv("HW3 - Testing Data For Students.csv", fileEncoding = 'latin1', stringsAsFactors = F)
testingData$X <- NULL
testingData$X.1 <- NULL

# String Handling
trainingData$reviewLength <- nchar(paste(trainingData$review.text))
testingData$reviewLength <- nchar(paste(testingData$review.text))

#Clear cases where number.of.votes = 0 
trainingDataReviewDB <- subset(trainingData,trainingData$number.of.votes > 0)
trainingDataReviewDB$percentHelpful <- trainingDataReviewDB$number.of.helpfulness/trainingDataReviewDB$number.of.votes

set.seed(123) # for reproduceable results

train <- sample(1:nrow(trainingDataReviewDB), nrow(trainingDataReviewDB)*0.80)
trainMC <- trainingDataReviewDB[train, ] # 80%
testMC <- trainingDataReviewDB[-train, ] # 20%

# CART Model
library(rpart)
cartFitTime <- system.time(rpart(percentHelpful ~ reviewLength + verified.purchase + vine.review + number.of.helpfulness, 
                                 data = trainMC, 
                                 method = "anova",
                                 control = rpart.control(xval = 10)))
cartFitTime # 0.075
cartFit <- rpart(percentHelpful ~ reviewLength + verified.purchase + vine.review + number.of.helpfulness, 
                 data = trainMC, 
                 method = "anova",
                 control = rpart.control(xval = 10))
cartPred <- predict(cartFit, data = testMC)
cartMSE <- print(mean((testMC$percentHelpful - cartPred)^2)) # 0.009019759
cartVec <- predict(cartFit, data = testingData)

# MARS Model
library(earth)
marsFitTime <- system.time(earth(percentHelpful ~ reviewLength + verified.purchase,
                                 data = trainMC))
marsFitTime # 0.028
marsFit <- earth(percentHelpful ~ reviewLength + verified.purchase,
                 data = trainMC)
marsPred <- predict(marsFit, data = testMC)
marsMSE <- print(mean((testMC$percentHelpful - marsPred)^2)) # 0.007585845
marsVec <- predict(marsFit, data = testingData)

# Bagging Model
library(ipred)
baggedFitTime <- system.time(bagging(percentHelpful ~ reviewLength + verified.purchase,
                                      control = rpart.control(minsplit = 2, cp = 2, xval = 10),
                                      data = trainMC))
baggedFitTime # 0.264
baggedFit <- bagging(percentHelpful ~ reviewLength + verified.purchase,
                     control = rpart.control(minsplit = 2, cp = 2, xval = 10),
                     data = trainMC)
baggedPred <- predict(baggedFit, data = testMC)
baggedMSE <- print(mean((testMC$percentHelpful - baggedPred)^2)) # 0.007355964
baggedVec <- predict(baggedFit, data = testingData)

#Random Forests
library("randomForest")
forestFitTime <- system.time(randomForest(percentHelpful ~ reviewLength + verified.purchase,
                                          data = trainMC))
forestFitTime # 2.490
forestFit <- randomForest(percentHelpful ~ reviewLength + verified.purchase,
                          data = trainMC) 
forestPred <- predict(forestFit, data = testMC)
forestMSE <- print(mean((testMC$percentHelpful - forestPred)^2)) # 0.007521544
forestVec <- predict(forestFit, data = testingData)


#Support Vector Machine
library("e1071")
svmFitTime <- system.time(svm(percentHelpful ~ reviewLength + verified.purchase + top.100.reviewer,
                              cross = 10,
                              data = trainMC))
svmFitTime # 27.044
svmFit <- svm(percentHelpful ~ reviewLength + verified.purchase + top.100.reviewer,
              cross = 10,
              data = trainMC)
svmPred <- predict(svmFit, data = testMC)
svmMSE <- print(mean((testMC$percentHelpful - svmPred)^2)) # 0.009038554
svmVec <- predict(svmFit, data = testingData)


# Boosting
library("gbm")
boostFitTime <- system.time(gbm(formula = percentHelpful ~ reviewLength + verified.purchase,
                                data = trainMC, cv.folds = 10,
                                distribution = "gaussian"))
boostFitTime # 5.520
boostFit <- gbm(formula = percentHelpful ~ reviewLength + verified.purchase,
                data = trainMC, cv.folds = 10,
                distribution = "gaussian")
boostPred <- predict(boostFit, data = testMC,
                     n.trees = boostFit$n.trees)
boostMSE <- print(mean((testMC$percentHelpful - boostPred)^2)) # 0.007357151
boostVec <- predict(svmFit, data = testingData)

# Neural Network
library("nnet")
nnetFitTime <- system.time(nnet(formula = percentHelpful ~ reviewLength + verified.purchase,
                                data = trainMC,
                                linout = 1, size = 2))
nnetFitTime # 0.022
nnetFit <- nnet(formula = percentHelpful ~ reviewLength + verified.purchase,
                data = trainMC,
                linout = 1, size = 2)
nnetPred <- predict(nnetFit, data = testMC)
nnetMSE <- print(mean((testMC$percentHelpful - nnetPred)^2)) # 0.007355835
nnetVec <- predict(nnetFit, data = testingData)

# K-Nearest Neighbor
library("kknn")
knnFitTime <- system.time(kknn(percentHelpful ~ verified.purchase + top.100.reviewer, 
                               k = 10,
                               train = trainMC, test = testMC))
knnFitTime # 0.182
knnFit <- kknn(percentHelpful ~ verified.purchase + top.100.reviewer, 
               k = 10,
               train = trainMC, test = testMC)
knnPred <- predict(knnFit, data = testMC)
knnMSE <- print(mean((testMC$percentHelpful - knnPred)^2)) # 0.007392343
knnVec <- predict(knnFit, data = testingData)

bestVec <- baggedVec

###############################################################################
# Model Comparison - Part 2
###############################################################################

mat <- matrix(NA, 2, 8)
colnames(mat) <- c("cartVec","marsVec", "baggedVec","forestVec","svmVec", "boostVec","nnetVec","knnVec")
rownames(mat) <- c("mse", "system.time")
mat[1, 1] <- cartMSE
mat[2, 1] <- 0.075

mat[1, 2] <- marsMSE
mat[2, 2] <- 0.028

mat[1, 3] <- baggedMSE
mat[2, 3] <- 0.264

mat[1, 4] <- forestMSE
mat[2, 4] <- 2.490

mat[1, 5] <- svmMSE
mat[2, 5] <- 28.924

mat[1, 6] <- boostMSE
mat[2, 6] <- 5.520

mat[1, 7] <- nnetMSE
mat[2, 7] <- 0.134

mat[1, 8] <- knnMSE
mat[2, 8] <- 0.180

part2DB <- data.frame(mat)
min(part2DB[1,]) # finding the model with lowest MSE
# baggedVec

save.image(file = "workspace.Rdata")

# Data cleaning for sample review
library(tm)
review_text <- paste(trainingData$review.text, collapse=" ")
review_source <- VectorSource(review_text)
corpus <- Corpus(review_source)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords("english"))

dtm <- DocumentTermMatrix(corpus)
dtm2 <- as.matrix(dtm)
frequency <- colSums(dtm2)
frequency <- sort(frequency, decreasing=TRUE)

library(wordcloud)
words <- names(frequency)
wordcloud(words[1:100], frequency[1:100])

save.image(file = "data cleaning hw3.Rdata")

