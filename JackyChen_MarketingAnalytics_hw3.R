# hw3_JackyChen

# setting up the working directory
rm(list = ls())
setwd("~/Google Drive/UofR/R/Assignment 3")

# read in csv
homework3DB <- read.csv('homework3DB.csv')

# remove the unnecessary column
homework3DB$X <- NULL

############################################################################
# Part 1 - Causal Regressions

# a) linear regression using releaseRank to predict log of subsequentEarnings
# subset for subsequentEarnings > 0 first
subsetGroup1 <- subset(homework3DB, homework3DB$subsequentEarnings > 0)
releaseRank <- subsetGroup1$releaseRank
subsequentEarnings <- subsetGroup1$subsequentEarnings

# running the regression
lm1 <- lm(log(subsequentEarnings) ~ releaseRank)

# regression results
summary(lm1)

############################################################################
# Part 2 - Data Cleaning

# a), b)
# unique 'date' by getting unique weeks in data set
uniqueReleaseDate <- unique(homework3DB$releaseDate)

# find highest rank and second highest rank
subsetRank1 <- subset(homework3DB, homework3DB$releaseRank == 1) # highest rank
subsetRank2 <- subset(homework3DB, homework3DB$releaseRank == 2) # second highest rank

# merge rank 1 and rank 2 movies by same release date
sameReleaseDateRank <- merge(subsetRank1, subsetRank2, by = "releaseDate")
sameReleaseDateRankV2 <- subset(sameReleaseDateRank,
                              sameReleaseDateRank$subsequentEarnings.x &
                              sameReleaseDateRank$subsequentEarnings.y > 0)

# create empty matrix, convert to data frame, rename the columns
mat <- matrix(0 , 1232, 3)
mat <- as.data.frame(mat)
colnames(mat) <- c("date", "firstWeekBoxOfficeDiff", "laterLogBoxOfficeDiff")

# for loop to make the new data frame with the wanted data
for(i in 1:nrow(sameReleaseDateRankV2)){
  mat[i,1] <- as.character(sameReleaseDateRankV2[i,1]) # 'date'
  mat[i,2] <- log(sameReleaseDateRankV2[i,3]) - log(sameReleaseDateRankV2[i,7]) # 'firstWeekBoxOfficeDiff'
  mat[i,3] <- log(sameReleaseDateRankV2[i,4]) - log(sameReleaseDateRankV2[i,8]) # 'laterLogBoxOfficeDiff'
}

# renaming the new data frame
boxOfficeDiffDB <- mat

# saving the finalDB
save(boxOfficeDiffDB, file = "finalDB.Rdata") 

# saving the workspace
save.image("submit.Rdata")

############################################################################
# Part 3 - Regression Discontinuity

# b)
subsetPart3 <- subset(boxOfficeDiffDB, boxOfficeDiffDB$firstWeekBoxOfficeDiff <.1)
subsetPart3

# c)
getApril42008Movie <- subset(sameReleaseDateRankV2, sameReleaseDateRankV2$releaseDate == "4/4/2008")
getApril42008MovieV2 <- subset(boxOfficeDiffDB, boxOfficeDiffDB$date == "4/4/2008")

# d)
t.test(subsetPart3$laterLogBoxOfficeDiff)