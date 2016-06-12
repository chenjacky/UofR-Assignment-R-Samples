rm(list = ls())
setwd("~/Google Drive/UofR/R/Assignment 3 - Wegmans")

library(foreign)
filnm <- "Wegmans Survey 1"; 

spssDataLab <- read.spss(paste(filnm,".sav",sep=""),to.data.frame=TRUE,use.value.labels=TRUE,trim_values=TRUE)
spssData <- read.spss(paste(filnm,".sav",sep=""),to.data.frame=TRUE,use.value.labels=FALSE)

summary(spssDataLab)

# check gender - 1
popSEX <- c(.87, .13)
cbind(popSEX, prop.table(table(spssData$Question33Areyou)[-c(1,4)]))
t <- table(spssData$Question33Areyou)[-c(1,4)]
chisq.test(t, p = popSEX)

# 2 - USE THIS ONE
popSEX <- c(.87, .13)
cbind(popSEX, prop.table(table(spssData$Question33Areyou)[-c(1,4)])); 
t <- table(spssData$Question33Areyou)[-c(1,4)]
chisq.test(t,p=popSEX)
w <-  popSEX/prop.table(table(spssData$Question33Areyou)[-c(1,4)])
w

# 3. updated one
popSEX = c(0, .87,.13, 0); #desired population values 
cbind(popSEX,prop.table(table(spssData$Question33Areyou))); #creating table as matrix 
t <- prop.table(table(spssData$Question33Areyou))
chisq.test(t,p = popSEX)

# Q4 

# FAGE

fage <- grep("Fage",colnames(spssData))
colnames(spssData[,fage])
boughtFage <- spssData$Question21HaveyoupurchasedFageGreekYogurtinthepastmonth == "Yes"
q24 <- grep("Question24", colnames(spssData))
q24Answers <- spssData[,q24]
q24Answers <- q24Answers[,c(2,6,9)]
q24bought <- q24Answers[boughtFage,]

# OIKOS
oikos <- grep("Oikos",colnames(spssData))
colnames(spssData[,oikos])
boughtOikos <- spssData$Question28HaveyoupurchasedStonyfieldOikosGreekYogurtinthepastmon == "Yes"
q30 <- grep("Question30",colnames(spssData))
q30Answers <- spssData[,q30]
q30Answers <- q30Answers[,c(2,8,10)]
q30bought <- q30Answers[boughtOikos,]

q24Natural <- as.numeric(na.omit(q24bought$Question24Allnatural))
q30Natural <- as.numeric(na.omit(q30bought$Question30Allnatural))
q24Price <- as.numeric(na.omit(q24bought$Question24Price))
q30Price <- as.numeric(na.omit(q30bought$Question30Price))
q24Taste <- as.numeric(na.omit(q24bought$Question24Taste))
q30Taste <- as.numeric(na.omit(q30bought$Question30Taste))
t.test(q24Natural,q30Natural)
t.test(q24Price,q30Price)
t.test(q24Taste,q30Taste)

# Q5
colnames(spssData[, c(47, 53, 54, 56)])
usage <- spssData[,c(47,53,54,56)]
cook <- subset(usage, spssData$Question12DoyouuseGreekYogurtforcooking=="Yes")
snack <- subset(usage, spssData$Question12DoyouuseGreekYogurtforcooking=="No ")
head(cook)
head(snack)
summary(cook)
summary(snack)
t.test(as.numeric(cook$Question6Allnatural),as.numeric(snack$Question6Allnatural))
t.test(as.numeric(cook$Question6Organic),as.numeric(snack$Question6Organic))
t.test(as.numeric(cook$Question6Price),as.numeric(snack$Question6Price))
t.test(as.numeric(cook$Question6rbSTfree),as.numeric(snack$Question6rbSTfree))

# then need to match up the ppl who use yogurt as cooking and what they rated
# on the follow attributes - natural, organic, rbst free, price




























