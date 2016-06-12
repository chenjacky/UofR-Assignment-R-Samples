rm(list = ls())
setwd("~/Google Drive/UofR/Winter Quarter/Digital Marketing Strategy/AdFX Assignment")
library(plyr)
library(ggplot2)

onlineCampaignEval <- read.csv('AdFX-Winter 2016.csv')

# Q1

# 1a)
# converting data to 1 = female, 0 = male

# method 1
# levels(onlineCampaignEval$gender) <- c("1","0")

# method 2 - I USED THIS ONE
onlineCampaignEval$Gender <- as.numeric(onlineCampaignEval$gender == "female")

# method 3
# onlineCampaignEval$Gndr[onlineCampaignEval$gender == "female"] <- 1
# onlineCampaignEval$Gndr[onlineCampaignEval$gender == "male"] <- 0

# treatment and control
treatment <- subset(onlineCampaignEval, onlineCampaignEval$Treatment == 1) # 288172 in treatment
control <- subset(onlineCampaignEval, onlineCampaignEval$Treatment == 0) # 288172 in control
onlineCampaignEval$treatment.f <- as.factor(onlineCampaignEval$Treatment)

# female
female <- subset(onlineCampaignEval, onlineCampaignEval$Gender == 1)

# male
male <- subset(onlineCampaignEval, onlineCampaignEval$Gender == 0)

# female proportion
femaleProp <- sum(control$Gender)/nrow(control) # 0.5867537 # males are 0, so summing it up works like this

# male proportion
maleProp <- 1 - (sum(control$Gender)/nrow(control)) # 0.4132463
maleProp
# ii) plot the histogram
ggplot(onlineCampaignEval, aes(gender, fill = treatment.f)) + geom_bar(position = "dodge")

# 1b)
treatmentGroup <- subset(onlineCampaignEval, onlineCampaignEval$Treatment == 1)
controlGroup <- subset(onlineCampaignEval, onlineCampaignEval$Treatment == 0)
avgPastSalesTreat <- mean(treatmentGroup$past_sales) # 1.231393
avgPastSalesControl <- mean(controlGroup$past_sales) # 1.237662
pastSalesTreat <- treatmentGroup$past_sales
pastSalesControl <- controlGroup$past_sales

liftPastSales <- avgPastSalesTreat - avgPastSalesControl
liftPastSales # -0.006268097; ads will make it worse

# St. Dev Treatment
sigma_pastSalesT <- sd(pastSalesTreat)
# St. Dev Control
sigma_pastSalesC <- sd(pastSalesControl)
# Obs. in Treatment
N_pastSalesT <- length(pastSalesTreat)
# Obs. in Control
N_pastSalesC <- length(pastSalesControl)
# Standard Error of Lift
stErr_liftPastSales <- sqrt(sigma_pastSalesT^2/N_pastSalesT + sigma_pastSalesC^2/N_pastSalesC)
stErr_liftPastSales # 0.01088483
# 95% Confidence Interval
liftPastSales + 1.96 * stErr_liftPastSales # 0.01506616
liftPastSales - 1.96 * stErr_liftPastSales # -0.02760235

# plotting histogram
ggplot(onlineCampaignEval,aes(past_sales)) + 
  geom_histogram(aes(fill=onlineCampaignEval$treatment.f),position = 'dodge',binwidth = 5) + 
  ylab("Consumer Count")

# Q2
# observational estimate
sawAdYesT <- mean(treatment$sales[treatment$saw_ads== 1])
sawAdNoT  <- mean(treatment$sales[treatment$saw_ads== 0])
obsEstSawAds <- mean(sawAdYesT) - mean(sawAdNoT)
obsEstSawAds # 0.9201687
summary(lm(sales ~ saw_ads, data = treatment))
obsEstSawAds + 1.96 * 0.010885 # 0.9415033
obsEstSawAds - 1.96 * 0.010885 # 0.8988341

# intention to treat (ITT) estimator
sawAdYesC <- mean(onlineCampaignEval$sales[onlineCampaignEval$Treatment == 1])
sawAdNoC <- mean(onlineCampaignEval$sales[onlineCampaignEval$Treatment == 0])
ittEst <- sawAdYesC - sawAdNoC
ittEst # 0.02415981
summary(lm(sales ~ Treatment, data = onlineCampaignEval))
ittEst + 1.96 * 0.011042 # 0.04580213
ittEst - 1.96 * 0.011042 # 0.002517487

# treatment on treatment (TOT) estimator
sawAds <- subset(onlineCampaignEval, onlineCampaignEval$saw_ads == 1)
sawAdsT <- mean(sawAds$sales[sawAds$Treatment== 1])
sawAdsC <- mean(sawAds$sales[sawAds$Treatment== 0])
totEst <- mean(sawAdsT) - mean(sawAdsC)
totEst # 0.02836015
summary(lm(sales ~ Treatment, data = sawAds))
totEst + 1.96 * 0.01456 # 0.05689775
totEst - 1.96 * 0.01456 # -0.0001774515

# Q3
# a)
women <- subset(onlineCampaignEval, onlineCampaignEval$Gender == 1)
womenSawAds <- subset(women, women$saw_ads == 1)
womenSawAdsT <- mean(womenSawAds$sales[womenSawAds$Treatment== 1])
womenSawAdsC <- mean(womenSawAds$sales[womenSawAds$Treatment== 0])
totEstWomen <- mean(womenSawAdsT) - mean(womenSawAdsC)
totEstWomen # 0.05421324

# b)
men <- subset(onlineCampaignEval, onlineCampaignEval$Gender == 0)
menSawAds <- subset(men, men$saw_ads == 1)
menSawAdsT <- mean(menSawAds$sales[menSawAds$Treatment== 1])
menSawAdsC <- mean(menSawAds$sales[menSawAds$Treatment== 0])
totEstMen <- mean(menSawAdsT) - mean(menSawAdsC)
totEstMen # -0.009724874