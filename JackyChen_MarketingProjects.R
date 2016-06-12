# Team Members: Jacky Chen, Surbhi Modi, Ying Zhang, Qing Wu
# Sigma Marketing Insights - Big Toys Insurance

rm(list = ls())
setwd("~/Google Drive/UofR/R/Marketing Projects/Big Toys")

library(dplyr)
library(ggplot2)
library(cluster)
library(fpc)

bigToysCust <- read.csv('Customer Database - Big Toys_CEXAppend.csv', header = T)
bigToysRand <- read.csv('Random Database Sample - Big Toys_CEXAppend.csv', header = T)
bigToysAll <- dplyr :: bind_rows(bigToysCust, bigToysRand)

summary(bigToysAll)



# exploring CUST
plot(density(bigToysCust$age))
plot(density(bigToysCust$income))
plot(density(bigToysCust$median_income))
abline(v=mean(bigToysCust$median_income))
abline(v=median(bigToysCust$median_income))
kmeans(cbind(bigToysCust$age, bigToysCust$income), centers = 2)

# for bigToysCust
bigToysCust$income = as.integer(bigToysCust$income)
bigToysCust$gender = as.integer(bigToysCust$gender)
bigToysCust$marital_status = as.integer(bigToysCust$marital_status)
bigToysCust$home_owner = as.integer(bigToysCust$home_owner)
bigToysCust$dwelling_type = as.integer(bigToysCust$dwelling_type)

bigToysCust$age[is.na(bigToysCust$age)] = median(bigToysCust$age, na.rm = TRUE)
bigToysCust$cars[is.na(bigToysCust$cars)] = median(bigToysCust$cars, na.rm = TRUE)
bigToysCust$children[is.na(bigToysCust$children)] = median(bigToysCust$children, na.rm = TRUE)
bigToysCust$doit_yourselfer[is.na(bigToysCust$doit_yourselfer)] = median(bigToysCust$doit_yourselfer, na.rm = TRUE)
bigToysCust$family[is.na(bigToysCust$family)] = median(bigToysCust$family, na.rm = TRUE)
bigToysCust$gender[is.na(bigToysCust$gender)] = median(bigToysCust$gender, na.rm = TRUE)
bigToysCust$great_outdoors[is.na(bigToysCust$great_outdoors)] = median(bigToysCust$great_outdoors, na.rm = TRUE)
bigToysCust$income[is.na(bigToysCust$income)] = median(bigToysCust$income, na.rm = TRUE)
bigToysCust$luxury_life[is.na(bigToysCust$luxury_life)] = median(bigToysCust$luxury_life, na.rm = TRUE)
bigToysCust$marital_status[is.na(bigToysCust$marital_status)] = median(bigToysCust$marital_status, na.rm = TRUE)
bigToysCust$median_income[is.na(bigToysCust$median_income)] = median(bigToysCust$median_income, na.rm = TRUE)
bigToysCust$motor_cycle[is.na(bigToysCust$motor_cycle)] = median(bigToysCust$motor_cycle, na.rm = TRUE)
bigToysCust$num_in_hhld[is.na(bigToysCust$num_in_hhld)] = median(bigToysCust$num_in_hhld, na.rm = TRUE)
bigToysCust$num_of_adults[is.na(bigToysCust$num_of_adults)] = median(bigToysCust$num_of_adults, na.rm = TRUE)
bigToysCust$num_of_children[is.na(bigToysCust$num_of_children)] = median(bigToysCust$num_of_children, na.rm = TRUE)
bigToysCust$sporting_life[is.na(bigToysCust$sporting_life)] = median(bigToysCust$sporting_life, na.rm = TRUE)
bigToysCust$travel[is.na(bigToysCust$travel)] = median(bigToysCust$travel, na.rm = TRUE)
bigToysCust$travel_vacation[is.na(bigToysCust$travel_vacation)] = median(bigToysCust$travel_vacation, na.rm = TRUE)

# for bigToysRand
bigToysRand$income = as.integer(bigToysRand$income)
bigToysRand$gender = as.integer(bigToysRand$gender)
bigToysRand$marital_status = as.integer(bigToysRand$marital_status)
bigToysRand$home_owner = as.integer(bigToysRand$home_owner)
bigToysRand$dwelling_type = as.integer(bigToysRand$dwelling_type)

bigToysRand$age[is.na(bigToysRand$age)] = median(bigToysRand$age, na.rm = TRUE)
bigToysRand$cars[is.na(bigToysRand$cars)] = median(bigToysRand$cars, na.rm = TRUE)
bigToysRand$children[is.na(bigToysRand$children)] = median(bigToysRand$children, na.rm = TRUE)
bigToysRand$doit_yourselfer[is.na(bigToysRand$doit_yourselfer)] = median(bigToysRand$doit_yourselfer, na.rm = TRUE)
bigToysRand$family[is.na(bigToysRand$family)] = median(bigToysRand$family, na.rm = TRUE)
bigToysRand$gender[is.na(bigToysRand$gender)] = median(bigToysRand$gender, na.rm = TRUE)
bigToysRand$great_outdoors[is.na(bigToysRand$great_outdoors)] = median(bigToysRand$great_outdoors, na.rm = TRUE)
bigToysRand$income[is.na(bigToysRand$income)] = median(bigToysRand$income, na.rm = TRUE)
bigToysRand$luxury_life[is.na(bigToysRand$luxury_life)] = median(bigToysRand$luxury_life, na.rm = TRUE)
bigToysRand$marital_status[is.na(bigToysRand$marital_status)] = median(bigToysRand$marital_status, na.rm = TRUE)
bigToysRand$median_income[is.na(bigToysRand$median_income)] = median(bigToysRand$median_income, na.rm = TRUE)
bigToysRand$motor_cycle[is.na(bigToysRand$motor_cycle)] = median(bigToysRand$motor_cycle, na.rm = TRUE)
bigToysRand$num_in_hhld[is.na(bigToysRand$num_in_hhld)] = median(bigToysRand$num_in_hhld, na.rm = TRUE)
bigToysRand$num_of_adults[is.na(bigToysRand$num_of_adults)] = median(bigToysRand$num_of_adults, na.rm = TRUE)
bigToysRand$num_of_children[is.na(bigToysRand$num_of_children)] = median(bigToysRand$num_of_children, na.rm = TRUE)
bigToysRand$sporting_life[is.na(bigToysRand$sporting_life)] = median(bigToysRand$sporting_life, na.rm = TRUE)
bigToysRand$travel[is.na(bigToysRand$travel)] = median(bigToysRand$travel, na.rm = TRUE)
bigToysRand$travel_vacation[is.na(bigToysCust$travel_vacation)] = median(bigToysRand$travel_vacation, na.rm = TRUE)


colnames(is.na(bigToysCust))


library(plyr)
coldata <- data.frame(coltype=sapply(bigToysCust, class))
coldata['num.na'] <- sapply(bigToysCust, function(x) sum(is.na(x)))
summary(coldata$num.na)
ggplot(coldata, aes(num.na)) + geom_histogram(binwidth=100) +
  theme(text = element_text(size=20)) +
  labs(title="Histogram of Features by Number of Missing Values", 
       x="Number of Missing Values", y="Number of Features")

coldata
coldata$Variables = rownames(coldata)

ggplot(coldata, aes(Variables,num.na)) + geom_bar(stat = "identity") + 
  labs(title="Variables by Number of Missing Values", 
       x="Variables", y="Number of Missing Values") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

bigCust = bigToysCust[,c(8,47,21,31,45,6,27,9,30,7,10,33,13,14,15,28,39,52)]

# Determine number of clusters
wss <- (nrow(bigCust))*sum(apply(bigCust,2,var))
for (i in 1:15) wss[i] <- sum(kmeans(bigCust, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


cluster1 = kmeans(bigCust,4)
cluster1
cluster1$centers
clusplot(bigCust, cluster1$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

plotcluster(bigCust,cluster1$cluster)

cluster2 = kmeans(bigCust,2)
cluster2
cluster2$centers
clusplot(bigCust, cluster2$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

plotcluster(bigCust,cluster2$cluster)