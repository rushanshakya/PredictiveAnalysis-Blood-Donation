setwd("C:/Users/Rusan/Desktop/School/ISQS 6349 - Predictive Analysis/Project/Blood")

library(ggplot2)

# Importing the dataset
training_set = read.csv('trainingdata.csv')

str(training_set)
head(training_set)

#Plotting the training data set
plot(training_set)
cor(training_set)

#Histograms
hist(training_set$Months.since.Last.Donation, xlab="Months since last donation", main="Histogram for Months since last donation")
hist(training_set$Number.of.Donations, xlab="Number of Donations", main="Histogram for number of donations")
hist(training_set$Total.Volume.Donated..c.c.., xlab="Total Volume Donated", main="Histogram for total volume donated")
hist(training_set$Months.since.First.Donation, xlab="Months Since First Donation", main="Histogram for months since first donation")
hist(training_set$Made.Donation.in.March.2007, xlab= "Made Donation in March 2007", main="Histogram for Made Donation in March 2007")
