# Logistic Regression

# Importing the dataset
training_set = read.csv('train.csv')
test_set = read.csv('test.csv')
# training_set = training_set[3:5]

## Preprocessing Train set

training_set['Average.Donation.Per.Period'] = (training_set$Months.since.First.Donation 
                                          - training_set$Months.since.Last.Donation)/training_set$Number.of.Donations

training_set['ML.MF.Ratio'] = 1 - (training_set$Months.since.Last.Donation/training_set$Months.since.First.Donation)

x_bar_train = sum((training_set$Months.since.First.Donation 
                   - training_set$Months.since.Last.Donation)
                  * training_set$Number.of.Donations)/sum(training_set$Months.since.First.Donation 
                                                     - training_set$Months.since.Last.Donation)

sigma_train = sqrt(sum(((training_set$Number.of.Donations - x_bar_train)^2)
                       *(training_set$Months.since.First.Donation 
                         - training_set$Months.since.Last.Donation))/sum(training_set$Months.since.First.Donation 
                                                                    - training_set$Months.since.Last.Donation))

training_set['z'] = abs(training_set$Number.of.Donations - x_bar_train)/sigma_train


# ## Preprocessing Test set
test_set['Average.Donation.Per.Period'] = (test_set$Months.since.First.Donation
        - test_set$Months.since.Last.Donation)/test_set$Number.of.Donations

test_set['ML.MF.Ratio'] = 1 - (test_set$Months.since.Last.Donation/test_set$Months.since.First.Donation)

x_bar = sum((test_set$Months.since.First.Donation
            - test_set$Months.since.Last.Donation)
            * test_set$Number.of.Donations)/sum(test_set$Months.since.First.Donation
                                            - test_set$Months.since.Last.Donation)

sigma = sqrt(sum(((test_set$Number.of.Donations - x_bar)^2)
                 *(test_set$Months.since.First.Donation
        - test_set$Months.since.Last.Donation))/sum(test_set$Months.since.First.Donation
        - test_set$Months.since.Last.Donation))

test_set['z'] = abs(test_set$Number.of.Donations - x_bar)/sigma



# changing to factor
training_set$Made.Donation.in.March.2007 <- as.character(training_set$Made.Donation.in.March.2007)
training_set$Made.Donation.in.March.2007 <- as.factor(training_set$Made.Donation.in.March.2007)
str(training_set)


# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
# library(caTools)
# set.seed(123)
# split = sample.split(dataset$Made.Donation.in.March.2007, SplitRatio = 0.75)
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)


# Feature Scaling
training_set[, 2:4] = scale(training_set[, 2:4])
training_set[, 6:8] = scale(training_set[, 6:8])

test_set[, 2:7] = scale(test_set[, 2:7])
# test_set[, 6:8] = scale(test_set[, 6:8])


# Fitting Logistic Regression to the Training set
LR_classifier = glm(formula = Made.Donation.in.March.2007 ~ Months.since.Last.Donation
                     + Number.of.Donations
                     + Months.since.First.Donation
                   #  + Average.Donation.Per.Period
                   #  + ML.MF.Ratio
                     + z ,
                    family = binomial,
                    data = training_set)


# we should search for low deviance 
# null deviance: fitting the model with intercept only, without any independent variables
# residual deviance: fitting the model with explanatory variables or independent variables
# AIC: How good the model is in comparison to other models. the model with lowest AIC should be chosen
summary(LR_classifier)


# Predicting the Test set results
prob_pred = predict(LR_classifier, type = 'response', newdata = test_set)
LR_y_pred = ifelse(prob_pred > 0.5, 1, 0)
LR_y_pred


# Making the Confusion Matrix
# cm = table(test_set[, 5], LR_y_pred > 0.5)
# cm

output <- data.frame(test_set$X, LR_y_pred)
# str(output)
write.csv(output, file = 'output_LR.csv')


# Fitting Kernel SVM to the Training set
# install.packages('e1071')
library(e1071)
svm_classifier = svm(formula = Made.Donation.in.March.2007 ~ Months.since.Last.Donation
                     + Number.of.Donations
                     + Months.since.First.Donation 
                     + Average.Donation.Per.Period
                     + ML.MF.Ratio,
                     #+ z ,
                     data = training_set,
                     type = 'C-classification',
                     kernel = 'radial')


summary(svm_classifier)
# print(svm_classifier)


# Predicting the Test set results
prob_pred_svm = predict(svm_classifier, newdata = test_set)


# Making the Confusion Matrix
# cm_svm = table(test_set[, 5], prob_pred_svm)
# cm_svm

output <- data.frame(test_set$X, prob_pred_svm)
# str(output)
write.csv(output, file = 'output_svm.csv')


# Fitting Random Forest Classification to the Training set
# install.packages('randomForest')
# library(randomForest)
# set.seed(123)
# RF_classifier = randomForest(x = training_set$Months.since.Last.Donation 
#                              + training_set$Number.of.Donations
#                              + training_set$Months.since.First.Donation 
#                              + training_set$Average.Donation.Per.Period
#                              + training_set$ML.MF.Ratio,
#                              + training_set$z,
#                           y = training_set$Made.Donation.in.March.2007,
#                           ntree = 500)
# 
# # Predicting the Test set results
# RF_y_pred = predict(RF_classifier, newdata = test_set[-5])
# 
# # Making the Confusion Matrix
# cm_RF = table(test_set[, 5], RF_y_pred)
# cm_RF


#####################################################
### Comparison

# compare diagnostics for the two models 
# forecast::CV(LR_classifier) 
# forecast::CV(svm_classifier) 
# 
# # assess change in R2 
# anova(LR_classifier, svm_classifier, test = 'Chisq') 
# anova(svm_classifier, LR_classifier, test = 'Chisq') 

