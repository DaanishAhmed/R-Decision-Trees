# DATA 630 Assignment 3
# Written by Daanish Ahmed
# Semester Summer 2017
# June 17, 2017
# Professor Edward Herranz

# This R script creates a decision tree classification model using a dataset 
# containing information on bank marketing data.  The purpose of this assignment 
# is to generate a model that predicts the value of the dependent variable 
# "suscribed.term.deposited" using the other variables as independent variables.
# Additionally, the script will evaluate the model's accuracy and attempt to 
# improve it by oversampling the minority class (to deal with bias) and construct
# a random forest (to deal with high variance).  The script consists of several
# components, including opening and initializing the data, exploration and 
# preprocessing, building and analyzing the decision tree, oversampling, and 
# applying the random forest algorithm to the oversampled dataset.



# This section of code covers opening the dataset and initializing the packages 
# that are used in this script.

# Sets the working directory for this assignment.  Please change this directory 
# to whichever directory you are using, and make sure that all files are placed 
# in that location.
setwd("~/Class Documents/2016-17 Summer/DATA 630/R/Assignment 3")

# In order to run the commands for the decision tree, oversampling, and random 
# forests, we need to use the following packages:

# If you have not installed these packages yet, remove the three # symbols below.
# install.packages("party")
# install.packages("unbalanced")
# install.packages("randomForest")

# Loads the necessary packages into the system.
library("party")
library("unbalanced")
library("randomForest")

# Opens the CSV file "bank-marketing.csv".
bank <- read.csv("bank-marketing.csv", head=TRUE, sep=",")

# End of opening the dataset.



# This section of code covers data preprocessing.  It includes exploration of 
# the original dataset, removing variables, and dealing with missing values.

# Previews the bank marketing dataset.
View(bank)

# Shows the descriptive statistics for all variables in the dataset.
summary(bank)

# Displays the structure of the data.  This is necessary to see if there are 
# any unique identifiers (IDs) or other variables that can be removed.
str(bank)

# There are no unique identifier variables in this dataset, thus we do not 
# need to remove them.

# The dependent variable "suscribed.term.deposited" is already a factor, 
# thus we do not need to convert it into one.

# The variables "yearly.balance" and "passed.days" are factors with 2294 and 
# 292 levels respectively, meaning that they aren't useful for the decision 
# tree algorithm.  Also, not all of their values are numbers (some of them 
# have "?" symbols as values).  Thus, we remove them.
bank <- bank[, -c(6, 14)]

# Verifies that the unwanted variables have been removed.
str(bank)

# The decision tree algorithm is able to handle missing values.  Therefore, 
# we do not need to check for missing values.

# End of data preprocessing.



# This section of code covers the creation of the decision tree classification
# model.  It includes dividing the data into training and test datasets, 
# creating and visualizing the model, and analyzing the model on the training 
# and test datasets.

# Generates a random seed to allow us to reproduce the results.
set.seed(1234)

# The following code splits the bank marketing dataset into a training set 
# consisting of 70% of the data and a test set containing 30% of the data.
ind <- sample(2, nrow(bank), replace = TRUE, prob = c(0.7, 0.3))
train.data <- bank[ind == 1, ]
test.data <- bank[ind == 2, ]

# The following code creates the decision tree classification model using the 
# training data with suscribed.term.deposited as the dependent variable and all 
# other variables as independent variables.
myFormula <- suscribed.term.deposited ~ .
model <- ctree(myFormula, data = train.data)

# Prints the decision tree model.
print(model)

# Displays the visualization of the decision tree.
plot(model)

# Plots a simplified version of the decision tree.
plot(model, type="simple")

# Displays the confusion matrix on the model for training data, allowing 
# us to see the model's accuracy.
table(predict(model), train.data$suscribed.term.deposited)

# Displays the confusion matrix on the model for test data.
testPred <- predict(model, newdata = test.data)
table(testPred, test.data$suscribed.term.deposited)

# End of creating the decision tree model.



# This section of code covers the oversampling of our original dataset.  
# The original model appears to be biased towards predicting the majority
# class of the dependent variable.  We will oversample the minority class
# to make the dataset less biased and apply the decision tree algorithm 
# to this dataset.

# Converts the dependent variable into a character type so that we can 
# replace the variable's values.
bank$suscribed.term.deposited <- as.character(bank$suscribed.term.deposited)

# The 'ubOver' function in the 'unbalanced' package requires the dependent 
# variable to be a factor with the values 0 and 1.  Thus, we replace the 
# values of our dependent variable.
bank$suscribed.term.deposited[bank$suscribed.term.deposited == 'no'] <- '0'
bank$suscribed.term.deposited[bank$suscribed.term.deposited == 'yes'] <- '1'

# Converts our dependent variable back into a factor.
bank$suscribed.term.deposited <- as.factor(bank$suscribed.term.deposited)

# The following code is from Package 'unbalanced'
# Based on Pozzolo, Caelen, & Bontempi (2015)
# https://cran.r-project.org/web/packages/unbalanced/unbalanced.pdf
# Modified for UMUC DATA 630 by Daanish Ahmed

# Implements the 'ubOver' function from the 'unbalanced' package so that 
# we can oversample the minority class of our dependent variable.  Uses 
# "suscribed.term.deposited" as the dependent variable and all other 
# variables as the independent variables.
output <- bank$suscribed.term.deposited
input <- bank[ ,-15]
data <- ubOver(X=input, Y= output)
bank <- cbind(data$X, data$Y)

# End of Pozzolo, Caelen, & Bontempi code.

# We give the dependent variable its original name.
names(bank)[15] <- "suscribed.term.deposited"

# Generates a random seed to allow us to reproduce the results.
set.seed(1234)

# The following code splits the oversampled dataset into a training set 
# consisting of 70% of the data and a test set containing 30% of the data.
ind <- sample(2, nrow(bank), replace = TRUE, prob = c(0.7, 0.3))
train.data <- bank[ind == 1, ]
test.data <- bank[ind == 2, ]

# The following code creates the decision tree classification model using the 
# oversampled training data with suscribed.term.deposited as the dependent 
# variable and all other variables as independent variables.
myFormula <- suscribed.term.deposited ~ .
model <- ctree(myFormula, data = train.data)

# Prints the decision tree model.
print(model)

# Displays the visualization of the decision tree (may take a few seconds).
plot(model)

# Plots a simplified version of the decision tree (may take a few seconds).
plot(model, type="simple")

# Displays the confusion matrix on the model for training data, allowing 
# us to see the model's accuracy.
table(predict(model), train.data$suscribed.term.deposited)

# Displays the confusion matrix on the model for test data.
testPred <- predict(model, newdata = test.data)
table(testPred, test.data$suscribed.term.deposited)

# End of creating the oversampled decision tree model.



# This section of code covers the implementation of the random forest 
# algorithm on the oversampled dataset.  This algorithm is used to reduce 
# the variance of our decision tree model.

# Generates a random seed to allow us to reproduce the results.
set.seed(1234)

# Implements the random forest algorithm on the training data using 
# "suscribed.term.deposited" as the dependent variable and all other 
# variables as independent variables.  We will create 25 trees.
forest <- randomForest(suscribed.term.deposited ~ ., data = train.data, 
                   ntree = 25, importance = TRUE)

# Displays the output of the random forest, including the type of forest, 
# number of trees, number of variables used for each split, confusion 
# matrix, and error rate.
forest

# Displays the confusion matrix on the model for test data.
forest.pred <- predict(forest, newdata = test.data)
table(forest.pred, test.data$suscribed.term.deposited)

# End of creating random forest.

# End of script.

