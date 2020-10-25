## Linear Regression ##

## Step1: Import the dataset.

dataset <- read.csv("ToyotaCorolla.csv")

## Step 2: Data Preprocessing

## There is no null and missing values in the data.

str(dataset) ## It is used to see the datatype of each of the attribute
## As we seen that FuelType is Categorical variable or quantitative variable.
## To use FuelType in Our Furthur Analysis Covert it into Numeric Type.

dataset$FuelType <- factor(dataset$FuelType, levels = c("CNG", "Diesel", "Petrol"), labels = c(0, 1, 2))

## Step 3: Exploratory Data Analysis 
## By Using plots we can do the Analysis

library(ggplot2)

ggplot(dataset, aes(y = Price, x = Age)) + geom_point()

## Attribute aes is used to specify variables on X and Y axis.
## Geom_point() is used to plot the points on the graph

ggplot(dataset, aes(y = Price, x = KM)) + geom_point()
ggplot(dataset, aes(y = Price, x = HP)) + geom_point()
ggplot(dataset, aes(y = Price, x = FuelType)) + geom_point()

#To check normality use the qqplot
qqnorm(dataset$Price) ## It is used to draw the noraml Distribution curve

qqline(dataset$Price) ## It is used to create the line.
qqplot(dataset$Age, dataset$Price) ## It is used to create the noraml Distribution curve in between two variables.


#step 4: Divide the data into Training data and Test Data
## First install 'caTools' Library

install.packages('caTools')
library(caTools)

split <- sample.split(dataset$Price, SplitRatio = 0.8)
## Sample.split function divide the data into two sets. By SplitRatio Attribute we can divide the dataset according to your need.
## In this we can divide the dataset i.e Training data is 80% and Test data is 20%


training_set <- subset(dataset, split == TRUE) ## 80% data to train the model
test_set <- subset(dataset, split == FALSE) ## 20% data to test the model

## Step 5: Now apply the linear Regression model 

regressor <- lm(formula = Price ~ ., data = training_set)
## lm function is a function for linear regression.
## Formula attribute is used to specify the dependent and independent variables
## variable on the left of '~' are dependent variables and on the right of it there are indepenedent variable.
## '.' operator means all the independent variable.But only one used at a time.

## Step 6: Now apply the model on Test dataset
y_pred <- predict(regressor, newdata = test_set)
y_pred ## It has all the predicted values of price.
summary(regressor) ## It is a used to see the statistics of predicted variable.


##Step 7: Test the Accuracy of the model
cor.test(y_pred, test_set$Price) ## Accuracy is 82.4 %




