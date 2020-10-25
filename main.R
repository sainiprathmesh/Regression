# Title     : TODO
# Objective : TODO
# Created by: prathmeshkumarsaini
# Created on: 25/09/20

# Logistic Regression

# Step 1: Importing the dataset
dataset <- read.csv('Social_Network_Ads.csv')

# Step 2: Data Preprocessing
# Encoding the target feature as factor

dataset <- dataset[3:5] # In this dataset there is no use of User.ID and Gender for furthur analysis.
dataset$Purchased <- factor(dataset$Purchased, levels = c(0, 1))

# Purchased attribute must be converted into factors in which we apply the linear regression model.

# Step 3: Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
split <- sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)


# Step 4: Feature Scaling
training_set[-3] <- scale(training_set[-3]) ## Here -3 means Exculding third column i.e Purchase, we can scale all the columns
test_set[-3] <- scale(test_set[-3])

hist(training_set$Age, col = "red", freq = F, xlim = c(-5, 5))
## Freq means logical; if TRUE, the histogram graphic is a representation of frequencies,
# the counts component of the result;
# if FALSE, probability densities, component density, are plotted (so that the histogram has a total area of one).
# Defaults to TRUE if and only if breaks are equidistant
## Xlim means to define the range of x-axis

curve(dnorm, -5, 5, add = T, col = "blue")
## dnorm is the R function that calculates the p. d. f. f of the normal distribution
## Add = T i.e True means to add the curve on previous graph.
## Add = F i.e False means to Does not add the curve on previous graph.

hist(training_set$EstimatedSalary, col = "red", freq = F, xlim = c(-5, 5))
curve(dnorm, -5, 5, add = T, col = "blue")

# Step 5: Fitting Logistic Regression to the Training set
classifier <- glm(formula = Purchased ~ .,
                  family = binomial,
                  data = training_set)
### glm stands for generalized linear model used for logistic regression.
## Formula is same as linear regression
## Family= binomial means it will consider only binary values
# Step 6: Predicting the Test set results


prob_pred <- predict(classifier, type = 'response', newdata = test_set[-3])

## Predict function is used to predict the values based on the response variable.

y_pred <- ifelse(prob_pred > 0.5, 1, 0)

## Y_pred is used to convert the the floating values in 0 and 1

# Step 7: Making the Confusion Matrix
cm <- table(test_set[, 3], y_pred > 0.5)
accuracy <- (cm[1, 1] + cm[2, 2]) / sum(cm)
print(accuracy)

