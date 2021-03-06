

#Maria's file

library(ISLR2)
library(tidyverse)
library(caret)
library(caTools)
library(corrplot)

install.packages("caret")
install.packages("caTools")
install.packages("corrplot")

?Boston

###########################################
# Exploratory data analysis

?Boston

# Examine the first 5 lines of the dataset
head(Boston)

# Look at the data structure
str(Boston)

# Produce some summary statistics
summary(Boston)

# Look at the distribution of some variables of interest
Boston %>% 
  ggplot(aes(x = medv)) + 
  geom_density()

Boston %>% 
  ggplot(aes(x = medv, y = lstat)) + 
  geom_density_2d()

# Draw a boxplot or a violin plot
Boston %>% 
  mutate(high_age = ifelse(age > 77, "high", "low")) %>% 
  ggplot(aes(x = high_age, y = medv)) +
  geom_boxplot()

Boston %>% 
  mutate(high_age = ifelse(age > 77, "high", "low")) %>% 
  ggplot(aes(x = high_age, y = medv)) +
  geom_violin()

# Draw a correlation plot
Boston %>% 
  cor() %>% 
  corrplot()

################################
# Linear regression

predict_mdev_1 <- lm(medv ~ lstat + age, data = Boston)


# Predict median value of owner-occupied homes using % of population with lower status and age of building


# Question: which variables are significant?
summary(predict_mdev_1)

# Question: run the same regression with lstat and and rm. What changes?

predict_medv_2 <- lm(medv ~ lstat + rm, data = Boston)


# Question: which variables are significant?

summary(predict_medv_2)

# Question: which model is the best at explaining variation in house value?

#model 2 R squared is higher
###################################################
# Logistic regression

# Recode the medv variable to create a new variable "expensive" which is equal to 1
# if medv is higher or equal to 25, and 0 otherwise
Boston_expensive <- Boston %>% 
  mutate(expensive = ifelse(medv >= 25, "expensive", "other")) %>% 
  mutate(expensive = as.factor(expensive))  

set.seed(42)
#Split the data between a training and a test set
train_index <- sample(c(1:nrow(Boston_expensive)),
                        0.8 * nrow(Boston_expensive))

train_data <- Boston_expensive[train_index, ]
test_data <- Boston_expensive[-train_index, ]


#go console head(train_index)

# Now run a logistic regression of expensive depending on lstat and average number of rooms

logistic_model_1 <- glm(expensive ~ lstat + rm,
                        data = train_data,
                        family = "binomial")

# Question: which variables are significant?
summary(logistic_model_1)

# Predict whether an area is expensive or not for our test data
prediction_1 <- predict(logistic_model_1,
                        test_data, type = "response")

# Convert predictions to factors to compare with real values
predict_1_factor <- ifelse(prediction_1 > 0.5,
                           "expensive", "other") %>% 
  as.factor()

# Create a confusion matrix - how good is the prediction?
confusionMatrix(test_data$expensive,
                predict_1_factor)

# Draw a ROC curve and calculate AUC

colAUC(prediction_1, test_data$expensive, plotROC = TRUE)
##########################################
# Train a model using 5-fold cross-validation


# Print the model


# Predict whether an area is expensive or not for our test data


# Create a confusion matrix - how good is the prediction?



##############################################
# Homework: using the dataset below, build a model to predict whether someone
# has a low credit rating or not. Build a confusion matrix to show the results.
# Push the results to github and create a pull request
credit_dataset <- Credit %>% 
  mutate(low_credit_rating = ifelse(Rating < 93, "low_rating", "other")) 

?credit_dataset

head(credit_dataset)

str(credit_dataset)

summary(credit_dataset)

credit_dataset <- Credit %>% 
  
  mutate(low_credit_rating = ifelse(Balance <= 460, "low", "other")) %>% 
  mutate(low_credit_rating = as.factor(low_credit_rating))  
set.seed(32)

train_index <- sample(c(1:nrow(credit_dataset)),
                      0.7 * nrow(credit_dataset))

train_data <- credit_dataset[train_index, ]
test_data <- credit_dataset[-train_index, ]

logistic_model_1 <- glm(low_credit_rating ~ Income + Limit,
                        data = train_data,
                        family = "binomial")
summary(logistic_model_1)


prediction_1 <- predict(logistic_model_1,
                        test_data, type = "response")

predict_1_factor <- ifelse(prediction_1 > 0.5,
                           "low", "other") %>% 
  as.factor()

confusionMatrix(test_data$low,
                predict_1_factor)

colAUC(prediction_1, test_data$low, plotROC = TRUE)
