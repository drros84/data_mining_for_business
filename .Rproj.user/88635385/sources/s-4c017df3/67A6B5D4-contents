
library(ISLR2)
library(tidyverse)
library(caret)
library(caTools)
library(corrplot)

###########################################
# Exploratory data analysis

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

# Draw a correlation plot
Boston %>% 
  cor() %>% 
  corrplot()

################################
# Linear regression

# Predict median value of owner-occupied homes using % of population with lower status and age of building


# Question: which variables are significant?


# Question: run the same regression with lstat and and rm. What changes?


# Question: which variables are significant?


# Question: which model is the best at explaining variation in house value?


###################################################
# Logistic regression

# Recode the medv variable to create a new variable "expensive" which is equal to 1
# if medv is higher or equal to 25, and 0 otherwise
Boston_expensive <- Boston %>% 
  mutate(expensive = ifelse(medv >= 25, "expensive", "other")) %>% 
  mutate(expensive = as.factor(expensive))  

#Split the data between a training and a test set



# Now run a logistic regression of expensive depending on lstat and average number of rooms


# Question: which variables are significant?


# Predict whether an area is expensive or not for our test data


# Convert predictions to factors to compare with real values


# Create a confusion matrix - how good is the prediction?


# Draw a ROC curve and calculate AUC


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

