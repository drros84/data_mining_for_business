
# Annamov_file



library(ISLR2)
library(tidyverse)
library(caret)
install.packages("caret")
library(caret)
 install.packages("caTools")
library(caTools)

library(caTools)
 
library(corrplot)
 install.packages("corrplot")
 
 library(corrplot)

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

# Predict median value of owner-occupied homes using % of population with lower status and age of building
predict_mdev_1 <- lm(medv ~ lstat + age, data = Boston)

# Question: which variables are significant?
summary(predict_mdev_1)

# Question: run the same regression with lstat and and rm. What changes?
predict_mdev_2 <- lm(medv ~ lstat + rm, data = Boston)
summary(predict_mdev_2)

# Question: which variables are significant?


# Question: which model is the best at explaining variation in house value?
# Model 2 becuase the R squared is higher

###################################################
# Logistic regression

# Recode the medv variable to create a new variable "expensive" which is equal to 1
# if medv is higher or equal to 25, and 0 otherwise
Boston_expensive <- Boston %>% 
  mutate(expensive = ifelse(medv >= 25, "expensive", "other")) %>% 
  mutate(expensive = as.factor(expensive)) 
set.seed(42)

#Split the data between a training and a test set

train_index <-  sample(c(1:nrow(Boston_expensive)),
                         0.8 * nrow(Boston_expensive))
train_data <- Boston_expensive[train_index, ]
test_data <- Boston_expensive[-train_index, ]
test_data
# Now run a logistic regression of expensive depending on lstat and average number of rooms

logistic_model_1 <-  glm(expensive ~ lstat + rm,
                         data = train_data,
                         family = "binomial")


# Question: which variables are significant?
summary(logistic_model_1)


# Predict whether an area is expensive or not for our test data
prediction_1 <-  predict(logistic_model_1, 
                         test_data, type="response")

# Convert predictions to factors to compare with real values


# Create a confusion matrix - how good is the prediction?
confusionMatrix(test_data$expensive,
                predict_1_factor)

# Draw a ROC curve and calculate AUC
colAUC(prediction_1, test_data$expensive,
       predict_1_factor)

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

library(ISLR2)
library(tidyverse)
library(caret)
install.packages("caret")
library(caret)
install.packages("caTools")
library(caTools)

library(caTools)

library(corrplot)
install.packages("corrplot")

library(corrplot)

install.packages("tidyverse")
library(tidyverse)

install.packages("caret")
library(caret)

install.packages("cartools")
library(caTools)
library(corrplot)
library(ISLR2)

install.packages("corrplot")
library(corrplot)

head(Boston)

?Boston
??Boston

head(Boston, 20)

str(Boston)

summary(Boston)

Boston%>%
  ggplot(aes(x = medv)) + 
  geom_density()

Boston%>%
ggplot(aes(x = medv)) +
  geom_density()

Boston%>%
  ggplot(aes(x = medv, y = lstat)) +
  geom_density_2d()

Boston%>%
ggplot(aes(x = medv, y = lstat)) +
  geom_density_2d()

Boston%>%
  mutate(high_age = ifelse(age > 77, "high", "low")) %>%
  ggplot(aes(x = high_age, y = medv)) +
  geom_boxplot()

Boston%>%
  mutate(high_age = case_when(age > 90 ~ "high" ,
                              age > 60 & age <=90 ~ "medium",
                              TRUE ~ "low"))%>%
ggplot(aes(x = high_age, y = medv)) + 
  geom_violin()

Boston %>%
  cor() %>%
  corrplot()

predict_mdev_1 <- lm(medv ~ lstat + age, data = Boston)
summary(predict_mdev_1)

predict_mdev_2 <- lm(medv ~ lstat + rm, data = Boston)
summary(predict_mdev_2)

Boston_expensive <- Boston%>%
  mutate(expensive = ifelse(medv >= 25, "expensive", "other")) %>%
  mutate(expensive = as.factor(expensive))

train_index <- createDataPartition(Boston_expensive$expensive,
                                   p = 0.8,
                                   times = 1,
                                   list = FALSE)
train_index <-  sample(c(1:nrow(Boston_expensive),0.8 * nrow(Boston_expensive)))

train_index <-  sample(c(1:nrow(Boston_expensive)),0.8 * nrow(Boston_expensive))
train_data <- Boston_expensive[train_index, ]
test_data <- Boston_expensive[-train_index, ]

logistic_model_1 <- glm(expensive ~ lstat + rm, 
                        data = train_data,
                        family = "binomial")
summary(logistic_model_1)

prediction_1 <- predict(logistic_model_1,
                         test_data, type = "response")

predict_1_factor 


##############################################
# Homework: using the dataset below, build a model to predict whether someone
# has a low credit rating or not. Build a confusion matrix to show the results.
# Push the results to github and create a pull request
credit_dataset <- Credit %>%
  
  
  mutate(low_credit_rating = ifelse(Rating < 247, "low_rating", "other")) %>% 
  mutate(low_credit_rating = as.factor(low_credit_rating))


predict_mdev_1 <- lm(Rating ~ Income + Age, data = credit_dataset)


summary(predict_mdev_1)


predict_mdev_2 <- lm(Rating ~ Income + Student, data = credit_dataset)
summary(predict_mdev_2)



train_index <-  sample(c(1:nrow(credit_dataset)),
                       0.8 * nrow(credit_dataset))
train_data <- credit_dataset[train_index, ]
test_data <- credit_dataset[-train_index, ]
test_data


logistic_model_1 <-  glm(low_credit_rating ~ Income + Age,
                         data = train_data,
                         family = "binomial")



# Question: which variables are significant?
summary(logistic_model_1)


#