# Kristine's file


##############################################
# Homework: using the dataset below, build a model to predict whether someone
# has a low credit rating or not. Build a confusion matrix to show the results.
# Push the results to github and create a pull request

library(ISLR2)
library(tidyverse)
library(caret)
library(caTools)
library(corrplot)

?Credit 

head(Credit, 20)
str(Credit)
summary(Credit)


Credit %>%
  ggplot(aes(x = Rating))+
  geom_density()
  
# Look at the distribution of some variables of interest
Credit %>% 
  ggplot(aes( x = Income))+
  geom_density()


Credit %>% 
  ggplot( aes(x=Rating)) +
  geom_point(aes(y = Income),col ="red") +
  labs(title = "Dependency of Rating and Income", x = "Rating", y = "Income")


ggplot(Credit, aes(x=Rating)) + 
  geom_histogram(binwidth=1, color="blue")

Credit %>% 
  ggplot( aes(x=Education)) +
  geom_point(aes(y = Income),col ="green") +
  labs(title = "Dependency of Rating and Income", x = "Education", y = "Income")

Credit_dataset <- Credit %>% 
  mutate(Rating = ifelse(Rating < 247, "low_rating", "other")) %>% 
  mutate(Rating = as.factor(Rating))  
head(Credit_dataset,20)


num_var <- data.frame(Credit$Income, Credit$Limit, Credit$Rating,Credit$Balance, Credit$Education, Credit$Age )
names(num_var) <- c("Income", "Limit", "Rating","Balance",  "Education", "Age")
str(num_var)

num_var %>% 
  cor() %>% 
  corrplot()

# Logistic regression

set.seed(42)
train_index <- sample(c(1:nrow(Credit_dataset)), 
                      0.8 * nrow(Credit_dataset))

head(train_index, 10)

train_data <- Credit_dataset[train_index, ]
test_data <- Credit_dataset[-train_index, ]

head(test_data, 10)


dim(Credit_dataset)
dim(train_data)
dim(test_data)


logistic_model_1 <- glm(Rating ~ Income + Limit + Balance+ Age,
                        data = train_data,
                        family="binomial")

summary(logistic_model_1)


plot(logistic_model_1)


prediction_1 <- predict(logistic_model_1,
                        test_data, type = "response")


predict_1_factor <- factor(ifelse(prediction_1 > 0.5,
                           "low_rating", "other") )

head(predict_1_factor, 20)


head(train_data,20)

confusionMatrix(data=predict_1_factor, reference=test_data$Rating,
                positive="low_rating")

