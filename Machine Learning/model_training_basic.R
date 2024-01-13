library(tidyverse)
library(caret)

## preview data
head(mtcars)

## 1. split data
train_test_split <- function(data, size=0.8) {
  set.seed(24)
  n <- nrow(data)
  train_id <- sample(1:n, size*n)
  train_df <- data[train_id, ]
  test_df <- data[-train_id, ]
  return (list(train_df, test_df))
}

prep_df <- train_test_split(mtcars, 0.8)


## 2. train model
## boot stands for Bootstrapped
## LOOCV stands for Leave one out cv
## cv stands for K-Fold CV * default number is 5
ctrl <- trainControl(method = "cv",
                     number = 5)
  
model <- train(mpg ~ hp + wt + am,
               data = prep_df[[1]],
               method = "lm",
               trControl = ctrl)

## 3. score model
pred_mpg <- predict(model, newdata = prep_df[[2]])

## 4. evaluate model
actual_mpg <- prep_df[[2]]$mpg

## error = actual - prediction
test_mae <- mean(abs(pred_mpg - actual_mpg))
test_rmse <- sqrt(mean((pred_mpg - actual_mpg) ** 2))

print(test_mae)
print(test_rmse)
