library(tidyverse)
library(caret)

## Model glm predict churn.csv

## Split function
train_test_split <- function(data, size=0.8) {
  set.seed(24)
  n <- nrow(data)
  train_id <- sample(1:n, size*n)
  train_df <- data[train_id, ]
  test_df <- data[-train_id, ]
  return (list(train_df, test_df))
}

## Read data
df <- read_csv("churn.csv")
df

## 1. Split
prep_df <- train_test_split(df)

## 2. Train
ctrl <- trainControl(method = "cv",
                     number = 5)
model <- train(churn ~ numbercustomerservicecalls + totaldaycalls + totalevecalls + totalnightcalls + totalintlcalls,
              data = prep_df[[1]],
              method = "glm",
              trControl = ctrl)
model

## 3. Score
pred_churn <- predict(model, data = prep_df[[1]])

## 4. Evaluate
actual_churn <- prep_df[[2]]$churn

acc <- mean(pred_churn == prep_df[[2]]$churn)
acc