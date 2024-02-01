library(tidyverse)
library(caret)
library(pROC)

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
train_df <- prep_df[[1]]
test_df <- prep_df[[2]]

## 2. Train
set.seed(24)
ctrl <- trainControl(method = "repeatedcv",
                     summaryFunction = defaultSummary,
                     classProbs = TRUE,
                     number = 10,
                     repeats = 5,
                     verboseIter = TRUE)

## 2.1 Train using Logistic Regression
glm_model <- train(churn ~ numbercustomerservicecalls + totaldaycalls + totalevecalls + totalnightcalls + totalintlcalls,
               data = train_df,
               method = "glm",
               trControl = ctrl)


## 2.2 Train using KNN
knn_model <- train(
  churn ~ numbercustomerservicecalls + totaldaycalls + totalevecalls + totalnightcalls + totalintlcalls,
  data = train_df,
  method = "knn",
  metric = "Accuracy",
  trControl = ctrl,
  tuneLength = 5
)
knn_model

training_set <- train_df %>%
  mutate(Predicted_prob = predict(knn_model, type = "prob")$Yes) %>%
  select(churn, Predicted_prob)

pROC_train <- roc(training_set$churn, training_set$Predicted_prob,
                  quiet = TRUE,
                  plot = TRUE,
                  percent = TRUE,
                  auc.polygon = TRUE,
                  print.auc = TRUE,
                  print.thres = TRUE,
                  print.thres.best.method = "youden")

## 3. Score
## 3.1 Predict Churn using Logistic Regression
pred_churn_glm <- predict(glm_model, newdata = test_df)

## 3.2 Predict Churn using KNN
pred_churn_knn <- predict(knn_model, newdata = test_df)

## 4. Evaluate
actual_churn <- test_df$churn

acc_glm <- mean(pred_churn_glm == actual_churn)
acc_glm

confusionMatrix(factor(pred_churn_glm),
                factor(actual_churn),
                positive = "Yes",
                mode = "prec_recall")

acc_knn <- mean(pred_churn_knn == actual_churn)
acc_knn

confusionMatrix(pred_churn_knn,
                factor(actual_churn),
                positive = "Yes",
                mode = "prec_recall")
