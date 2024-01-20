## build ML to classify diabetes patients
## binary classification
library(tidyverse)
library(caret)
library(mlbench)
library(MLmetrics)

## load dataset
data("PimaIndiansDiabetes")
df <- PimaIndiansDiabetes

## explore dataset
glimpse(df)

## no missing value
mean(complete.cases(df)) ## if 1 means "no missing value"
## complete.cases(data_frame) will return TRUE is row has no missing value

## select variables
df_starter <- df %>%
  select(2, 5, 6, 8, 9) # glucose, insulin, mass, age, diabetes

## summarize correlation for age, mass to diabetes
## compare by mean
df_starter %>%
  group_by(diabetes) %>%
  summarise(mean(age), mean(mass))


## 1. Split data
set.seed(24) # lock result in split data
n <- nrow(df_starter)
id <- sample(x = 1:n,
             size = 0.8 * n)
train_df <- df_starter[id, ]
test_df <- df_starter[-id, ]

## 2. train
set.seed(24) # lock result in train model

## use for Recall, Precision, F1, AUC
ctrl <- trainControl(method = "cv",
                     number = 5,
                     ## pr = precision + recall
                     summaryFunction = prSummary,
                     classProbs = TRUE)
## use for Accuracy
acc_ctrl <- trainControl(method = "cv",
                     number = 5)

logis_model <- train(diabetes ~ .,
                     data = train_df,
                     method = "glm",
                     metric = "Recall",
                     trControl = ctrl)

knn_model <- train(diabetes ~ .,
                   data = train_df,
                   method = "knn",
                   metric = "Recall",
                   trControl = ctrl)

## baseline accuracy
## no model accuracy
nrow(train_df)
table(train_df$diabetes)
399 / 614 # 0.6498371
## our logis_model better than normal by 0.7655338 - 0.6498371

## 3. score
p <- predict(logis_model, newdata = test_df)

## 4. evaluate
mean(p == test_df$diabetes)

## confusion matrix manual
table(test_df$diabetes, p, dnn = c("actual", "preds"))

## confusion matrix
## recall, precision, f1-score
confusionMatrix(p, 
                test_df$diabetes, 
                positive = "pos",
                mode = "prec_recall")

## recall: Among the patients, how many did the model predict correctly?