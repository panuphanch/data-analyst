library(tidyverse)
library(caret)
library(mlbench)


## load data
data("PimaIndiansDiabetes")

df <- PimaIndiansDiabetes

mean(complete.cases(df)) # 0 has missing value

## train model rpart 
## rpart need to tune cp
ctrl <- trainControl(method = "cv",
                     number = 5,
                     verboseIter = TRUE,
                     classProbs = TRUE, # we can change threshold away from 0.5
                     summaryFunction = twoClassSummary)

## recursive partitioning (decision tree)
tree_model <- train(diabetes ~ glucose + pressure + insulin + mass + age,
                    data = df,
                    method = "rpart",
                    metric = "ROC", # Area under the curve
                    trControl = ctrl)

## prediction
predict(tree_model, df, type = "prob")[1:10, ]

## change threshold
probs <- predict(tree_model, type = "prob")

## 0.5 is normal threshold
## to increase or decrease depends on subject
p_class <- ifelse(probs$pos >= 0.5, "pos", "neg") 

table(df$diabetes, p_class)


## random forest (bagging)
## Top 5 most valuable
rf_ctrl <- trainControl(method = "cv",
                     number = 5,
                     verboseIter = TRUE)

## To compare with Random Forest
ac_tree_model <- train(diabetes ~ glucose + pressure + insulin + mass + age,
                    data = df,
                    method = "rpart",
                    metric = "Accuracy", # Area under the curve
                    trControl = rf_ctrl)

## recursive partitioning (decision tree)
rf_model <- train(diabetes ~ glucose + pressure + insulin + mass + age,
                    data = df,
                    method = "rf", # ranger is other library for train random forest
                    metric = "Accuracy", # Area under the curve
                    tuneGrid = data.frame(mtry = c(2,3,4)),
                    trControl = rf_ctrl)


## Ridge vs. Lasso Regression
## Regularization
## Ridge => beta will be lower, but not zero
## Lasso => beta can be zero (feature selection)
glmnet_model <- train(diabetes ~ .,
                  data = df,
                  method = "glmnet",
                  metric = "Accuracy",
                  tuneGrid = expand.grid(
                    alpha = 0:1,
                    lambda = c(0.004, 0.04, 0.08)
                  ),
                  trControl = rf_ctrl)

## save model
saveRDS(glmnet_model, "ridge_lasso_reg.RDS")
