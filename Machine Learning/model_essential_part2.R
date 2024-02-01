## find the best hyperparameter that improve model performance

library(tidyverse)
library(caret)
library(mlbench)

data("BostonHousing")
data("PimaIndiansDiabetes")

## review train
avg_hp <- mean(mtcars$hp)
avg_wt <- mean(mtcars$wt)
clean_df <- mtcars %>% 
  select(mpg, hp, wt, am) %>%
  ## imputation (mean, median)
  mutate(hp = replace_na(hp, avg_hp),
         wt = replace_na(wt, avg_wt)) %>%
  ## rule of thumb: NA < 3-5%
  drop_na()
  

## linear regression
lm_model <- train(
  mpg ~ .,
  data = clean_df,
  method = "lm"
)

## decision tree
set.seed(24) ## lock result
dct_model <- train(
  mpg ~ .,
  data = clean_df,
  method = "rpart",
  metric = "RMSE" # Close to zero
)

## knn: k-nearest neighbors
## Experimentation
set.seed(24) ## lock result

## grid search (dataframe)
## default knn will use 5, 7, 9
k_grid <- data.frame(k = c(3, 5, 7))

ctrl <- trainControl(
  method = "cv", #"repeatedcv",
  number = 3,
  # number = 5, # K (5, 7, 9)
  verboseIter = TRUE, # log result each iteration (progress bar)
)

knn_model <- train(
  mpg ~ .,
  data = clean_df,
  method = "knn",
  metric = "RMSE", # Close to zero
  trControl = ctrl,
  tuneGrid = k_grid
  # tuneLength = 4 # When we don't know k to choose
)

## save model
## .RDS extension
saveRDS(knn_model, "knnModel.RDS")

## normalization
## 1. min-max (feature scaling 0-1)
## 2. standardization -3, +3

x <- c(5, 10, 12, 15, 20)

minMaxNorm <- function(x) {
  (x-min(x)) / (max(x) - min(x))
}

# center and scale
# standardization
zNorm <- function(x) {
  (x-mean(x)) / sd(x)
}

## preProcess()
train_df <- mtcars[1:20, ]
test_df <- mtcars[21:32, ]

## compute x_bar, x_sd
## learn from train dataset not full dataset
transformer <- preProcess(train_df,
                          method = c("center", "scale"))

## normalization
train_df_z <- predict(transformer, train_df)
test_df_z <- predict(transformer, test_df)

## min max scaling [0,1]
min_max_transformer <- preProcess(train_df,
                                  method = c("range"))

## normalization
train_df_range <- predict(min_max_transformer, train_df)
test_df_range <- predict(min_max_transformer, test_df)
