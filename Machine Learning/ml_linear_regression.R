library(caret)

View(mtcars)

# split data
train_test_split <- function(data) {
  set.seed(24)
  n <- nrow(data)
  id <- sample(n, 0.8*n)
  train_data <- data[id, ]
  test_data <- data[-id, ]
  return (list(train_data, test_data))
}

split_data <- train_test_split(mtcars)


# train model
lm_model <- train(mpg ~ hp, 
      data = split_data[[1]],
      method = "lm") # lm = Linear regression

# score and evaluate
p <- predict(lm_model, newdata = split_data[[2]])

error <- split_data[[2]]$mpg - p
rmse <- sqrt(mean(error**2))
