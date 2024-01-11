library(caret)

# split data
train_test_split <- function(data) {
  set.seed(24)
  n <- nrow(data)
  id <- sample(n, 0.8*n)
  train_data <- data[id, ]
  test_data <- data[-id, ]
  return (list(train_data, test_data))
}

data("mtcars")

# prepare data
mtcars$am <- factor(mtcars$am,
                    levels = c(0, 1),
                    labels = c("Auto", "Manual"))

split_data <- train_test_split(mtcars)


# train model
glm_model <- train(am ~ mpg, 
                  data = split_data[[1]],
                  method = "glm") # glm = classification

# score and evaluate
p <- predict(glm_model, newdata = split_data[[2]])

acc <- mean(p == split_data[[2]]$am)
