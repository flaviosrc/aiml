require("class")
require("datasets")
data("iris")

rnum <- sample(rep(1:150))
iris <- iris[rnum, ]

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

iris_new <- as.data.frame(lapply(iris[, c(1, 2, 3, 4)], normalize))

head(iris_new)

iris_train <- iris_new[1:120, ]
iris_train_target <- iris[1:120, 5]
iris_test <- iris_new[121:150, ]
iris_test_target <- iris[121:150, 5]

summary(iris_new)

model1 <- knn(
  train = iris_train,
  test = iris_test,
  cl = iris_train_target,
  k = 7
)

table(iris_test_target, model1)