library(datasets)
library(DescTools)

dados <- cars

media <- mean(cars$speed)

moda_dist <- Mode(cars$dist)
moda_speed <- Mode(cars$speed)

mediana_dist <- median(cars$dist)
mediana_speed <- median(cars$speed)

desvio_padrao_dist <- sd(cars$dist)
desvio_padrao_speed <- sd(cars$speed)

correlacao <- cor(cars$speed, cars$dist)
correlacao <- cor.test(cars$speed, cars$dist)

# 10 amostras da coluna speed
amostra_speed <- sample(cars$speed, 10, replace = FALSE)

# 10 amostras da coluna dist
amostra_dist <- sample(cars$dist, 10, replace = FALSE)

# 10 amostras da linha 1 a 50
amostra_index <- sample(1:50, 10, replace = FALSE)

amostras <- dados[amostra_index, ]

# Regressão linear

linear_model <- lm(dist ~ speed, data = cars)

summary(linear_model)

# Predição

novos_dados <- data.frame("speed" = c(21, 24, 26))

predict(linear_model, newdata = novos_dados, interval = "confidence")