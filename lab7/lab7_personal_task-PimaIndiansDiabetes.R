# LAB: Random Forest — подбор min.node.size (классификация)
# Автор: Євчик Олексій

# Пакети
library(mlbench)
library(ranger)
library(pROC)
library(ggplot2)
library(dplyr)
library(purrr)
library(tibble)

# Дані
data("PimaIndiansDiabetes")
df <- PimaIndiansDiabetes
df$diabetes <- as.factor(df$diabetes)

# Параметри для дослідження
mtry_values <- c(1, 2, 3, 4, 5, 6, 7, 8)
min_node_values <- c(1, 5, 10, 20, 30)
trees_values <- c(50, 100, 200, 300, 500)

# Функція отримання OOB та AUC
evaluate_model <- function(mtry, min_node, trees) {
  
  model <- ranger(
    diabetes ~ .,
    data = df,
    mtry = mtry,
    min.node.size = min_node,
    num.trees = trees,
    probability = TRUE,
    seed = 123,
    oob.error = TRUE
  )
  
  # OOB error
  oob_error <- model$prediction.error
  
  # OOB Probabilities
  probs <- model$predictions[, "pos"]
  
  # AUC
  auc <- auc(df$diabetes, probs)
  
  tibble(
    mtry = mtry,
    min_node = min_node,
    trees = trees,
    oob_error = oob_error,
    auc = as.numeric(auc)
  )
}

# Запускаємо експеримент
results <- cross_df(list(mtry = mtry_values,
                         min_node = min_node_values,
                         trees = trees_values)) %>%
  pmap_dfr(~ evaluate_model(..1, ..2, ..3))

# -------- ГРАФІКИ ---------

# AUC vs mtry
ggplot(results, aes(x = mtry, y = auc, color = factor(min_node))) +
  geom_line() +
  geom_point() +
  labs(title = "AUC залежно від mtry та min.node.size",
       color = "min.node.size") +
  theme_minimal()

# OOB error vs mtry
ggplot(results, aes(x = mtry, y = oob_error, color = factor(min_node))) +
  geom_line() +
  geom_point() +
  labs(title = "OOB Error залежно від mtry та min.node.size",
       color = "min.node.size") +
  theme_minimal()

# AUC vs кількість дерев
ggplot(results, aes(x = trees, y = auc, color = factor(mtry))) +
  geom_line() +
  geom_point() +
  labs(title = "AUC залежно від числа дерев",
       color = "mtry") +
  theme_minimal()
