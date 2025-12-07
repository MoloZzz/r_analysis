# ============================================================
# LAB: Random Forest — подбор min.node.size (классификация)
# Автор: Євчик Олексій
# ============================================================
# Потрібні пакети: tidyverse, ranger, caret, pROC, pdp
# install.packages(c("tidyverse","ranger","caret","pROC","pdp"))
# ============================================================

library(tidyverse)
library(ranger)
library(caret)
library(pROC)
library(pdp)

set.seed(42)

# ======================
# 1) Підготовка даних
# ======================
# Використовуємо iris, але ціль робимо факторною для probability RF
df <- iris %>%
  mutate(target = ifelse(Species == "setosa", "setosa", "other")) %>%
  select(-Species) %>%
  mutate(target = factor(target, levels = c("other", "setosa")))

# Швидка перевірка
table(df$target)

# ======================
# 2) Train/Test split
# ======================
train_idx <- createDataPartition(df$target, p = 0.7, list = FALSE)
train <- df[train_idx, ]
test  <- df[-train_idx, ]

# ======================
# 3) Сітка min.node.size
# ======================
grid_min_node <- c(1, 3, 5, 7, 10)

results <- tibble(min_node = integer(), accuracy = double(), auc = double())
models_list <- list()

# ======================
# 4) Перебір параметрів
# ======================
for (m in grid_min_node) {
  cat("Running min.node.size =", m, "...\n")
  rf <- ranger(
    formula = target ~ .,
    data = train,
    num.trees = 500,
    mtry = 2,
    probability = TRUE,        # probability forest
    importance = "permutation",
    min.node.size = m,
    sample.fraction = 0.8,
    seed = 42
  )
  
  models_list[[as.character(m)]] <- rf
  
  # Прогнози: ranger при probability=TRUE повертає матрицю з колонками-мітками класів
  pred <- predict(rf, data = test)$predictions
  # Переконаємось, що колонка "setosa" існує
  if (!"setosa" %in% colnames(pred)) {
    stop("Очікувана колонка 'setosa' відсутня у pred$predictions. Перевірте цільовий фактор.")
  }
  probs_pos <- pred[, "setosa"]
  
  # Перевірка: чи не всі прогнози однакові (тоді AUC недостовірний)
  if (length(unique(probs_pos)) == 1) {
    warning(glue::glue("Увага: для min.node.size={m} всі ймовірності однакові -> AUC некоректний"))
  }
  
  # Класи за порогом 0.5
  pred_class <- factor(ifelse(probs_pos >= 0.5, "setosa", "other"), levels = levels(test$target))
  acc <- mean(pred_class == test$target)
  
  # AUC за pROC
  roc_obj <- try(roc(response = test$target, predictor = probs_pos, levels = c("other", "setosa"), direction = "<"), silent = TRUE)
  if (inherits(roc_obj, "try-error")) {
    auc_val <- NA_real_
    warning(glue::glue("Не вдалося порахувати ROC для min.node.size={m}"))
  } else {
    auc_val <- as.numeric(auc(roc_obj))
  }
  
  results <- bind_rows(results, tibble(min_node = m, accuracy = acc, auc = auc_val))
}

# ======================
# 5) Результати
# ======================
print(results)

best_row <- results %>% filter(!is.na(auc)) %>% arrange(desc(auc)) %>% slice(1)
if (nrow(best_row) == 0) {
  stop("Не знайдено валідних AUC (всі NA). Перевірте дані та прогнозні ймовірності.")
}
best_min_node <- best_row$min_node
cat("\nНайкращий min.node.size за AUC:", best_min_node, "\n")
best_model <- models_list[[as.character(best_min_node)]]

# ======================
# 6) Графіки
# ======================
# AUC ~ min.node.size
library(ggplot2)
ggplot(results, aes(x = factor(min_node), y = auc, group = 1)) +
  geom_line(aes(group=1)) + geom_point(size = 3) +
  labs(title = "AUC залежно від min.node.size", x = "min.node.size", y = "AUC") +
  theme_minimal()

# Importance (permutation importance, якщо доступна)
if (!is.null(best_model$variable.importance)) {
  imp_df <- enframe(best_model$variable.importance, name = "feature", value = "importance") %>%
    arrange(importance)
  ggplot(imp_df, aes(x = reorder(feature, importance), y = importance)) +
    geom_col() + coord_flip() +
    labs(title = paste0("Permutation importance (min.node.size=", best_min_node, ")"),
         x = "", y = "Importance") +
    theme_minimal()
} else {
  message("Variable importance not available for best model.")
}

# OOB error curve (якщо доступна)
if (!is.null(best_model$prediction.error)) {
  oob_df <- tibble(trees = seq_along(best_model$prediction.error), error = best_model$prediction.error)
  ggplot(oob_df, aes(trees, error)) + geom_line() +
    labs(title = "OOB error (по дере́вам)", x = "Number of trees", y = "OOB error") +
    theme_minimal()
} else {
  message("OOB prediction.error not available.")
}

# PDP для найважливішої фічі (якщо є)
top_feat <- NULL
if (!is.null(best_model$variable.importance)) {
  top_feat <- names(sort(best_model$variable.importance, decreasing = TRUE))[1]
}
if (!is.null(top_feat)) {
  pd <- partial(best_model, pred.var = top_feat, train = train, prob = TRUE)
  plotPartial(pd, main = paste("PDP for", top_feat))
}

# ======================
# 7) Confusion matrix для best model на тесті
# ======================
pred_best <- predict(best_model, data = test)$predictions[, "setosa"]
pred_class_best <- factor(ifelse(pred_best >= 0.5, "setosa", "other"), levels = levels(test$target))
cm <- table(Predicted = pred_class_best, Actual = test$target)
cat("\nConfusion matrix (best model):\n")
print(cm)

# ======================
# 8) Висновки (консоль)
# ======================
cat("\n=== ВИСНОВКИ ===\n")
cat("Результати (min.node.size, accuracy, AUC):\n")
print(results)
cat("\nОптимальне min.node.size за AUC: ", best_min_node, "\n")

# ======================
# 9) sessionInfo()
# ======================
cat("\nSession info:\n")
print(sessionInfo())