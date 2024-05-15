library(data.table)
library(caret)
library(tidyverse)
library(broom)

eval_df <- data.frame(
  model = character(0),
  epochs = numeric(0),
  accuracy = numeric(0),
  kappa = numeric(0),
  precision = numeric(0),
  recall = numeric(0),
  f1 = numeric(0)
)

thresh_df <- data.frame(
  model = character(0),
  epochs = numeric(0),
  accuracy = numeric(0),
  kappa = numeric(0),
  precision = numeric(0),
  recall = numeric(0),
  f1 = numeric(0)
)
for (model in c("bert", "roberta", "lbert")) {
  for (i in 1:5) {
    df <- fread(
      paste0(
        "output/validate/validate_01_12_24_sentence_control_",
        model,
        "_",
        i,
        "_document.csv"
      )
    )
    # Baseline
    df[, true := factor(true, levels = c(0:1))]
    df[, pred := factor(pred, levels = c(0:1))]
    df[, thresh_pred := ifelse(pred == 1 & score > 0.85, 1, 0)]
    df[, thresh_pred := factor(thresh_pred, levels = c(0:1))]

    cm <- confusionMatrix(df$true, df$pred, positive = "1", mode = "everything")
    tidy_cm <- tidy(cm)
    eval_row <- data.frame(
      model = model,
      epochs = i,
      accuracy = tidy_cm[[1, "estimate"]],
      kappa = tidy_cm[[2, "estimate"]],
      precision = tidy_cm[[8, "estimate"]],
      recall = ifelse(!is.na(tidy_cm[[9, "estimate"]]), tidy_cm[[9, "estimate"]], 0),
      f1 = ifelse(!is.na(tidy_cm[[10, "estimate"]]), tidy_cm[[10, "estimate"]], 0)
    )

    eval_df <- rbind(eval_df, eval_row)

    # Threshold
    cm <- confusionMatrix(df$true, df$thresh_pred, positive = "1", mode = "everything")
    tidy_cm <- tidy(cm)
    thresh_row <- data.frame(
      model = model,
      epochs = i,
      accuracy = tidy_cm[[1, "estimate"]],
      kappa = tidy_cm[[2, "estimate"]],
      precision = tidy_cm[[8, "estimate"]],
      recall = ifelse(!is.na(tidy_cm[[9, "estimate"]]), tidy_cm[[9, "estimate"]], 0),
      f1 = ifelse(!is.na(tidy_cm[[10, "estimate"]]), tidy_cm[[10, "estimate"]], 0)
    )

    thresh_df <- rbind(thresh_df, thresh_row)
  }
}

fwrite(eval_df, "./output/appendix/compare_llm_sentence.csv")
fwrite(thresh_df, "./output/appendix/compare_llm_thresh_sentence.csv")

df <- fread("output/validate/validate_01_12_24_paragraph_control_lbert_4_document.csv")
df[, true := factor(true, levels = c(0:1))]
df[, pred := factor(pred, levels = c(0:1))]


compare_threshold <- data.frame(
  threshold = numeric(0),
  accuracy = numeric(0),
  kappa = numeric(0),
  precision = numeric(0),
  recall = numeric(0),
  f1 = numeric(0)
)

for (i in seq(0.5, 0.999, 0.001)) {
  df[, thresh_pred := ifelse(pred == 1 & score > i, 1, 0)]
  df[, thresh_pred := factor(thresh_pred, levels = c(0:1))]

  cm <- confusionMatrix(df$true, df$thresh_pred, positive = "1", mode = "everything")
  tidy_cm <- tidy(cm)
  compare_row <- data.frame(
    threshold = i,
    accuracy = tidy_cm[[1, "estimate"]],
    kappa = tidy_cm[[2, "estimate"]],
    precision = tidy_cm[[8, "estimate"]],
    recall = ifelse(!is.na(tidy_cm[[9, "estimate"]]), tidy_cm[[9, "estimate"]], 0),
    f1 = ifelse(!is.na(tidy_cm[[10, "estimate"]]), tidy_cm[[10, "estimate"]], 0)
  )

  compare_threshold <- rbind(compare_threshold, compare_row)
}

compare_threshold

fwrite(compare_threshold, "./output/appendix/threshold_lbert_4.csv")

eval_df <- data.frame(
  fold = numeric(0),
  accuracy = numeric(0),
  kappa = numeric(0),
  precision = numeric(0),
  recall = numeric(0),
  f1 = numeric(0)
)

thresh_df <- data.frame(
  fold = numeric(0),
  accuracy = numeric(0),
  kappa = numeric(0),
  precision = numeric(0),
  recall = numeric(0),
  f1 = numeric(0)
)

for (i in 0:4) {
  df <- fread(paste0("output/validate/validate_01_12_24_paragraph_kfold_lbert_", i, "_document.csv"))

  # Baseline
  df[, true := factor(true, levels = c(0:1))]
  df[, pred := factor(pred, levels = c(0:1))]
  df[, thresh_pred := ifelse(pred == 1 & score > 0.85, 1, 0)]
  df[, thresh_pred := factor(thresh_pred, levels = c(0:1))]

  cm <- confusionMatrix(df$true, df$pred, positive = "1", mode = "everything")
  tidy_cm <- tidy(cm)
  eval_row <- data.frame(
    fold = i,
    accuracy = tidy_cm[[1, "estimate"]],
    kappa = tidy_cm[[2, "estimate"]],
    precision = tidy_cm[[8, "estimate"]],
    recall = ifelse(!is.na(tidy_cm[[9, "estimate"]]), tidy_cm[[9, "estimate"]], 0),
    f1 = ifelse(!is.na(tidy_cm[[10, "estimate"]]), tidy_cm[[10, "estimate"]], 0)
  )

  eval_df <- rbind(eval_df, eval_row)

  # Threshold
  cm <- confusionMatrix(df$true, df$thresh_pred, positive = "1", mode = "everything")
  tidy_cm <- tidy(cm)
  thresh_row <- data.frame(
    fold = i,
    accuracy = tidy_cm[[1, "estimate"]],
    kappa = tidy_cm[[2, "estimate"]],
    precision = tidy_cm[[8, "estimate"]],
    recall = ifelse(!is.na(tidy_cm[[9, "estimate"]]), tidy_cm[[9, "estimate"]], 0),
    f1 = ifelse(!is.na(tidy_cm[[10, "estimate"]]), tidy_cm[[10, "estimate"]], 0)
  )

  thresh_df <- rbind(thresh_df, thresh_row)
}

fwrite(eval_df, "./output/appendix/kfold_lbert_paragraph.csv")
fwrite(thresh_df, "./output/appendix/kfold_lbert_thresh_paragraph.csv")


df <- fread("output/validate/validate_01_12_24_paragraph_control_lbert_3_document.csv")
text <- read.csv("./data/training_data/01_03_24_eval_with_ids_paragraph_document.csv")

df$X <- df$X + 1
df[, true := factor(true, levels = c(0:1))]
df[, pred := factor(pred, levels = c(0:1))]
df$id <- text$id

compare_threshold <- data.frame(
  threshold = numeric(0),
  accuracy = numeric(0),
  kappa = numeric(0),
  precision = numeric(0),
  recall = numeric(0),
  f1 = numeric(0)
)

for (i in seq(0.5, 0.999, 0.001)) {
  document_level <- df %>%
    group_by(id) %>%
    summarise(
      true = ifelse(any(true == 1), 1, 0),
      thresh_pred = ifelse(any(pred == 1 & score > i), 1, 0)
    ) %>%
    as.data.table()
  document_level[, thresh_pred := factor(thresh_pred, levels = c(0:1))]
  document_level[, true := factor(true, levels = c(0:1))]

  cm <- confusionMatrix(
    document_level$true,
    document_level$thresh_pred,
    positive = "1",
    mode = "everything"
  )
  tidy_cm <- tidy(cm)
  compare_row <- data.frame(
    threshold = i,
    accuracy = tidy_cm[[1, "estimate"]],
    kappa = tidy_cm[[2, "estimate"]],
    precision = tidy_cm[[8, "estimate"]],
    recall = ifelse(!is.na(tidy_cm[[9, "estimate"]]), tidy_cm[[9, "estimate"]], 0),
    f1 = ifelse(!is.na(tidy_cm[[10, "estimate"]]), tidy_cm[[10, "estimate"]], 0)
  )

  compare_threshold <- rbind(compare_threshold, compare_row)
}

fwrite(compare_threshold, "./output/appendix/threshold_lbert_3_document.csv")
