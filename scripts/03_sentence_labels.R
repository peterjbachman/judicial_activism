library(data.table)
library(jsonlite)
library(tidyverse)
library(magrittr)

test <- read_json("./data/fine_tune_01_02_24_test.json", simplifyVector = TRUE, flatten = TRUE)

test %>%
  select(id, data.type, data.author) %>%
  fwrite("./output/data/id_info.csv")

df <- lapply(test[, "annotations"], as.data.table)
df <- rbindlist(df)
df
df <- df %>%
  subset(last_action %in% c("accepted", "fixed_and_accepted"))
df %<>% group_by(task) %>%
  arrange(id) %>%
  slice(head(row_number(), 1)) %>%
  ungroup() %>%
  as.data.table()

df
labels <- rbindlist(df[, "result"][[1]], idcol = "ID")[, -c("id")]

for (i in seq_along(df$task)) {
  labels$id[labels$ID == i] <- df$task[i]
}

labels$value.labels <- as.character(labels$value.labels)

labels <- labels %>%
  mutate(
    labels_simp = ifelse(
      value.labels %in% c(
        "Diversions from conventional interpretations",
        "Diversions from precedent",
        "Legislating from the bench",
        "Striking down constitutional acts from the legislature or executive"
      ),
      "institutional",
      ifelse(
        value.labels == "Ruling based on preferences and not on rules",
        "ideological",
        ifelse(
          value.labels %in% c("Neither", "Call on Congress to Act"),
          "neither",
          NA
        )
      )
    )
  )
summary(factor(labels$labels_simp))
labels$labels_binary <- ifelse(labels$labels_simp %in% c("institutional", "ideological"), 1, 0)
labels
fine_tune <- labels[, c("value.text", "labels_binary", "id")] %>%
  rename(.,
    text = value.text,
    label = labels_binary
  )

# By Document ------------------------------------------------------------------
document_level_eval <- fine_tune %>%
  group_by(id) %>%
  summarise(has_activism = ifelse(any(label == 1), 1, 0))
document_level_eval %<>% as.data.table()

activism_eval_no <- round(nrow(document_level_eval[has_activism == 1]) * 0.25)
na_eval_no <- round(nrow(document_level_eval[has_activism == 0]) * 0.25)

activism_eval_no
na_eval_no

set.seed(1996)
eval_df <- document_level_eval[has_activism == 1] %>%
  sample_n(activism_eval_no, replace = FALSE)

eval_df <- fine_tune[id %in% eval_df$id, ]

eval_na <- document_level_eval[has_activism == 0] %>%
  sample_n(na_eval_no, replace = FALSE)

eval_na <- fine_tune[id %in% eval_na$id, ]

eval_df <- rbind(eval_df, eval_na)
eval_df

fine_tune_df <- anti_join(fine_tune, eval_df)

# Do case-control stuff
activism_labels <- fine_tune_df[label == 1]
activism_labels

control_no <- round(nrow(activism_labels) / 0.30) - nrow(activism_labels)
control_no
set.seed(1996)
neither_labels <- sample_n(fine_tune_df[label == 0], control_no, replace = FALSE)

fine_tune_control <- rbind(activism_labels, neither_labels)
fwrite(fine_tune_control[text != "", -c("id")], "./data/training_data/01_03_24_control_sentence_document.csv")
fwrite(fine_tune_df[text != "", -c("id")], "./data/training_data/01_03_24_full_sentence_document.csv")
fwrite(eval_df[text != "", -c("id")], "./data/training_data/01_03_24_eval_sentence_document.csv")
fwrite(eval_df[text != "", ], "./data/training_data/01_03_24_eval_with_ids_sentence_document.csv")
fwrite(fine_tune[text != "", ], "./data/training_data/01_03_24_full_with_ids_sentence_document.csv")

# Random -----------------------------------------------------------------------
activism_eval_no <- round(nrow(fine_tune[label == 1]) * 0.25)
na_eval_no <- round(nrow(fine_tune[label == 0]) * 0.25)

activism_eval_no
na_eval_no


set.seed(1996)
eval_df <- fine_tune[label == 1] %>%
  sample_n(activism_eval_no, replace = FALSE)

eval_na <- fine_tune[label == 0] %>%
  sample_n(na_eval_no, replace = FALSE)

eval_df <- rbind(eval_df, eval_na)

fine_tune_df <- anti_join(fine_tune, eval_df)

# Do case-control stuff
activism_labels <- fine_tune_df[label == 1]
activism_labels

control_no <- round(nrow(activism_labels) / 0.35) - nrow(activism_labels)
control_no
set.seed(1996)
neither_labels <- sample_n(fine_tune_df[label == 0], control_no, replace = FALSE)

fine_tune_control <- rbind(activism_labels, neither_labels)
fwrite(fine_tune_control[text != "", -c("id")], "./data/training_data/01_02_24_control.csv")
fwrite(fine_tune_df[text != "", -c("id")], "./data/training_data/01_02_24_full.csv")
fwrite(eval_df[text != "", -c("id")], "./data/training_data/01_02_24_eval.csv")
fwrite(eval_df[text != "", ], "./data/training_data/01_02_24_eval_with_ids.csv")
fwrite(fine_tune[text != "", ], "./data/training_data/01_02_24_full_with_ids.csv")

# Training data

test <- read_json("./data/fine_tune_dobbs_12_5_23.json", simplifyVector = TRUE, flatten = TRUE)

df <- lapply(test[, "annotations"], as.data.table)
df <- rbindlist(df)

labels <- rbindlist(df[, "result"][[1]], idcol = "ID")[, -c("id")]

for (i in seq_along(df$task)) {
  labels$id[labels$ID == i] <- df$task[i]
}

labels$value.labels <- as.character(labels$value.labels)

labels

labels$labels_binary <- ifelse(labels$value.labels %in% c("institutional", "ideological"), 1, 0)

labels <- labels[, c("value.text", "labels_binary", "id")] %>%
  rename(.,
    text = value.text,
    label = labels_binary
  )


fwrite(labels, "./data/training_data/dobbs_12_28_23.csv")

df[!is_empty(df$reviews), ]

test <- df[, reviews]

View(test[[1]])
