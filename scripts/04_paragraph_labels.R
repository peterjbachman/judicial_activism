library(data.table)
library(jsonlite)
library(tidyverse)
library(stringr)
library(magrittr)


test <- read_json("./data/fine_tune_01_02_24_test.json", simplifyVector = TRUE, flatten = TRUE)

df <- as.data.table(test[, c("id", "data.text")]) %>%
  mutate(data.text = strsplit(as.character(data.text), "\\n\\n")) %>%
  unnest(data.text) %>%
  rename(text = data.text)

sentence_labels <- fread("./data/training_data/01_02_24_full_with_ids.csv")

activism_sentences <- sentence_labels %>%
  filter(label > 0)

df$label <- 0

for (i in unique(activism_sentences$id)) {
  # Put all sentences in a vector
  all_sent <- str_squish(gsub(
    "[^[:alnum:] ]", "", activism_sentences$text[activism_sentences$id == i]
  ))
  for (j in df$text[df$id == i]) {
    if (any(
      sapply(
        all_sent,
        function(x, y) grepl(x, str_squish(gsub("[^[:alnum:] ]", "", y))),
        y = j
      )
    )) {
      df$label[df$text == j] <- 1
    }
  }
}

fwrite(df[df$text != "", c("text", "label", "id")], "./data/training_data/01_03_24_paragraph.csv")
fine_tune <- as.data.table(df)

fine_tune_trimmed <- fine_tune %>%
  filter(str_length(text) > 50)

# Document-level ---------------------------------------------------------------
document_level_eval <- fine_tune_trimmed %>%
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

eval_df <- fine_tune_trimmed[id %in% eval_df$id, ]

eval_na <- document_level_eval[has_activism == 0] %>%
  sample_n(na_eval_no, replace = FALSE)

eval_na <- fine_tune_trimmed[id %in% eval_na$id, ]

eval_df <- rbind(eval_df, eval_na)

fine_tune_df <- anti_join(fine_tune_trimmed, eval_df)

# Do case-control stuff
activism_labels <- fine_tune_df[label == 1]
activism_labels

control_no <- round(nrow(activism_labels) / 0.30) - nrow(activism_labels)
control_no
set.seed(1996)
neither_labels <- sample_n(fine_tune_df[label == 0], control_no, replace = FALSE)

fine_tune_control <- rbind(activism_labels, neither_labels)
fwrite(fine_tune_control[text != "", -c("id")], "./data/training_data/01_03_24_control_paragraph_document.csv")
fwrite(fine_tune_df[text != "", -c("id")], "./data/training_data/01_03_24_full_paragraph_document.csv")
fwrite(eval_df[text != "", -c("id")], "./data/training_data/01_03_24_eval_paragraph_document.csv")
fwrite(eval_df[text != "", ], "./data/training_data/01_03_24_eval_with_ids_paragraph_document.csv")
fwrite(fine_tune_trimmed[text != "", ], "./data/training_data/01_03_24_full_with_ids_paragraph_document.csv")

# Paragraph-level --------------------------------------------------------------
activism_eval_no <- round(nrow(fine_tune_trimmed[label == 1]) * 0.25)
na_eval_no <- round(nrow(fine_tune_trimmed[label == 0]) * 0.25)

activism_eval_no
na_eval_no

set.seed(1996)
eval_df <- fine_tune_trimmed[label == 1] %>%
  sample_n(activism_eval_no, replace = FALSE)

eval_na <- fine_tune_trimmed[label == 0] %>%
  sample_n(na_eval_no, replace = FALSE)

eval_df <- rbind(eval_df, eval_na)

fine_tune_df <- anti_join(fine_tune_trimmed, eval_df)

# Do case-control stuff
activism_labels <- fine_tune_df[label == 1]
activism_labels

control_no <- round(nrow(activism_labels) / 0.25) - nrow(activism_labels)
control_no
set.seed(1996)
neither_labels <- sample_n(fine_tune_df[label == 0], control_no, replace = FALSE)

fine_tune_control <- rbind(activism_labels, neither_labels)
fwrite(fine_tune_control[text != "", -c("id")], "./data/training_data/01_03_24_control_paragraph.csv")
fwrite(fine_tune_df[text != "", -c("id")], "./data/training_data/01_03_24_full_paragraph.csv")
fwrite(eval_df[text != "", -c("id")], "./data/training_data/01_03_24_eval_paragraph.csv")
fwrite(eval_df[text != "", ], "./data/training_data/01_03_24_eval_with_ids_paragraph.csv")
fwrite(fine_tune_trimmed[text != "", ], "./data/training_data/01_03_24_full_with_ids_paragraph.csv")



dissent <- readLines("./data/dobbs_dissent.txt", warn = FALSE)

validate <- data.frame(text = dissent[dissent != ""])
validate$opinion <- "dissent"
validate$label <- 0
concur <- readLines("./data/dobbs_concur.txt", warn = FALSE)
concurrences <- data.frame(text = concur[concur != ""], opinion = "concur", label = 0)
validate <- rbind(validate, concurrences)

sentence_validate <- fread("./data/training_data/dobbs_12_5_23.csv") %>%
  filter(label %in% c(1, 2))
validate$id <- ifelse(validate$opinion == "dissent", 79521948, 79521947)
validate
for (i in unique(sentence_validate$id)) {
  # Put all sentences in a vector
  all_sent <- str_squish(gsub(
    "[^[:alnum:] ]", "", sentence_validate$text[sentence_validate$id == i]
  ))
  for (j in validate$text[validate$id == i]) {
    if (any(
      sapply(
        all_sent,
        function(x, y) grepl(x, str_squish(gsub("[^[:alnum:] ]", "", y))),
        y = j
      )
    )) {
      validate$label[validate$text == j] <- 1
    }
  }
}

validate
fwrite(validate[validate$text != "", c("text", "label", "id")], "./data/training_data/12_11_23_dobbs_paragraph.csv")

