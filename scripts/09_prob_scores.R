library(data.table)
library(stringr)
library(reticulate)
library(quanteda)
library(purrr)
library(tidyverse)
library(jsonlite)
library(magrittr)
library(caret)

# First I need to get the binary classification for the labelled opinions
df <- fread("./data/opinions_text_01_13_24.csv")
df_seps <- df[type != "opinion"]
setorder(df_seps, scdb_id)

use_virtualenv("./.env")
transformers <- import("transformers")
tokenizer <- transformers$AutoTokenizer$from_pretrained(
  "nlpaueb/legal-bert-base-uncased",
  truncation = TRUE
)
pipeline <- transformers$pipeline(
  "text-classification",
  model = "./output/models/01_12_24_paragraph_control_lbert_4_document",
  tokenizer = tokenizer,
  truncation = TRUE
)

compute_activism <- function(data_row) {
  id <- data_row[1]
  print(paste("Computing Activism on Case", id))
  text <- data_row[5]

  # Split text into sentences
  paragraphs <- corpus(text) %>%
    corpus_reshape(to = "paragraphs")

  # Predict every sentence
  output <- sapply(paragraphs, function(x) pipeline(x)[[1]])


  output <- as.data.table(t(output))
  output[, label := as.character(label)]
  output[, score := as.numeric(score)]

  output[, score := ifelse(label == "not activism", 1 - score, score)]
  output[, label := ifelse(label == "not activism", "activism", label)]

  output[, words := ntoken(paragraphs)]
  output[, weight := words / sum(words)]
  
  # calculate weighted probability and flat probability
  prob <- mean(output$score)
  prob_w <- sum(output$score * output$weight)

  # Calculate based on threshold
  prob_thresh <- mean(output$score > 0.974)
  prob_w_thresh <- sum((output$score > 0.974) * output$weight)

  bin_activist <- any(output$score > 0.5)
  bin_thresh_activist <- any(output$score > 0.974)

  # Calculate based on threshold - paragraph
  prob_thresh_pg <- mean(output$score > 0.84)
  prob_w_thresh_pg <- sum((output$score > 0.84) * output$weight)

  bin_thresh_activist_pg <- any(output$score > 0.84)

  # Return vector with values
  return(
    c(
      data_row[1],
      data_row[2],
      data_row[3],
      data_row[4],
      prob_activist = prob,
      prob_activist_w = prob_w,
      prob_activist_thresh = prob_thresh,
      prob_activist_thresh_pg = prob_thresh_pg,
      prob_activist_w_thresh = prob_w_thresh,
      prob_activist_w_thresh_pg = prob_w_thresh_pg,
      bin_activist = bin_activist,
      bin_activist_thresh = bin_thresh_activist,
      bin_activist_thresh_pg = bin_thresh_activist_pg
    )
  )
}

act <- apply(df_seps, 1, compute_activism)

act <- t(act)

test <- as.data.table(act)
test

fwrite(test, "./output/data/final_data_opinion_activism_document_01_13_24_fixed.csv")
