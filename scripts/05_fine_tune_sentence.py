"""
Fine-Tune Legal BERT model
"""
from transformers import AutoTokenizer
from transformers import AutoModelForSequenceClassification
from transformers import TrainingArguments
from transformers import Trainer
from transformers import pipeline
from transformers import set_seed
from datasets import load_dataset
import numpy as np
import evaluate
import pandas as pd
from sklearn.metrics import accuracy_score, precision_recall_fscore_support

# Functions
def tokenize_function(examples):
    '''Tokenize baby'''
    return tokenizer(
        examples["text"], padding="max_length", truncation=True, max_length=512
    )

def compute_metrics(eval_pred):
    '''compute stats'''
    logits, labels = eval_pred
    predictions = np.argmax(logits, axis=-1)
    precision, recall, f1, _ = precision_recall_fscore_support(labels, predictions, average='binary', pos_label=1)
    acc = accuracy_score(labels, predictions)
    return {
        'accuracy': acc,
        'f1': f1,
        'precision': precision,
        'recall': recall,
        'matthews correlation': metric.compute(predictions=predictions, references=labels)["matthews_correlation"]
    }

# def compute_metrics(eval_pred):
#     '''Prediction function'''
#     logits, labels = eval_pred
#     predictions = np.argmax(logits, axis=-1)
#     return metric.compute(predictions=predictions, references=labels)



# Training objects
metric = evaluate.load("matthews_correlation")
# metric = evaluate.load("precision", zero_division=0)
# metric = evaluate.load("f1")

# Load in Dataset, clean it
data_files = {
    "train": "./data/training_data/01_03_24_control_sentence_document.csv",
    "full_eval": "./data/training_data/01_03_24_eval_sentence_document.csv",
}
dataset = load_dataset("csv", data_files=data_files)

tokenizer = AutoTokenizer.from_pretrained("nlpaueb/legal-bert-base-uncased")

tokenized_data = dataset.map(tokenize_function, batched=True)

train_dataset = tokenized_data["train"]
eval_dataset = tokenized_data["full_eval"]

id2label = {0: "not activism", 1: "activism"}
label2id = {"not activism": 0, "activism": 1}

model = AutoModelForSequenceClassification.from_pretrained(
    "nlpaueb/legal-bert-base-uncased",
    num_labels=2,
    id2label=id2label,
    label2id=label2id,
)

for i in range(1, 6, 1):

# Models to run:
# - [X] bert-base-uncased
# - [X] nlpaueb/legal-bert-base-uncased
# - [X] roberta-base
# - [X] pile-of-law/legalbert-large-1.7M-1
    
# Load in Dataset, clean it
    data_files = {
    "train": "./data/training_data/01_03_24_control_sentence_document.csv",
    "full_eval": "./data/training_data/01_03_24_eval_sentence_document.csv",
    }
    dataset = load_dataset("csv", data_files=data_files)

    tokenizer = AutoTokenizer.from_pretrained("nlpaueb/legal-bert-base-uncased")

    tokenized_data = dataset.map(tokenize_function, batched=True)

    train_dataset = tokenized_data["train"]
    eval_dataset = tokenized_data["full_eval"]

    id2label = {0: "not activism", 1: "activism"}
    label2id = {"not activism": 0, "activism": 1}

    model = AutoModelForSequenceClassification.from_pretrained(
        "nlpaueb/legal-bert-base-uncased",
        num_labels=2,
        id2label=id2label,
        label2id=label2id,
    )


    training_args = TrainingArguments(
        output_dir="test_trainer",
        evaluation_strategy="epoch",
        save_strategy="epoch",
        num_train_epochs=i,
        seed=12345,
    )

    trainer = Trainer(
        model=model,
        args=training_args,
        train_dataset=train_dataset,
        eval_dataset=eval_dataset,
        compute_metrics=compute_metrics,
    )

    set_seed(12345)

    trainer.train()
    trainer.save_model(f"./output/models/01_12_24_sentence_control_lbert_{i}_document")

# Create Confusion Matrix
    tokenizer = AutoTokenizer.from_pretrained("nlpaueb/legal-bert-base-uncased")

    pred_model = pipeline(
        "text-classification",
        model=f"./output/models/01_12_24_sentence_control_lbert_{i}_document",
        tokenizer=tokenizer,
        max_length=512,
        truncation=True
    )

    y_preds = []
    y_trues = []
    y_scores = []
    for index, text in enumerate(dataset["full_eval"]):
        pred = pred_model(text["text"])[0]
        y_trues.append(dataset["full_eval"][index]["label"])
        y_preds.append(label2id[pred["label"]])
        y_scores.append(pred["score"])

    conf_df = pd.DataFrame({"true": y_trues, "pred": y_preds, "score": y_scores})

    conf_df.to_csv(
        f"./output/validate/validate_01_12_24_sentence_control_lbert_{i}_document.csv"
    )
