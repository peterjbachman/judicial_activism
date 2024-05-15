'''
Fine-Tune Legal BERT model
'''
from transformers import AutoTokenizer
from transformers import TFAutoModelForSequenceClassification
from transformers import Trainer
from transformers import pipeline
from transformers import DataCollatorWithPadding
from transformers import create_optimizer
from transformers.keras_callbacks import KerasMetricCallback
from datasets import load_dataset
import tensorflow as tf
import numpy as np
import evaluate
import pandas as pd

# Training objects
metric = evaluate.load("accuracy")

# Load in Dataset, clean it
data_files = {"train": "./data/training_data/12_11_23_paragraph.csv", "test": "./data/training_data/12_11_23_dobbs_paragraph.csv"}
dataset = load_dataset("csv", data_files=data_files)

tokenizer = AutoTokenizer.from_pretrained("nlpaueb/legal-bert-base-uncased")
data_collator = DataCollatorWithPadding(tokenizer=tokenizer, return_tensors="tf")

# Functions

def tokenize_function(examples):
    return tokenizer(examples["text"], padding="max_length", truncation=True)

def compute_metrics(eval_pred):
    logits, labels = eval_pred
    predictions = np.argmax(logits, axis=-1)
    return metric.compute(predictions=predictions, references=labels)

tokenized_data = dataset.map(tokenize_function, batched=True)

train_dataset = tokenized_data["train"]
eval_dataset = tokenized_data["test"]

id2label = {0: "not activism", 1: "activism"}
label2id = {"not activism": 0, "activism": 1}

model = TFAutoModelForSequenceClassification.from_pretrained(
    "nlpaueb/legal-bert-base-uncased", 
    num_labels=2,
    id2label=id2label,
    label2id=label2id
)

batch_size = 4
num_epochs = 5
batches_per_epoch = len(tokenized_data["train"]) // batch_size
total_train_steps = int(batches_per_epoch * num_epochs)
optimizer, schedule = create_optimizer(init_lr=2e-5, num_warmup_steps=0, num_train_steps=total_train_steps)

tf_train_set = model.prepare_tf_dataset(
    tokenized_data["train"],
    shuffle=True,
    batch_size=4,
    collate_fn=data_collator,
)

tf_validation_set = model.prepare_tf_dataset(
    tokenized_data["test"],
    shuffle=False,
    batch_size=4,
    collate_fn=data_collator,
)

model.compile(optimizer=optimizer)
metric_callback = KerasMetricCallback(metric_fn=compute_metrics, eval_dataset=tf_validation_set)
callbacks = [metric_callback]

model.fit(x=tf_train_set, validation_data=tf_validation_set, epochs=3, callbacks=callbacks)

model.save_pretrained("./output/models/12_11_23_paragraph")

# Create Confusion Matrix

model = AutoModelForSequenceClassification.from_pretrained(
    "./output/models/12_11_23_paragraph",
    num_labels=2,
    id2label=id2label,
    label2id=label2id
)

tokenizer = AutoTokenizer.from_pretrained("nlpaueb/legal-bert-base-uncased")

y_preds = []
y_trues = []
for index,val_text in enumerate(dataset["test"]):
    tokenized_val_text = tokenizer(val_text["text"], padding="max_length", truncation=True)
    outputs = model(tokenized_val_text["input_ids"])
    y_pred = outputs.logits.argmax(-1)
    y_true = dataset["test"][index]["label"]
    y_preds.append(y_pred)
    y_trues.append(y_true)

pred_model = pipeline("text-classification", model="./output/models/12_11_23_paragraph", tokenizer=tokenizer)

y_preds = []
y_trues = []
for index, text in enumerate(dataset["test"]):
    pred = pred_model(text["text"])[0]["label"]
    y_trues.append(dataset["test"][index]["label"])
    y_preds.append(label2id[pred])

conf_df = pd.DataFrame(
    {
        'dobbs_true': y_trues,
        'dobbs_pred': y_preds
    }
)

conf_df.to_csv("./output/validate/dobbs_validate_12_11_23.csv")