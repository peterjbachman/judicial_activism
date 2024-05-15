import xgboost as xgb
import pandas as pd
from sklearn.feature_extraction.text import TfidfVectorizer
import numpy as np

def _F1_eval(preds, labels):
    t = np.arange(0, 1, 0.005)
    f = np.repeat(0, 200)
    results = np.vstack([t, f]).T
    # assuming labels only containing 0's and 1's
    n_pos_examples = sum(labels)
    if n_pos_examples == 0:
        raise ValueError("labels not containing positive examples")

    for i in range(200):
        pred_indexes = (preds >= results[i, 0])
        TP = sum(labels[pred_indexes])
        FP = len(labels[pred_indexes]) - TP
        precision = 0
        recall = TP / n_pos_examples

        if (FP + TP) > 0:
            precision = TP / (FP + TP)

        if (precision + recall > 0):
            F1 = 2 * precision * recall / (precision + recall)
        else:
            F1 = 0
        results[i, 1] = F1
    return (max(results[:, 1]))

def F1_eval(preds, dtrain):
    res = _F1_eval(preds, dtrain.get_label())
    return 'f1_err', 1-res


# load in and convert data
train = pd.read_csv("data/training_data/01_03_24_control_sentence_document.csv")
test = pd.read_csv("data/training_data/01_03_24_eval_sentence_document.csv")

text = pd.concat([train.text, test.text])

vectorizer = TfidfVectorizer(stop_words="english")
x_all = vectorizer.fit_transform(text)

x_all.get_shape()

x_train = x_all[0:687]
y_train = train.label

x_test = x_all[687:]
y_test = test.label

dtrain = xgb.DMatrix(x_train, label=y_train)
dtest = xgb.DMatrix(x_test, label=y_test)

# Train
param = {'max_depth': 4, 'eta': 1, 'objective': 'binary:logistic', 'disable_default_eval_metric': 1}
param['nthread'] = 4
evallist = [(dtrain, 'train'), (dtest, 'eval')]

bst = xgb.train(param, dtrain, num_boost_round=500, evals=evallist, feval=F1_eval, early_stopping_rounds=30)

pred = bst.predict(dtest)
pred

conf_df = pd.DataFrame({"true": y_test, "pred": pred})

conf_df.to_csv("./output/validate/validate_01_14_24_control_xgboost_sentence.csv")
