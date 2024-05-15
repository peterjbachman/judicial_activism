# judicial_activism
Decision in Question: A Study of Supreme Court Justices’ Accusations of Judicial Activism in Separate Opinions

## NOTE

I am still working on getting the data in a clean format. I need to

## Necessary Packages

For R:

```
broom
caret
data.table
fastDummies
ggeffects
ggplot2
haven
jsonlite
labelled
magrittr
modelsummary
psych
purrr
quanteda
readr
reticulate
rsvd
sampleSelection
scico
stringr
text2vec
tidyverse
tokenizers
```

Python Packages (using `3.11.x`)

```
bs4
datasets
evaluate
numpy
pandas
sklearn
tensorflow
transformers
xgboost
```

## Scripts

- `01_pull_opinions.ipynb`: Takes the XML files of all Supreme Court opinions, and saves it under `data/opinions_text.csv`.
    - **Note**: The raw XML files are not provided here because they come from Lexis Nexis.
- `02_pull_dobbs.R`: Takes a PDF of the "Dobbs v. Jackson" Supreme Court opinion and turns it into `data/dobbs_validate.csv`.
    - **Note**: This was originally used as an outside way to validate the LLM measuring *judicial activism*, but was later scrapped as the validation test set increased in size.
- `03_sentence_labels.R`: This takes the set of hand-coded labels and converts them to a number of files under `data/training_data/` where they are split in various ways for fine-tuning.
    - **Note**: Splitting is done two different ways.
        Labels are randomly split into "test" and "train" datasets both at a document-level and randomly with no respect to document.
        For both document-level and full randomization, undersampling of the non-*judicial activism* labels is done by randomly selecting a number of non-*judicial activism* labels such that they only account for 65% of the labels in the dataset (as opposed to 97%).
- `04_paragraph_labels.R`: Does the same thing as `03_sentence_labels.R` but uses labels at the paragraph-level instead of the sentence-level.
- `05_fine_tune_sentence.py`: Performs the fine-tuning necessary for text classification at the sentence level, and creates a .csv file showing the true and predicted values for each text label.
   The script performs fine-tuning for the specified model over a range of epochs.
   Because of this, the scripts take a considerable amount of time to run, but I was able to run these scripts on a MacBook Pro M2 over a few hours.
   The script also saves the weights of the fine-tuned version of the Large Language Model, and as such make sure you have ~20 GB available if you wish to run all models.
- `06_fine_tune_paragraph.py`: The same script as `05_fine_tune_sentence.py`, except for performing fine-tuning at the paragraph-level.
- `07_compare_models.R`: Creates comparisons of text classification accuracy across different Large Language Models and different fine-tuning specifications. The output files are placed in `output/appendix/`
- `08_xgboost.py`: Performs Gradient Boosting through the `xgboost` package to create a text classification comparison point using a Bag-of-Words approach.
- `09_prob_scores.R`: Generates the probability that each sentence/paragraph is classified as *judicial activism*. Because the classification of *judicial activism* is a binary indicator, I can pull the probability that each text label is classified as non-*judicial activism* (0), or *judicial activism* (1). This allows me to adjust the threshold it takes for a text label to be classified as *judicial activism*
- `10_merge_regress.R`: Merges *judicial activism* scores with the rest of the measures I use (see the "Data" section in the PDF in this repository). The file then models the data using the two-stage selection model. Results of the intermediary probits and final regressions are saved under `output/regressions/probits_robust.RData` and `output/regressions/2step_robust.RData` respectively.

## Data

Data is still under works. The reason being that the labeling data contains identifying information about the people who helped me label data for this project. Before I upload that data I need to remove identifiers from that data.

## Questions?

If you have questions about this project please feel free to reach out. My contact information can be found on my [GitHub Profile](https://github.com/peterjbachman) or on my [personal website](https://peterjbachman.lol/)