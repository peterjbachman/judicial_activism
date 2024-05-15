library(scotustext)
library(stringr)

decisions_sample <- decision_processor(dir_path = "./data/opinion_pdf")

df <- decisions_sample[4:5, c(4, 6, 7)]
df
write.csv(df, "./data/dobbs_validate.csv")
