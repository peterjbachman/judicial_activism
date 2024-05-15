library(data.table)
library(tidyverse)
library(haven)
library(sampleSelection)
library(fastDummies)
library(labelled)
library(magrittr)

df <- fread("./data/SCDB_2023_01_justiceCentered_Citation.csv")
mq <- fread("./data/final_data_merging/justices.csv")
mq[, term := term + 1]

# create measure of cooperation
all_combos <- df %>%
  group_by(term) %>%
  expand(justice = justice, agree = justice) %>%
  as.data.table()

fst_agr <- df[!is.na(firstAgreement) & firstAgreement != 0, ] %>%
  group_by(term, justice) %>%
  count(firstAgreement) %>%
  rename(agree = firstAgreement)

scnd_agr <- df[!is.na(secondAgreement) & secondAgreement != 0, ] %>%
  group_by(term, justice) %>%
  count(secondAgreement) %>%
  rename(agree = secondAgreement)

agree <- bind_rows(fst_agr, scnd_agr) %>%
  group_by(term, justice, agree) %>%
  summarise(count = sum(n)) %>%
  as.data.table()

sep_opinions_count <- df %>%
  group_by(term, justice) %>%
  summarise(total_sep_opinions = sum(as.numeric(vote != 1 & opinion %in% c(2, 3)))) %>%
  as.data.table()

all_combos <- merge(all_combos, agree, by = c("term", "justice", "agree"), all.x = TRUE)
all_combos <- merge(all_combos, sep_opinions_count, by = c("term", "justice"), all.x = TRUE)
all_combos[is.na(count), count := ifelse(justice == agree, total_sep_opinions, 0)]

agree <- all_combos[!(justice == agree)]

agree <- merge(agree, mq[, .(term, justice, post_mn)], by = c("term", "justice"), all.x = TRUE)
setnames(agree, "post_mn", "justice_mq")
agree <- merge(agree, mq[, .(term, justice, post_mn)], by.x = c("term", "agree"), by.y = c("term", "justice"), all.x = TRUE)
setnames(agree, "post_mn", "agree_mq")

agree <- agree[!is.na(agree_mq) & !is.na(justice_mq)]

agree[, dist := abs(justice_mq - agree_mq)]
setorder(agree, term, justice)

agree[, percentage_join := count / total_sep_opinions]
agree[, percentage_join := ifelse(is.infinite(percentage_join), 0, percentage_join)]
agree[is.na(percentage_join), percentage_join := 0]

coop <- lm(percentage_join ~ dist, data = agree)

agree$cooperation <- coop$residuals

coop_df <- agree[, .(term, justice, agree, cooperation)]

df <- merge(df, coop_df, by.x = c("term", "justice", "majOpinWriter"), by.y = c("term", "justice", "agree"), all.x = TRUE)
setorder(df, caseId)

df[is.na(cooperation) & (majOpinWriter != justice), cooperation := 0]

# Read in Opinion Data
opinion_activism <- fread("./output/data/final_data_opinion_activism_document_01_13_24_fixed.csv")
opinion_activism[, bin_activist := as.numeric(bin_activist)]
opinion_activism[, bin_activist_thresh := as.numeric(bin_activist_thresh)]
opinion_activism[, bin_activist_thresh_pg := as.numeric(bin_activist_thresh_pg)]

df_seps <- opinion_activism[type != "opinion"]
df_seps$justice <- as.numeric(str_extract(df_seps$author, "\\d+"))
df_seps$caseId <- df_seps$scdb_id
df_seps$firstAgreement <- df_seps$justice
df_seps$secondAgreement <- df_seps$justice
setorder(df_seps, scdb_id)

df[, maj_op_yes := ifelse(majOpinWriter == justice, 1, 0)]
df[, author_merge := paste(justice, str_to_lower(justiceName), sep = "-")]

df <- merge(df, df_seps[, -c("type", "lxs_cite", "justice", "caseId", "firstAgreement", "secondAgreement")], by.x = c("caseId", "author_merge"), by.y = c("scdb_id", "author"), all.x = TRUE)
df[
  df_seps[, -c("type", "lxs_cite", "justice")],
  on = .(caseId, firstAgreement),
  c(
    "prob_activist",
    "prob_activist_w",
    "prob_activist_thresh",
    "prob_activist_w_thresh",
    "prob_activist_thresh_pg",
    "prob_activist_w_thresh_pg",
    "bin_activist",
    "bin_activist_thresh",
    "bin_activist_thresh_pg"
  ) := .(
    i.prob_activist,
    i.prob_activist_w,
    i.prob_activist_thresh,
    i.prob_activist_w_thresh,
    i.prob_activist_thresh_pg,
    i.prob_activist_w_thresh_pg,
    i.bin_activist,
    i.bin_activist_thresh,
    i.bin_activist_thresh_pg
  )
]

df[
  df_seps[, -c("type", "lxs_cite", "justice")],
  on = .(caseId, secondAgreement),
  c(
    "prob_activist",
    "prob_activist_w",
    "prob_activist_thresh",
    "prob_activist_w_thresh",
    "prob_activist_thresh_pg",
    "prob_activist_w_thresh_pg",
    "bin_activist",
    "bin_activist_thresh",
    "bin_activist_thresh_pg"
  ) := .(
    i.prob_activist,
    i.prob_activist_w,
    i.prob_activist_thresh,
    i.prob_activist_w_thresh,
    i.prob_activist_thresh_pg,
    i.prob_activist_w_thresh_pg,
    i.bin_activist,
    i.bin_activist_thresh,
    i.bin_activist_thresh_pg
  )
]

setorder(df, lexisCite, justiceName)

# Separate opinions only go up until 2013
df <- df[term <= 2013]
# df[is.na(prob_activist), prob_activist := 0]
# df[is.na(prob_activist_w), prob_activist_w := 0]
# df[is.na(prob_activist_thresh), prob_activist_thresh := 0]
# df[is.na(prob_activist_w_thresh), prob_activist_w_thresh := 0]

# MQ Scores
mq <- fread("./data/final_data_merging/justices.csv")
df <- merge(df, mq, by = c("term", "justice", "justiceName"), all.x = TRUE)

# Complexity scores
complex <- as.data.table(read_dta("./data/final_data_merging/complexity.dta"))
df <- merge(df, complex[, .(caseId, latentComplexity)], by = c("caseId"), all.x = TRUE)
setorder(df, lexisCite, justiceName)

# distance to Majority opinion writer
df <- df %>%
  group_by(caseId) %>%
  mutate(maj_writer_mq = post_mn[maj_op_yes == 1]) %>%
  mutate(
    dist_to_maj_writer = abs(maj_writer_mq - post_mn),
    dist_sq = dist_to_maj_writer**2
  ) %>%
  as.data.table()

# Chief Justice
df[, is_chief := ifelse(
  (author_merge == "99-weburger" & chief == "Burger") |
    (author_merge == "90-ewarren" & chief == "Warren") |
    (author_merge == "102-whrehnquist" & chief == "Rehnquist") |
    (author_merge == "111-jgroberts" & chief == "Roberts") |
    (author_merge == "87-fmvinson" & chief == "Vinson"),
  1, 0
)]

# More extreme than the majority opinion writer
df[, is_more_extreme := ifelse(
  (maj_writer_mq > 0 & post_mn > maj_writer_mq) |
    (maj_writer_mq < 0 & post_mn < maj_writer_mq),
  1, 0
)]

# Wrote a separate opinion
df[, wrote_sep_opinion := ifelse(vote != 1 & opinion %in% c(2, 3), 1, 0)]
df[wrote_sep_opinion == 1]

# Median in the majority
df[!(majVotes <= minVotes), maj_opinion_median_mq := median(post_mn[vote %in% c(1, 3, 4, 5)], na.rm = TRUE), by = caseId]

df[, dist_median := abs(maj_opinion_median_mq - post_mn)]
df[, dist_median_sq := dist_median**2]

# Political Salience Measure
load("./data/CLRSalienceEstimates100714.RData")
salience <- as.data.table(salienceEstimates)

df <- merge(df, salience[, .(salienceData.caseId, early.salience.est)], by.x = "caseId", by.y = "salienceData.caseId", all.x = TRUE)

# Wrote separate opinion dummy variable
df[, wrote_concur := ifelse(vote %in% c(3) & opinion %in% c(2, 3), 1, 0)]
df[, wrote_concur_special := ifelse(vote %in% c(4) & opinion %in% c(2, 3), 1, 0)]
df[, wrote_dissent := ifelse(vote %in% c(2) & opinion %in% c(2, 3), 1, 0)]

# wrote or joined
df[, wrote_joined_concur := ifelse((vote == 3 & opinion %in% c(2, 3)) | (vote == 3 & !(is.na(firstAgreement))), 1, 0)]
df[, wrote_joined_concur_special := ifelse((vote == 4 & opinion %in% c(2, 3))| (vote == 4 & !(is.na(firstAgreement))), 1, 0)]
df[, wrote_joined_dissent := ifelse((vote == 2 & opinion %in% c(2, 3)) | (vote == 2 & !(is.na(firstAgreement))), 1, 0)]

# Minimum Coalition dummy variable
df[, min_coalition := ifelse(majVotes == ceiling((majVotes + minVotes) / 2), 1, 0)]

# freshman justices
freshman_terms <- df %>%
  group_by(term) %>%
  reframe(justices = unique(justiceName)) %>%
  ungroup() %>%
  arrange(justices, term) %>%
  group_by(justices) %>%
  slice_head(n = 2) %>%
  filter(!(justices %in% c(
    "FFrankfurter",
    "FMurphy",
    "HLBlack",
    "RHJackson",
    "SFReed",
    "WBRutledge",
    "WODouglas"
  ))) %>%
  ungroup() %>%
  as.data.table()

# drop FFrankfurter, FMurphy, HLBlack, RHJackson, SFReed, WBRutledge, WODouglas
freshman_terms[, freshman_term := 1]

df <- merge(
  df,
  freshman_terms,
  by.x = c("term", "justiceName"),
  by.y = c("term", "justices"),
  all.x = TRUE
)
df[is.na(freshman_term), freshman_term := 0]
setorder(df, lexisCite, justiceName)

merge(test, test_activist, by.x = c("caseId", "firstAgreement"), by.y = c("caseId", "justice"))

# Number of Amicus Briefs
amicus <- read_dta("data/replication data.dta") %>%
  as.data.table()

amicus <- amicus[, .(uslexis, amiciz)]
amicus <- unique(amicus)

df <- merge(df, amicus, by.x = "lexisCite", by.y = "uslexis", all.x = TRUE)

# personal relevance scores
oa_words <- read_dta("./data/BSJ_Salience-20130506/SCDB_Small_Justice.dta") %>%
  as.data.table()

df <- merge(df, oa_words[, .(caseId, justice, wordLog_zscore)], by = c("caseId", "justice"), all.x = TRUE)

# Workload
df %<>%
  arrange(term, dateArgument) %>%
  group_by(term, justice) %>%
  mutate(workload = cumsum(ifelse(!is.na(maj_op_yes), maj_op_yes, 0))) %>%
  ungroup() %>%
  as.data.table()

df
fwrite(df, "./output/data/early_final_justice_lv_01_14_24.csv")

df <- fread("./output/data/early_final_justice_lv_01_14_24.csv")

complete <- df[!is.na(latentComplexity) & !is.na(early.salience.est) & !is.na(dist_to_maj_writer)]
complete$IMR1 <- 0
complete$justiceName <- factor(complete$justiceName, labels = c(
  "A. Fortas",
  "A. Goldberg",
  "A. Kennedy",
  "A. Scalia",
  "B. White",
  "C. Whittaker",
  "C. Thomas",
  "D. Souter",
  "E. Warren",
  "F. Frankfurter",
  "H. Blackmun",
  "H. Burton",
  "H. Black",
  "J. Roberts",
  "J. Harlan",
  "J. Stevens",
  "L. Powell",
  "P. Stewart",
  "R. Ginsburg",
  "S. Alito",
  "S. O'Connor",
  "S. Reed",
  "S. Breyer",
  "S. Minton",
  "T. Clark",
  "T. Marshall",
  "W. Burger",
  "W. Rehnquist",
  "W. Brennan",
  "W. Douglas"
))
complete$issueArea <- factor(complete$issueArea, labels = c(
  "Criminal Procedure",
  "Civil Rights",
  "First Amendment",
  "Due Process",
  "Privacy",
  "Attorneys",
  "Unions",
  "Economic Activity",
  "Judicial Power",
  "Federalism",
  "Interstate Relations",
  "Federal Taxation",
  "Miscellaneous"
))

var_label(complete$wrote_concur) <- "Wrote Normal Concurrence"
var_label(complete$wrote_concur_special) <- "Wrote Special Concurrence"
var_label(complete$wrote_dissent) <- "Wrote Dissent"
var_label(complete$dist_to_maj_writer) <- "Ideologcal Distance"
var_label(complete$is_chief) <- "Chief Justice"
var_label(complete$early.salience.est) <- "Political Salience"
var_label(complete$latentComplexity) <- "Legal Complexity"
var_label(complete$is_more_extreme) <- "More Extreme than Opinion Writer"
var_label(complete$freshman_term) <- "Freshman Justice"
var_label(complete$min_coalition) <- "Minimum Coalition"
var_label(complete$IMR1) <- "Inverse Mill's Ratio"
var_label(complete$issueArea) <- "Issue Area"

complete <- complete[maj_op_yes != 1 & !is.na(vote)]
complete <- complete[decisionType == 1]
# Only Wrote + Median member of the Majority -----------------------------------

concur_probit <- probit(
  wrote_concur ~ dist_median + is_chief + latentComplexity +
    early.salience.est + freshman_term + min_coalition + is_more_extreme +
    cooperation + workload,
  data = complete
)
imr_Data <- invMillsRatio(concur_probit)
complete$IMR1 <- imr_Data$IMR1

concur_out <- lm(
  prob_activist_thresh ~ dist_median + dist_median_sq + early.salience.est +
    wordLog_zscore + latentComplexity + issueArea + IMR1,
  data = complete, subset = (wrote_concur == 1)
)

summary(concur_out)

concur_special_probit <- probit(
  wrote_concur_special ~ dist_median + is_chief + latentComplexity +
    early.salience.est + is_more_extreme + freshman_term + min_coalition +
    cooperation + workload,
  data = complete
)
imr_Data <- invMillsRatio(concur_special_probit)
complete$IMR1 <- imr_Data$IMR1

concur_special_out <- lm(
  prob_activist_thresh ~ dist_median + dist_median_sq + early.salience.est +
    wordLog_zscore + latentComplexity + issueArea + IMR1,
  data = complete, subset = (wrote_concur_special == 1)
)
summary(concur_special_out)

diss_probit <- probit(
  wrote_joined_dissent ~ dist_median + dist_median_sq + is_chief +
    min_coalition + latentComplexity + early.salience.est + is_more_extreme +
    freshman_term,
  data = complete
)
imrData <- invMillsRatio(diss_probit)
complete$IMR1 <- imrData$IMR1

diss_out <- lm(
  prob_activist_w_thresh ~ dist_median + dist_median_sq + early.salience.est +
    wordLog_zscore + latentComplexity + decisionDirectionDissent + issueArea +
    IMR1,
  data = complete, subset = (wrote_joined_dissent == 1)
)

summary(diss_out)

# Weighted Probability Activist ------------------------------------------------

concur_probit <- probit(
  wrote_concur ~ majVotes + dist_to_maj_writer + is_chief +
    latentComplexity + early.salience.est + is_more_extreme + freshman_term +
    min_coalition + workload,
  data = complete
)
imr_Data <- invMillsRatio(concur_probit)
complete$IMR1 <- imr_Data$IMR1

concur_out_w <- lm(
  prob_activist_w ~ dist_to_maj_writer + early.salience.est +
    legal_salience + issueArea + justiceName + IMR1,
  data = complete, subset = (wrote_concur == 1)
)
summary(concur_out_w)

concur_special_probit <- probit(
  wrote_concur_special ~ majVotes + dist_to_maj_writer + is_chief +
    latentComplexity + early.salience.est + is_more_extreme + freshman_term +
    legal_salience + min_coalition,
  data = complete
)
imr_Data <- invMillsRatio(concur_special_probit)
complete$IMR1 <- imr_Data$IMR1

concur_special_out_w <- lm(
  prob_activist_w ~ dist_to_maj_writer + early.salience.est +
    legal_salience + issueArea + justiceName + IMR1,
  data = complete, subset = (wrote_concur_special == 1)
)
summary(concur_special_out_w)

diss_probit <- probit(
  wrote_dissent ~ majVotes + dist_to_maj_writer + is_chief +
    latentComplexity + early.salience.est + is_more_extreme + freshman_term +
    legal_salience,
  data = complete
)
imrData <- invMillsRatio(diss_probit)
complete$IMR1 <- imrData$IMR1

diss_out_w <- lm(
  prob_activist_thresh_pg ~ dist_to_maj_writer + early.salience.est +
    decisionDirectionDissent + legal_salience + issueArea + justiceName + IMR1,
  data = complete, subset = (wrote_dissent == 1)
)
summary(diss_out_w)

probits <- list(concur_probit, concur_special_probit, diss_probit)
saveRDS(probits, file = "./output/regressions/probits_robust.RData")

results <- list(diss_out, diss_out_w, concur_out, concur_out_w, concur_special_out, concur_special_out_w)
saveRDS(results, file = "./output/regressions/2step_robust.RData")

# Probability Activist - Robust ------------------------------------------------

concur_probit <- probit(
  wrote_joined_concur ~ dist_to_maj_writer + is_chief +
    latentComplexity + early.salience.est + is_more_extreme + freshman_term +
    legal_salience + min_coalition,
  data = complete
)
imr_Data <- invMillsRatio(concur_probit)
complete$IMR1 <- imr_Data$IMR1

concur_out <- lm(
  prob_activist_thresh_pg ~ dist_to_maj_writer + early.salience.est +
    legal_salience + issueArea + justiceName + IMR1,
  data = complete, subset = (wrote_joined_concur == 1)
)
summary(concur_out)

concur_special_probit <- probit(
  wrote_joined_concur_special ~ dist_to_maj_writer + is_chief +
    latentComplexity + early.salience.est + is_more_extreme + freshman_term +
    legal_salience + min_coalition,
  data = complete
)
imr_Data <- invMillsRatio(concur_special_probit)
complete$IMR1 <- imr_Data$IMR1

concur_special_out <- lm(
  prob_activist_thresh_pg ~ dist_to_maj_writer + early.salience.est +
    legal_salience + issueArea + justiceName + IMR1,
  data = complete, subset = (wrote_joined_concur_special == 1)
)
summary(concur_special_out)

diss_probit <- probit(
  wrote_joined_dissent ~ majVotes + dist_to_maj_writer + is_chief +
    latentComplexity + early.salience.est + is_more_extreme + freshman_term +
    legal_salience,
  data = complete
)
imrData <- invMillsRatio(diss_probit)
complete$IMR1 <- imrData$IMR1

diss_out <- lm(
  prob_activist_thresh_pg ~ dist_to_maj_writer + dist_sq + early.salience.est +
    decisionDirectionDissent + legal_salience + issueArea +
    justiceName + IMR1,
  data = complete, subset = (wrote_joined_dissent == 1)
)

diss_out <- glm(
  bin_activist_thresh_pg ~ dist_to_maj_writer + dist_sq + early.salience.est +
    decisionDirectionDissent + legal_salience + issueArea +
    justiceName + IMR1,
  data = complete, subset = (wrote_joined_dissent == 1),
  family = "binomial"
)

summary(diss_out)

# Weighted Probability Activist - Robust ---------------------------------------

concur_probit <- probit(
  wrote_joined_concur ~ majVotes + dist_to_maj_writer + is_chief +
    latentComplexity + early.salience.est + is_more_extreme + freshman_term +
    legal_salience + min_coalition,
  data = complete
)
imr_Data <- invMillsRatio(concur_probit)
complete$IMR1 <- imr_Data$IMR1

concur_out_w <- lm(
  prob_activist_w ~ dist_to_maj_writer + early.salience.est +
    legal_salience + issueArea + justiceName + IMR1,
  data = complete, subset = (wrote_joined_concur == 1)
)
summary(concur_out_w)

concur_special_probit <- probit(
  wrote_joined_concur_special ~ majVotes + dist_to_maj_writer + is_chief +
    latentComplexity + early.salience.est + is_more_extreme + freshman_term +
    legal_salience + min_coalition,
  data = complete
)
imr_Data <- invMillsRatio(concur_special_probit)
complete$IMR1 <- imr_Data$IMR1

concur_special_out_w <- lm(
  prob_activist_w ~ dist_to_maj_writer + early.salience.est +
    legal_salience + issueArea + justiceName + IMR1,
  data = complete, subset = (wrote_joined_concur_special == 1)
)
summary(concur_special_out_w)

diss_probit <- probit(
  wrote_joined_dissent ~ majVotes + dist_to_maj_writer + is_chief +
    latentComplexity + early.salience.est + is_more_extreme + freshman_term +
    legal_salience,
  data = complete
)
imrData <- invMillsRatio(diss_probit)
complete$IMR1 <- imrData$IMR1

diss_out_w <- lm(
  prob_activist_w ~ dist_to_maj_writer + early.salience.est +
    decisionDirectionDissent + legal_salience + issueArea + justiceName + IMR1,
  data = complete, subset = (wrote_joined_dissent == 1)
)

probits <- list(concur_probit, concur_special_probit, diss_probit)
saveRDS(probits, file = "./output/regressions/probits_robust.RData")

results <- list(diss_out, diss_out_w, concur_out, concur_out_w, concur_special_out, concur_special_out_w)
saveRDS(results, file = "./output/regressions/2step_robust.RData")
