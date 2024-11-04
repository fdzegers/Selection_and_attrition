# The cohort is made up of
# 1. All included after data cleaning at T0, and 
# 2. All who completed T0b
# 
# For externals: T0 is the baseline questionnaire
#                 T0b is the first follow-up questionnaire
#
# Here, one big data file of all participants is made
#
# When this is done we will go on with prettified and making a table in different R-scripts

# Initial packages
library(readr)
library(dplyr)
library(ggplot2)

# Directory of data
getwd()
setwd("//vf-DataSafe/DataSafe$/Div0/ict/HebikReuma_deidentified_2216/Floor/Attrition_selection_article_no_1/Data")

# Load data
dat_T0 = read.csv("dat_newT0.csv", sep = ",")
dat_T0b = read.csv("dat_newT0b_eligible.csv", sep = ",")
dat_T0b_compl = read.csv("dat_newT0b_compl.csv", sep = ",")

dat_T0b_compl <- dat_T0b_compl$Castor.Record.ID

# Make sure all variables from T0 and T0b- datasets are merged together
drop_cols_T0b <- c("index", "exchangeInfoWithHP", "keepDataForFutureResearch", "contactForFollowUp", "exchangeInfoWithHP", "keepDataForFutureResearch", "contactForFollowUp")
drop_cols_T0 <- c("index","exchangeInfoWithHP.y", "keepDataForFutureResearch.y", "contactForFollowUp.y", "exchangeInfoWithHP.y", "keepDataForFutureResearch.y", "contactForFollowUp.y", "exchangeInfoWithHP.x", "keepDataForFutureResearch.x", "contactForFollowUp.x", "exchangeInfoWithHP.x", "keepDataForFutureResearch.x", "contactForFollowUp.x")

dat_T0b <- dat_T0b[, !(names(dat_T0b) %in% drop_cols_T0b)] 
dat_T0 <- dat_T0[, !(names(dat_T0) %in% drop_cols_T0)] 

# Put all answers from T0b on T0 (the mother)-dataset
dat_T0 <- merge(x=dat_T0,y=dat_T0b, all.x = T, by = "Castor.Record.ID")

# Make one big fat dataset with the two groups of interest indicated
dat_T0b_ans <- dat_T0 %>%
  filter(Castor.Record.ID %in% dat_T0b_compl)


dat_T0b_ans$dataset <- "Responders"

## Who are eligible for T0b
# We will not need this dataset, I just want the numbers for the flowchart
dat_T0b_el <- dat_T0b$Castor.Record.ID


dat_T0$dataset <- "Non-responders"

# Put the three datasets below each other rbind!
dat_all <- rbind(dat_T0, dat_T0b_ans)

save(dat_all, file = "dat_all.Rdata")
