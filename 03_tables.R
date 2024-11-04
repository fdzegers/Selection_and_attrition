# Here, the descriptive tables are made
# For this, the package gtsummary is used

# Also, the forest plot/box-and-whiskers-plot is made here
# This is done in the same R-script for convenience, as I use the table of characteristics that is made

library(tidyverse)
library(dplyr)
library(gt)
library(gtsummary)
library(gapminder)

setwd("//vf-DataSafe/DataSafe$/Div0/ict/HebikReuma_deidentified_2216/Floor/Attrition_selection_article_no_1/Data")
load("dat_all_03.RData") 

min(dat_all$Participant.Creation.Date_adj, na.rm = TRUE)
max(dat_all$Participant.Creation.Date_adj, na.rm = TRUE)


# Remake the T0 and T0b datasets from the r-binded dataset
dat_all_T0 <- subset(dat_all, dat_all$dataset=="Non-responders")
dat_all_T0b <- subset(dat_all, dat_all$dataset=="Responders")


# Make the dataset that indicates if someone answered only T0 or also T0b
# Delete the entries that were appended (so duplicates of each individual from T0b)
dat_all_sub <- dat_all %>%
  arrange(desc(dataset)) %>%
  filter(!duplicated(Castor.Record.ID) )

#################### 
# T0 and T0b Eerdere diagnose
##################

dat_all_sub <- dat_all_sub %>%
  mutate(Diag_T0_T0b = if_else(Diagnosis_T0=="Yes" | Diagnosis_T0b=="Yes", "Yes",
                               if_else(Diagnosis_T0=="Don't know" | Diagnosis_T0b=="Don't know", "Don't know", "No")))

dat_all_T0b <- dat_all_T0b %>%
  mutate(Diag_T0_T0b = if_else(Diagnosis_T0=="Yes" | Diagnosis_T0b=="Yes", "Yes",
                               if_else(Diagnosis_T0=="Don't know" | Diagnosis_T0b=="Don't know", "Don't know", "No")))


############
# Table across datasets
############


characteristics <- c("Sex","Agegroup","Smoking", "Alcohol","BMI_4cat", "Family_diag_Yes_No","Swelling","Pain","MorningStiffness","AllDayStiffness","Exhaustion","ReducedEndurance","NoneOfTheAbove","Diag_T0_T0b")
table_dataset_characteristics <- dat_all_sub %>%
  tbl_summary(include=all_of(characteristics),  missing_text = "Missing", by = "dataset", percent = "column") %>% 
  add_overall() %>%
  remove_row_type(variables = "Sex",
                  type="level",
                  level_value = "Male") %>%
  modify_header(all_stat_cols() ~ "**{level}**, N={n} ({style_percent(p)}%)") %>%
  as_gt() %>%
  gt::tab_header(title = "Characteristics of non-responders and responders to the follow-up survey")
table_dataset_characteristics
gtsave(table_dataset_characteristics, filename="../Tables/Table_dataset_characteristics.docx")



#################
# Table across source
#################
# Only T0b answers
characteristics_T0b <- c("Sex","Agegroup","Smoking", "Alcohol","BMI_4cat", "Family_diag_Yes_No","Swelling","Pain","MorningStiffness","AllDayStiffness","Exhaustion","ReducedEndurance","NoneOfTheAbove","Diag_T0_T0b")

dat_all_T0b$Source_group2 <- droplevels(dat_all_T0b$Source_group2)

dat_all_T0b <- dat_all_T0b %>% 
  mutate(Source_group2 = factor(Source_group2, levels = c("Facebook", "Instagram", "Google", "ReumaNL",  "Other online", "GP", "Hospital", "Other")))
    # To reorder the columns in the table

table_source_characteristics <- dat_all_T0b %>%
  tbl_summary(include=all_of(characteristics_T0b),  missing_text = "Missing", by = "Source_group2") %>%
  remove_row_type(variables = "Sex",
                  type="level",
                  level_value = "Male") %>%
  modify_header(all_stat_cols() ~ "**{level}**, N={n} ({style_percent(p)}%)") %>%
  as_gt() %>%
  gt::tab_header(title = "Characteristics according to source of recruitment for participants responding to the follow-up survey")
table_source_characteristics
gtsave(table_source_characteristics, filename="../Tables/Table_source_characteristics.docx")



# Difference in percentage (and plot?)
n_total <- nrow(dat_all_sub)
n_T0b_total <-nrow(subset(dat_all_sub,dat_all_sub$dataset=="Responders"))
n_T0_total <-nrow(subset(dat_all_sub,dat_all_sub$dataset=="Non-responders"))

# Sex
n_female_T0b <- nrow(subset(dat_all_sub,dat_all_sub$Sex=="Female" & dat_all_sub$dataset=="Responders"))
n_female_T0 <- nrow(subset(dat_all_sub,dat_all_sub$Sex=="Female" & dat_all_sub$dataset=="Non-responders"))

# p2-p1, dus follow-up minus niet follow-up
p2_sex <- n_female_T0/n_T0_total # prop of n_female_T0
p1_sex <- n_female_T0b/n_T0b_total # prop of n_female_T0b

se_dif_sex <- sqrt(abs(p1_sex*(1-p1_sex)/n_T0_total+p2_sex*(1-p2_sex)/n_T0b_total ))
100*c(p1_sex,p2_sex, p1_sex-p2_sex, p1_sex-p2_sex-1.96*se_dif_sex, p1_sex-p2_sex+1.96*se_dif_sex)
round(100*c(p1_sex,p2_sex, p1_sex-p2_sex, p1_sex-p2_sex-1.96*se_dif_sex, p1_sex-p2_sex+1.96*se_dif_sex), digits=1) 
diff1_sex <- (p1_sex-p2_sex)
diff_percent_all <- data.frame(sapply("difference_female", function(cell) paste0(round(100*diff1_sex, digits=1), " (", round(100*(diff1_sex-1.96*se_dif_sex), digits = 1), ",", round(100*(diff1_sex+1.96*se_dif_sex), digits = 1), ")"), USE.NAMES = T))
names(diff_percent_all)[1] <- "Difference with CI"
# Initial entry to dataframe

# Age
n_age_under_40_total <- nrow(subset(dat_all_sub, dat_all_sub$Agegroup=="< 40"))
n_age_under_40_T0b <- nrow(subset(dat_all_sub,dat_all_sub$Agegroup=="< 40" & dat_all_sub$dataset=="Responders"))
n_age_under_40_T0 <- nrow(subset(dat_all_sub,dat_all_sub$Agegroup=="< 40" & dat_all_sub$dataset=="Non-responders"))

n_age_under_50_total <- nrow(subset(dat_all_sub, dat_all_sub$Agegroup=="40-50"))
n_age_under_50_T0b <- nrow(subset(dat_all_sub,dat_all_sub$Agegroup=="40-50" & dat_all_sub$dataset=="Responders"))
n_age_under_50_T0 <- nrow(subset(dat_all_sub,dat_all_sub$Agegroup=="40-50" & dat_all_sub$dataset=="Non-responders"))
n_age_50_60_total <- nrow(subset(dat_all_sub, dat_all_sub$Agegroup=="50-60"))
n_age_50_60_T0b <- nrow(subset(dat_all_sub,dat_all_sub$Agegroup=="50-60" & dat_all_sub$dataset=="Responders"))
n_age_50_60_T0 <- nrow(subset(dat_all_sub,dat_all_sub$Agegroup=="50-60" & dat_all_sub$dataset=="Non-responders"))
n_age_over_60_total <- nrow(subset(dat_all_sub, dat_all_sub$Agegroup=="60+"))
n_age_over_60_T0b <- nrow(subset(dat_all_sub,dat_all_sub$Agegroup=="60+" & dat_all_sub$dataset=="Responders"))
n_age_over_60_T0 <- nrow(subset(dat_all_sub,dat_all_sub$Agegroup=="60+" & dat_all_sub$dataset=="Non-responders"))

p2_age_under_40 <- n_age_under_40_T0/n_T0_total 
p1_age_under_40 <- n_age_under_40_T0b/n_T0b_total 
se_dif_under_40 <- sqrt(abs(p1_age_under_40*(1-p1_age_under_40)/n_T0_total+p2_age_under_40*(1-p2_age_under_40)/n_T0b_total ))
diff1_under_40 <- (p1_age_under_40-p2_age_under_40)
diff_under_40 <-  data.frame(sapply("difference_under40", function(cell) paste0(round(100*diff1_under_40, digits=1), " (", round(100*(diff1_under_40-1.96*se_dif_under_40), digits = 1), ",", round(100*(diff1_under_40+1.96*se_dif_under_40), digits = 1), ")"), USE.NAMES = T))
names(diff_under_40)[1] <- "Difference with CI"

p2_age_under_50 <- n_age_under_50_T0/n_T0_total 
p1_age_under_50 <- n_age_under_50_T0b/n_T0b_total 
se_dif_under_50 <- sqrt(abs(p1_age_under_50*(1-p1_age_under_50)/n_T0_total+p2_age_under_50*(1-p2_age_under_50)/n_T0b_total ))
diff1_under_50 <- (p1_age_under_50-p2_age_under_50)
diff_under_50 <-  data.frame(sapply("diff1erence_under50", function(cell) paste0(round(100*diff1_under_50, digits=1), " (", round(100*(diff1_under_50-1.96*se_dif_under_50), digits = 1), ",", round(100*(diff1_under_50+1.96*se_dif_under_50), digits = 1), ")"), USE.NAMES = T))
names(diff_under_50)[1] <- "Difference with CI"

p2_age_50_60 <- n_age_50_60_T0/n_T0_total 
p1_age_50_60 <- n_age_50_60_T0b/n_T0b_total
se_dif_50_60 <- sqrt(abs(p1_age_50_60*(1-p1_age_50_60)/n_T0_total+p2_age_50_60*(1-p2_age_50_60)/n_T0b_total ))
diff1_50_60 <- (p1_age_50_60-p2_age_50_60)
diff_50_60 <-  data.frame(sapply("diff1erence_50_60", function(cell) paste0(round(100*diff1_50_60, digits=1), " (", round(100*(diff1_50_60-1.96*se_dif_50_60), digits = 1), ",", round(100*(diff1_50_60+1.96*se_dif_50_60), digits = 1), ")"), USE.NAMES = T))
names(diff_50_60)[1] <- "Difference with CI"

p2_age_over_60 <- n_age_over_60_T0/n_T0_total 
p1_age_over_60 <- n_age_over_60_T0b/n_T0b_total
se_dif_over_60 <- sqrt(abs(p1_age_over_60*(1-p1_age_over_60)/n_T0_total+p2_age_over_60*(1-p2_age_over_60)/n_T0b_total ))
diff1_over_60 <- (p1_age_over_60-p2_age_over_60)
diff_over_60 <-  data.frame(sapply("difference_over60", function(cell) paste0(round(100*diff1_over_60, digits=1), " (", round(100*(diff1_over_60-1.96*se_dif_over_60), digits = 1), ",", round(100*(diff1_over_60+1.96*se_dif_over_60), digits = 1), ")"), USE.NAMES = T))
names(diff_over_60)[1] <- "Difference with CI"


# Smoking
n_smoking_yes_total <- nrow(subset(dat_all_sub, dat_all_sub$Smoking=="Yes"))
n_smoking_yes_T0b <- nrow(subset(dat_all_sub,dat_all_sub$Smoking=="Yes" & dat_all_sub$dataset=="Responders"))
n_smoking_yes_T0 <- nrow(subset(dat_all_sub,dat_all_sub$Smoking=="Yes" & dat_all_sub$dataset=="Non-responders"))
n_smoking_no_total <- nrow(subset(dat_all_sub, dat_all_sub$Smoking=="No"))
n_smoking_no_T0b <- nrow(subset(dat_all_sub,dat_all_sub$Smoking=="No" & dat_all_sub$dataset=="Responders"))
n_smoking_no_T0 <- nrow(subset(dat_all_sub,dat_all_sub$Smoking=="No" & dat_all_sub$dataset=="Non-responders"))
n_smoking_prev_total <- nrow(subset(dat_all_sub, dat_all_sub$Smoking=="Previous smoker"))
n_smoking_prev_T0b <- nrow(subset(dat_all_sub,dat_all_sub$Smoking=="Previous smoker" & dat_all_sub$dataset=="Responders"))
n_smoking_prev_T0 <- nrow(subset(dat_all_sub,dat_all_sub$Smoking=="Previous smoker" & dat_all_sub$dataset=="Non-responders"))

p2_smo_yes <- n_smoking_yes_T0/n_T0_total 
p1_smo_yes <- n_smoking_yes_T0b/n_T0b_total
se_dif_smo_yes <- sqrt(abs(p1_smo_yes*(1-p1_smo_yes)/n_T0_total+p2_smo_yes*(1-p2_smo_yes)/n_T0b_total ))
diff1_smo_yes <- (p1_smo_yes-p2_smo_yes)
diff_smo_yes <-  data.frame(sapply("difference_smo_yes", function(cell) paste0(round(100*diff1_smo_yes, digits=1), " (", round(100*(diff1_smo_yes-1.96*se_dif_smo_yes), digits = 1), ",", round(100*(diff1_smo_yes+1.96*se_dif_smo_yes), digits = 1), ")"), USE.NAMES = T))
names(diff_smo_yes)[1] <- "Difference with CI"

p2_smo_no <- n_smoking_no_T0/n_T0_total 
p1_smo_no <- n_smoking_no_T0b/n_T0b_total
se_dif_smo_no <- sqrt(abs(p1_smo_no*(1-p1_smo_no)/n_T0_total+p2_smo_no*(1-p2_smo_no)/n_T0b_total ))
diff1_smo_no <- (p1_smo_no-p2_smo_no)
diff_smo_no <-  data.frame(sapply("difference_smo_no", function(cell) paste0(round(100*diff1_smo_no, digits=1), " (", round(100*(diff1_smo_no-1.96*se_dif_smo_no), digits = 1), ",", round(100*(diff1_smo_no+1.96*se_dif_smo_no), digits = 1), ")"), USE.NAMES = T))
names(diff_smo_no)[1] <- "Difference with CI"

p2_smo_prev <- n_smoking_prev_T0/n_T0_total 
p1_smo_prev <- n_smoking_prev_T0b/n_T0b_total
se_dif_smo_prev <- sqrt(abs(p1_smo_prev*(1-p1_smo_prev)/n_T0_total+p2_smo_prev*(1-p2_smo_prev)/n_T0b_total ))
diff1_smo_prev <- (p1_smo_prev-p2_smo_prev)
diff_smo_prev <-  data.frame(sapply("difference_smo_prev", function(cell) paste0(round(100*diff1_smo_prev, digits=1), " (", round(100*(diff1_smo_prev-1.96*se_dif_smo_prev), digits = 1), ",", round(100*(diff1_smo_prev+1.96*se_dif_smo_prev), digits = 1), ")"), USE.NAMES = T))
names(diff_smo_prev)[1] <- "Difference with CI"

# Alcohol     Never    Once in a while   < 4 glasses/week 4 - 7 glasses/week   > 7 glasses/week Don’t know 
n_alcohol_no_total <- nrow(subset(dat_all_sub, dat_all_sub$Alcohol=="Never"))
n_alcohol_no_T0b <- nrow(subset(dat_all_sub,dat_all_sub$Alcohol=="Never" & dat_all_sub$dataset=="Responders"))
n_alcohol_no_T0 <- nrow(subset(dat_all_sub,dat_all_sub$Alcohol=="Never" & dat_all_sub$dataset=="Non-responders"))
n_alcohol_once_total <- nrow(subset(dat_all_sub, dat_all_sub$Alcohol=="Once in a while"))
n_alcohol_once_T0b <- nrow(subset(dat_all_sub,dat_all_sub$Alcohol=="Once in a while" & dat_all_sub$dataset=="Responders"))
n_alcohol_once_T0 <- nrow(subset(dat_all_sub,dat_all_sub$Alcohol=="Once in a while" & dat_all_sub$dataset=="Non-responders"))
n_alcohol_4_total <- nrow(subset(dat_all_sub, dat_all_sub$Alcohol=="< 4 glasses/week"))
n_alcohol_4_T0b <- nrow(subset(dat_all_sub,dat_all_sub$Alcohol=="< 4 glasses/week" & dat_all_sub$dataset=="Responders"))
n_alcohol_4_T0 <- nrow(subset(dat_all_sub,dat_all_sub$Alcohol=="< 4 glasses/week" & dat_all_sub$dataset=="Non-responders"))
n_alcohol_4_7_total <- nrow(subset(dat_all_sub, dat_all_sub$Alcohol=="4 - 7 glasses/week"))
n_alcohol_4_7_T0b <- nrow(subset(dat_all_sub,dat_all_sub$Alcohol=="4 - 7 glasses/week" & dat_all_sub$dataset=="Responders"))
n_alcohol_4_7_T0 <- nrow(subset(dat_all_sub,dat_all_sub$Alcohol=="4 - 7 glasses/week" & dat_all_sub$dataset=="Non-responders"))
n_alcohol_7_total <- nrow(subset(dat_all_sub, dat_all_sub$Alcohol=="> 7 glasses/week"))
n_alcohol_7_T0b <- nrow(subset(dat_all_sub,dat_all_sub$Alcohol=="> 7 glasses/week" & dat_all_sub$dataset=="Responders"))
n_alcohol_7_T0 <- nrow(subset(dat_all_sub,dat_all_sub$Alcohol=="> 7 glasses/week" & dat_all_sub$dataset=="Non-responders"))
n_alcohol_dont_know_total <- nrow(subset(dat_all_sub, dat_all_sub$Alcohol=="Don’t know"))
n_alcohol_dont_know_T0b <- nrow(subset(dat_all_sub,dat_all_sub$Alcohol=="Don’t know" & dat_all_sub$dataset=="Responders"))
n_alcohol_dont_know_T0 <- nrow(subset(dat_all_sub,dat_all_sub$Alcohol=="Don’t know" & dat_all_sub$dataset=="Non-responders"))


p2_alcohol_no <- n_alcohol_no_T0/n_T0_total 
p1_alcohol_no <- n_alcohol_no_T0b/n_T0b_total
se_dif_alcohol_no <- sqrt(abs(p1_alcohol_no*(1-p1_alcohol_no)/n_T0_total+p2_alcohol_no*(1-p2_alcohol_no)/n_T0b_total ))
diff1_alcohol_no <- (p1_alcohol_no-p2_alcohol_no)
diff_alcohol_no <-  data.frame(sapply("difference_alcohol_no", function(cell) paste0(round(100*diff1_alcohol_no, digits=1), " (", round(100*(diff1_alcohol_no-1.96*se_dif_alcohol_no), digits = 1), ",", round(100*(diff1_alcohol_no+1.96*se_dif_alcohol_no), digits = 1), ")"), USE.NAMES = T))
names(diff_alcohol_no)[1] <- "Difference with CI"

p2_alcohol_once <- n_alcohol_once_T0/n_T0_total 
p1_alcohol_once <- n_alcohol_once_T0b/n_T0b_total
se_dif_alcohol_once <- sqrt(abs(p1_alcohol_once*(1-p1_alcohol_once)/n_T0_total+p2_alcohol_once*(1-p2_alcohol_once)/n_T0b_total ))
diff1_alcohol_once <- (p1_alcohol_once-p2_alcohol_once)
diff_alcohol_once <-  data.frame(sapply("difference_alcohol_once", function(cell) paste0(round(100*diff1_alcohol_once, digits=1), " (", round(100*(diff1_alcohol_once-1.96*se_dif_alcohol_once), digits = 1), ",", round(100*(diff1_alcohol_once+1.96*se_dif_alcohol_once), digits = 1), ")"), USE.NAMES = T))
names(diff_alcohol_once)[1] <- "Difference with CI"

p2_alcohol_4 <- n_alcohol_4_T0/n_T0_total 
p1_alcohol_4 <- n_alcohol_4_T0b/n_T0b_total
se_dif_alcohol_4 <- sqrt(abs(p1_alcohol_4*(1-p1_alcohol_4)/n_T0_total+p2_alcohol_4*(1-p2_alcohol_4)/n_T0b_total ))
diff1_alcohol_4 <- (p1_alcohol_4-p2_alcohol_4)
diff_alcohol_4 <-  data.frame(sapply("difference_alcohol_4", function(cell) paste0(round(100*diff1_alcohol_4, digits=1), " (", round(100*(diff1_alcohol_4-1.96*se_dif_alcohol_4), digits = 1), ",", round(100*(diff1_alcohol_4+1.96*se_dif_alcohol_4), digits = 1), ")"), USE.NAMES = T))
names(diff_alcohol_4)[1] <- "Difference with CI"

p2_alcohol_4_7 <- n_alcohol_4_7_T0/n_T0_total 
p1_alcohol_4_7 <- n_alcohol_4_7_T0b/n_T0b_total
se_dif_alcohol_4_7 <- sqrt(abs(p1_alcohol_4_7*(1-p1_alcohol_4_7)/n_T0_total+p2_alcohol_4_7*(1-p2_alcohol_4_7)/n_T0b_total ))
diff1_alcohol_4_7 <- (p1_alcohol_4_7-p2_alcohol_4_7)
diff_alcohol_4_7 <-  data.frame(sapply("difference_alcohol_4_7", function(cell) paste0(round(100*diff1_alcohol_4_7, digits=1), " (", round(100*(diff1_alcohol_4_7-1.96*se_dif_alcohol_4_7), digits = 1), ",", round(100*(diff1_alcohol_4_7+1.96*se_dif_alcohol_4_7), digits = 1), ")"), USE.NAMES = T))
names(diff_alcohol_4_7)[1] <- "Difference with CI"

p2_alcohol_7 <- n_alcohol_7_T0/n_T0_total 
p1_alcohol_7 <- n_alcohol_7_T0b/n_T0b_total
se_dif_alcohol_7 <- sqrt(abs(p1_alcohol_7*(1-p1_alcohol_7)/n_T0_total+p2_alcohol_7*(1-p2_alcohol_7)/n_T0b_total ))
diff1_alcohol_7 <- (p1_alcohol_7-p2_alcohol_7)
diff_alcohol_7 <-  data.frame(sapply("difference_alcohol_7", function(cell) paste0(round(100*diff1_alcohol_7, digits=1), " (", round(100*(diff1_alcohol_7-1.96*se_dif_alcohol_7), digits = 1), ",", round(100*(diff1_alcohol_7+1.96*se_dif_alcohol_7), digits = 1), ")"), USE.NAMES = T))
names(diff_alcohol_7)[1] <- "Difference with CI"

p2_alcohol_dont_know <- n_alcohol_dont_know_T0/n_T0_total 
p1_alcohol_dont_know <- n_alcohol_dont_know_T0b/n_T0b_total
se_dif_alcohol_dont_know <- sqrt(abs(p1_alcohol_dont_know*(1-p1_alcohol_dont_know)/n_T0_total+p2_alcohol_dont_know*(1-p2_alcohol_dont_know)/n_T0b_total ))
diff1_alcohol_dont_know <- (p1_alcohol_dont_know-p2_alcohol_dont_know)
diff_alcohol_dont_know <-  data.frame(sapply("difference_alcohol_dont_know", function(cell) paste0(round(100*diff1_alcohol_dont_know, digits=1), " (", round(100*(diff1_alcohol_dont_know-1.96*se_dif_alcohol_dont_know), digits = 1), ",", round(100*(diff1_alcohol_dont_know+1.96*se_dif_alcohol_dont_know), digits = 1), ")"), USE.NAMES = T))
names(diff_alcohol_dont_know)[1] <- "Difference with CI"

# BMI
n_BMI_18_total <- nrow(subset(dat_all_sub, dat_all_sub$BMI_4cat=="<18.5"))
n_BMI_18_T0b <- nrow(subset(dat_all_sub,dat_all_sub$BMI_4cat=="<18.5" & dat_all_sub$dataset=="Responders"))
n_BMI_18_T0 <- nrow(subset(dat_all_sub,dat_all_sub$BMI_4cat=="<18.5" & dat_all_sub$dataset=="Non-responders"))
n_BMI_18_24_total <- nrow(subset(dat_all_sub, dat_all_sub$BMI_4cat=="18.5-24.9"))
n_BMI_18_24_T0b <- nrow(subset(dat_all_sub,dat_all_sub$BMI_4cat=="18.5-24.9" & dat_all_sub$dataset=="Responders"))
n_BMI_18_24_T0 <- nrow(subset(dat_all_sub,dat_all_sub$BMI_4cat=="18.5-24.9" & dat_all_sub$dataset=="Non-responders"))
n_BMI_25_29_total <- nrow(subset(dat_all_sub, dat_all_sub$BMI_4cat=="25-29.9"))
n_BMI_25_29_T0b <- nrow(subset(dat_all_sub,dat_all_sub$BMI_4cat=="25-29.9" & dat_all_sub$dataset=="Responders"))
n_BMI_25_29_T0 <- nrow(subset(dat_all_sub,dat_all_sub$BMI_4cat=="25-29.9" & dat_all_sub$dataset=="Non-responders"))
n_BMI_30_total <- nrow(subset(dat_all_sub, dat_all_sub$BMI_4cat=="30+"))
n_BMI_30_T0b <- nrow(subset(dat_all_sub,dat_all_sub$BMI_4cat=="30+" & dat_all_sub$dataset=="Responders"))
n_BMI_30_T0 <- nrow(subset(dat_all_sub,dat_all_sub$BMI_4cat=="30+" & dat_all_sub$dataset=="Non-responders"))

p2_BMI_18 <- n_BMI_18_T0/n_T0_total 
p1_BMI_18 <- n_BMI_18_T0b/n_T0b_total
se_dif_BMI_18 <- sqrt(abs(p1_BMI_18*(1-p1_BMI_18)/n_T0_total+p2_BMI_18*(1-p2_BMI_18)/n_T0b_total ))
diff1_BMI_18 <- (p1_BMI_18-p2_BMI_18)
diff_BMI_18 <-  data.frame(sapply("difference_BMI_18", function(cell) paste0(round(100*diff1_BMI_18, digits=1), " (", round(100*(diff1_BMI_18-1.96*se_dif_BMI_18), digits = 1), ",", round(100*(diff1_BMI_18+1.96*se_dif_BMI_18), digits = 1), ")"), USE.NAMES = T))
names(diff_BMI_18)[1] <- "Difference with CI"

p2_BMI_18_24 <- n_BMI_18_24_T0/n_T0_total 
p1_BMI_18_24 <- n_BMI_18_24_T0b/n_T0b_total
se_dif_BMI_18_24 <- sqrt(abs(p1_BMI_18_24*(1-p1_BMI_18_24)/n_T0_total+p2_BMI_18_24*(1-p2_BMI_18_24)/n_T0b_total ))
diff1_BMI_18_24 <- (p1_BMI_18_24-p2_BMI_18_24)
diff_BMI_18_24 <-  data.frame(sapply("difference_BMI_18_24", function(cell) paste0(round(100*diff1_BMI_18_24, digits=1), " (", round(100*(diff1_BMI_18_24-1.96*se_dif_BMI_18_24), digits = 1), ",", round(100*(diff1_BMI_18_24+1.96*se_dif_BMI_18_24), digits = 1), ")"), USE.NAMES = T))
names(diff_BMI_18_24)[1] <- "Difference with CI"

p2_BMI_25_29 <- n_BMI_25_29_T0/n_T0_total 
p1_BMI_25_29 <- n_BMI_25_29_T0b/n_T0b_total
se_dif_BMI_25_29 <- sqrt(abs(p1_BMI_25_29*(1-p1_BMI_25_29)/n_T0_total+p2_BMI_25_29*(1-p2_BMI_25_29)/n_T0b_total ))
diff1_BMI_25_29 <- (p1_BMI_25_29-p2_BMI_25_29)
diff_BMI_25_29 <-  data.frame(sapply("difference_BMI_25_29", function(cell) paste0(round(100*diff1_BMI_25_29, digits=1), " (", round(100*(diff1_BMI_25_29-1.96*se_dif_BMI_25_29), digits = 1), ",", round(100*(diff1_BMI_25_29+1.96*se_dif_BMI_25_29), digits = 1), ")"), USE.NAMES = T))
names(diff_BMI_25_29)[1] <- "Difference with CI"

p2_BMI_30 <- n_BMI_30_T0/n_T0_total 
p1_BMI_30 <- n_BMI_30_T0b/n_T0b_total
se_dif_BMI_30 <- sqrt(abs(p1_BMI_30*(1-p1_BMI_30)/n_T0_total+p2_BMI_30*(1-p2_BMI_30)/n_T0b_total ))
diff1_BMI_30 <- (p1_BMI_30-p2_BMI_30)
diff_BMI_30 <-  data.frame(sapply("difference_BMI_30", function(cell) paste0(round(100*diff1_BMI_30, digits=1), " (", round(100*(diff1_BMI_30-1.96*se_dif_BMI_30), digits = 1), ",", round(100*(diff1_BMI_30+1.96*se_dif_BMI_30), digits = 1), ")"), USE.NAMES = T))
names(diff_BMI_30)[1] <- "Difference with CI"

# Family diagnoses
n_fam_no_total <- nrow(subset(dat_all_sub, dat_all_sub$Family_diag_Yes_No=="No"))
n_fam_no_T0b <- nrow(subset(dat_all_sub,dat_all_sub$Family_diag_Yes_No=="No" & dat_all_sub$dataset=="Responders"))
n_fam_no_T0 <- nrow(subset(dat_all_sub,dat_all_sub$Family_diag_Yes_No=="No" & dat_all_sub$dataset=="Non-responders"))
n_fam_yes_total <- nrow(subset(dat_all_sub, dat_all_sub$Family_diag_Yes_No=="Yes"))
n_fam_yes_T0b <- nrow(subset(dat_all_sub,dat_all_sub$Family_diag_Yes_No=="Yes" & dat_all_sub$dataset=="Responders"))
n_fam_yes_T0 <- nrow(subset(dat_all_sub,dat_all_sub$Family_diag_Yes_No=="Yes" & dat_all_sub$dataset=="Non-responders"))
n_fam_dont_know_total <- nrow(subset(dat_all_sub, dat_all_sub$Family_diag_Yes_No=="Don't know"))
n_fam_dont_know_T0b <- nrow(subset(dat_all_sub,dat_all_sub$Family_diag_Yes_No=="Don't know" & dat_all_sub$dataset=="Responders"))
n_fam_dont_know_T0 <- nrow(subset(dat_all_sub,dat_all_sub$Family_diag_Yes_No=="Don't know" & dat_all_sub$dataset=="Non-responders"))

p2_fam_no <- n_fam_no_T0/n_T0_total 
p1_fam_no <- n_fam_no_T0b/n_T0b_total
se_dif_fam_no <- sqrt(abs(p1_fam_no*(1-p1_fam_no)/n_T0_total+p2_fam_no*(1-p2_fam_no)/n_T0b_total ))
diff1_fam_no <- (p1_fam_no-p2_fam_no)
diff_fam_no <-  data.frame(sapply("difference_fam_no", function(cell) paste0(round(100*diff1_fam_no, digits=1), " (", round(100*(diff1_fam_no-1.96*se_dif_fam_no), digits = 1), ",", round(100*(diff1_fam_no+1.96*se_dif_fam_no), digits = 1), ")"), USE.NAMES = T))
names(diff_fam_no)[1] <- "Difference with CI"

p2_fam_yes <- n_fam_yes_T0/n_T0_total 
p1_fam_yes <- n_fam_yes_T0b/n_T0b_total
se_dif_fam_yes <- sqrt(abs(p1_fam_yes*(1-p1_fam_yes)/n_T0_total+p2_fam_yes*(1-p2_fam_yes)/n_T0b_total ))
diff1_fam_yes <- (p1_fam_yes-p2_fam_yes)
diff_fam_yes <-  data.frame(sapply("difference_fam_yes", function(cell) paste0(round(100*diff1_fam_yes, digits=1), " (", round(100*(diff1_fam_yes-1.96*se_dif_fam_yes), digits = 1), ",", round(100*(diff1_fam_yes+1.96*se_dif_fam_yes), digits = 1), ")"), USE.NAMES = T))
names(diff_fam_yes)[1] <- "Difference with CI"

p2_fam_dont_know <- n_fam_dont_know_T0/n_T0_total 
p1_fam_dont_know <- n_fam_dont_know_T0b/n_T0b_total
se_dif_fam_dont_know <- sqrt(abs(p1_fam_dont_know*(1-p1_fam_dont_know)/n_T0_total+p2_fam_dont_know*(1-p2_fam_dont_know)/n_T0b_total ))
diff1_fam_dont_know <- (p1_fam_dont_know-p2_fam_dont_know)
diff_fam_dont_know <-  data.frame(sapply("difference_fam_dont_know", function(cell) paste0(round(100*diff1_fam_dont_know, digits=1), " (", round(100*(diff1_fam_dont_know-1.96*se_dif_fam_dont_know), digits = 1), ",", round(100*(diff1_fam_dont_know+1.96*se_dif_fam_dont_know), digits = 1), ")"), USE.NAMES = T))
names(diff_fam_dont_know)[1] <- "Difference with CI"

# Complaints
n_Swelling_T0b <- nrow(subset(dat_all_sub,dat_all_sub$Swelling==1 & dat_all_sub$dataset=="Responders"))
n_Swelling_T0 <- nrow(subset(dat_all_sub,dat_all_sub$Swelling==1 & dat_all_sub$dataset=="Non-responders"))
n_Pain_T0b <- nrow(subset(dat_all_sub,dat_all_sub$Pain==1 & dat_all_sub$dataset=="Responders"))
n_Pain_T0 <- nrow(subset(dat_all_sub,dat_all_sub$Pain==1 & dat_all_sub$dataset=="Non-responders"))
n_MorningStiffness_T0b <- nrow(subset(dat_all_sub,dat_all_sub$MorningStiffness==1 & dat_all_sub$dataset=="Responders"))
n_MorningStiffness_T0 <- nrow(subset(dat_all_sub,dat_all_sub$MorningStiffness==1 & dat_all_sub$dataset=="Non-responders"))
n_AllDayStiffness_T0b <- nrow(subset(dat_all_sub,dat_all_sub$AllDayStiffness==1 & dat_all_sub$dataset=="Responders"))
n_AllDayStiffness_T0 <- nrow(subset(dat_all_sub,dat_all_sub$AllDayStiffness==1 & dat_all_sub$dataset=="Non-responders"))
n_Exhaustion_T0b <- nrow(subset(dat_all_sub,dat_all_sub$Exhaustion==1 & dat_all_sub$dataset=="Responders"))
n_Exhaustion_T0 <- nrow(subset(dat_all_sub,dat_all_sub$Exhaustion==1 & dat_all_sub$dataset=="Non-responders"))
n_ReducedEndurance_T0b <- nrow(subset(dat_all_sub,dat_all_sub$ReducedEndurance==1 & dat_all_sub$dataset=="Responders"))
n_ReducedEndurance_T0 <- nrow(subset(dat_all_sub,dat_all_sub$ReducedEndurance==1 & dat_all_sub$dataset=="Non-responders"))
n_NoneOfTheAbove_T0b <- nrow(subset(dat_all_sub,dat_all_sub$NoneOfTheAbove==1 & dat_all_sub$dataset=="Responders"))
n_NoneOfTheAbove_T0 <- nrow(subset(dat_all_sub,dat_all_sub$NoneOfTheAbove==1 & dat_all_sub$dataset=="Non-responders"))


p2_Swelling <- n_Swelling_T0/n_T0_total 
p1_Swelling <- n_Swelling_T0b/n_T0b_total
se_dif_Swelling <- sqrt(abs(p1_Swelling*(1-p1_Swelling)/n_T0_total+p2_Swelling*(1-p2_Swelling)/n_T0b_total ))
diff1_Swelling <- (p1_Swelling-p2_Swelling)
diff_Swelling <-  data.frame(sapply("difference_Swelling", function(cell) paste0(round(100*diff1_Swelling, digits=1), " (", round(100*(diff1_Swelling-1.96*se_dif_Swelling), digits = 1), ",", round(100*(diff1_Swelling+1.96*se_dif_Swelling), digits = 1), ")"), USE.NAMES = T))
names(diff_Swelling)[1] <- "Difference with CI"

p2_Pain <- n_Pain_T0/n_T0_total 
p1_Pain <- n_Pain_T0b/n_T0b_total
se_dif_Pain <- sqrt(abs(p1_Pain*(1-p1_Pain)/n_T0_total+p2_Pain*(1-p2_Pain)/n_T0b_total ))
diff1_Pain <- (p1_Pain-p2_Pain)
diff_Pain <-  data.frame(sapply("difference_Pain", function(cell) paste0(round(100*diff1_Pain, digits=1), " (", round(100*(diff1_Pain-1.96*se_dif_Pain), digits = 1), ",", round(100*(diff1_Pain+1.96*se_dif_Pain), digits = 1), ")"), USE.NAMES = T))
names(diff_Pain)[1] <- "Difference with CI"

p2_MorningStiffness <- n_MorningStiffness_T0/n_T0_total 
p1_MorningStiffness <- n_MorningStiffness_T0b/n_T0b_total
se_dif_MorningStiffness <- sqrt(abs(p1_MorningStiffness*(1-p1_MorningStiffness)/n_T0_total+p2_MorningStiffness*(1-p2_MorningStiffness)/n_T0b_total ))
diff1_MorningStiffness <- (p1_MorningStiffness-p2_MorningStiffness)
diff_MorningStiffness <-  data.frame(sapply("difference_MorningStiffness", function(cell) paste0(round(100*diff1_MorningStiffness, digits=1), " (", round(100*(diff1_MorningStiffness-1.96*se_dif_MorningStiffness), digits = 1), ",", round(100*(diff1_MorningStiffness+1.96*se_dif_MorningStiffness), digits = 1), ")"), USE.NAMES = T))
names(diff_MorningStiffness)[1] <- "Difference with CI"

p2_AllDayStiffness <- n_AllDayStiffness_T0/n_T0_total 
p1_AllDayStiffness <- n_AllDayStiffness_T0b/n_T0b_total
se_dif_AllDayStiffness <- sqrt(abs(p1_AllDayStiffness*(1-p1_AllDayStiffness)/n_T0_total+p2_AllDayStiffness*(1-p2_AllDayStiffness)/n_T0b_total ))
diff1_AllDayStiffness <- (p1_AllDayStiffness-p2_AllDayStiffness)
diff_AllDayStiffness <-  data.frame(sapply("difference_AllDayStiffness", function(cell) paste0(round(100*diff1_AllDayStiffness, digits=1), " (", round(100*(diff1_AllDayStiffness-1.96*se_dif_AllDayStiffness), digits = 1), ",", round(100*(diff1_AllDayStiffness+1.96*se_dif_AllDayStiffness), digits = 1), ")"), USE.NAMES = T))
names(diff_AllDayStiffness)[1] <- "Difference with CI"

p2_Exhaustion <- n_Exhaustion_T0/n_T0_total 
p1_Exhaustion <- n_Exhaustion_T0b/n_T0b_total
se_dif_Exhaustion <- sqrt(abs(p1_Exhaustion*(1-p1_Exhaustion)/n_T0_total+p2_Exhaustion*(1-p2_Exhaustion)/n_T0b_total ))
diff1_Exhaustion <- (p1_Exhaustion-p2_Exhaustion)
diff_Exhaustion <-  data.frame(sapply("difference_Exhaustion", function(cell) paste0(round(100*diff1_Exhaustion, digits=1), " (", round(100*(diff1_Exhaustion-1.96*se_dif_Exhaustion), digits = 1), ",", round(100*(diff1_Exhaustion+1.96*se_dif_Exhaustion), digits = 1), ")"), USE.NAMES = T))
names(diff_Exhaustion)[1] <- "Difference with CI"

p2_ReducedEndurance <- n_ReducedEndurance_T0/n_T0_total 
p1_ReducedEndurance <- n_ReducedEndurance_T0b/n_T0b_total
se_dif_ReducedEndurance <- sqrt(abs(p1_ReducedEndurance*(1-p1_ReducedEndurance)/n_T0_total+p2_ReducedEndurance*(1-p2_ReducedEndurance)/n_T0b_total ))
diff1_ReducedEndurance <- (p1_ReducedEndurance-p2_ReducedEndurance)
diff_ReducedEndurance <-  data.frame(sapply("difference_ReducedEndurance", function(cell) paste0(round(100*diff1_ReducedEndurance, digits=1), " (", round(100*(diff1_ReducedEndurance-1.96*se_dif_ReducedEndurance), digits = 1), ",", round(100*(diff1_ReducedEndurance+1.96*se_dif_ReducedEndurance), digits = 1), ")"), USE.NAMES = T))
names(diff_ReducedEndurance)[1] <- "Difference with CI"

p2_NoneOfTheAbove <- n_NoneOfTheAbove_T0/n_T0_total 
p1_NoneOfTheAbove <- n_NoneOfTheAbove_T0b/n_T0b_total
se_dif_NoneOfTheAbove <- sqrt(abs(p1_NoneOfTheAbove*(1-p1_NoneOfTheAbove)/n_T0_total+p2_NoneOfTheAbove*(1-p2_NoneOfTheAbove)/n_T0b_total ))
diff1_NoneOfTheAbove <- (p1_NoneOfTheAbove-p2_NoneOfTheAbove)
diff_NoneOfTheAbove <-  data.frame(sapply("difference_NoneOfTheAbove", function(cell) paste0(round(100*diff1_NoneOfTheAbove, digits=1), " (", round(100*(diff1_NoneOfTheAbove-1.96*se_dif_NoneOfTheAbove), digits = 1), ",", round(100*(diff1_NoneOfTheAbove+1.96*se_dif_NoneOfTheAbove), digits = 1), ")"), USE.NAMES = T))
names(diff_NoneOfTheAbove)[1] <- "Difference with CI"

# Previous diagnoses
n_diag_no_total <- nrow(subset(dat_all_sub, dat_all_sub$Diag_T0_T0b=="No"))
n_diag_no_T0b <- nrow(subset(dat_all_sub,dat_all_sub$Diag_T0_T0b=="No" & dat_all_sub$dataset=="Responders"))
n_diag_no_T0 <- nrow(subset(dat_all_sub,dat_all_sub$Diag_T0_T0b=="No" & dat_all_sub$dataset=="Non-responders"))
n_diag_yes_total <- nrow(subset(dat_all_sub, dat_all_sub$Diag_T0_T0b=="Yes"))
n_diag_yes_T0b <- nrow(subset(dat_all_sub,dat_all_sub$Diag_T0_T0b=="Yes" & dat_all_sub$dataset=="Responders"))
n_diag_yes_T0 <- nrow(subset(dat_all_sub,dat_all_sub$Diag_T0_T0b=="Yes" & dat_all_sub$dataset=="Non-responders"))
n_diag_dont_know_total <- nrow(subset(dat_all_sub, dat_all_sub$Diag_T0_T0b=="Don't know"))
n_diag_dont_know_T0b <- nrow(subset(dat_all_sub,dat_all_sub$Diag_T0_T0b=="Don't know" & dat_all_sub$dataset=="Responders"))
n_diag_dont_know_T0 <- nrow(subset(dat_all_sub,dat_all_sub$Diag_T0_T0b=="Don't know" & dat_all_sub$dataset=="Non-responders"))

p2_diag_no <- n_diag_no_T0/n_T0_total 
p1_diag_no <- n_diag_no_T0b/n_T0b_total
se_dif_diag_no <- sqrt(abs(p1_diag_no*(1-p1_diag_no)/n_T0_total+p2_diag_no*(1-p2_diag_no)/n_T0b_total ))
diff1_diag_no <- (p1_diag_no-p2_diag_no)
diff_diag_no <-  data.frame(sapply("difference_diag_no", function(cell) paste0(round(100*diff1_diag_no, digits=1), " (", round(100*(diff1_diag_no-1.96*se_dif_diag_no), digits = 1), ",", round(100*(diff1_diag_no+1.96*se_dif_diag_no), digits = 1), ")"), USE.NAMES = T))
names(diff_diag_no)[1] <- "Difference with CI"

p2_diag_yes <- n_diag_yes_T0/n_T0_total 
p1_diag_yes <- n_diag_yes_T0b/n_T0b_total
se_dif_diag_yes <- sqrt(abs(p1_diag_yes*(1-p1_diag_yes)/n_T0_total+p2_diag_yes*(1-p2_diag_yes)/n_T0b_total ))
diff1_diag_yes <- (p1_diag_yes-p2_diag_yes)
diff_diag_yes <-  data.frame(sapply("difference_diag_yes", function(cell) paste0(round(100*diff1_diag_yes, digits=1), " (", round(100*(diff1_diag_yes-1.96*se_dif_diag_yes), digits = 1), ",", round(100*(diff1_diag_yes+1.96*se_dif_diag_yes), digits = 1), ")"), USE.NAMES = T))
names(diff_diag_yes)[1] <- "Difference with CI"

p2_diag_dont_know <- n_diag_dont_know_T0/n_T0_total 
p1_diag_dont_know <- n_diag_dont_know_T0b/n_T0b_total
se_dif_diag_dont_know <- sqrt(abs(p1_diag_dont_know*(1-p1_diag_dont_know)/n_T0_total+p2_diag_dont_know*(1-p2_diag_dont_know)/n_T0b_total ))
diff1_diag_dont_know <- (p1_diag_dont_know-p2_diag_dont_know)
diff_diag_dont_know <-  data.frame(sapply("difference_diag_dont_know", function(cell) paste0(round(100*diff1_diag_dont_know, digits=1), " (", round(100*(diff1_diag_dont_know-1.96*se_dif_diag_dont_know), digits = 1), ",", round(100*(diff1_diag_dont_know+1.96*se_dif_diag_dont_know), digits = 1), ")"), USE.NAMES = T))
names(diff_diag_dont_know)[1] <- "Difference with CI"

spacer <- rep("",1)

diff_percent_all <- diff_percent_all %>%
  add_row("Difference with CI"=spacer) %>%
  add_row(diff_under_40) %>%
  add_row(diff_under_50) %>%
  add_row(diff_50_60) %>%
  add_row(diff_over_60) %>%
  add_row("Difference with CI"=spacer) %>%
  add_row(diff_smo_yes) %>%
  add_row(diff_smo_no) %>%
  add_row(diff_smo_prev) %>%
  add_row("Difference with CI"=spacer) %>%
  add_row(diff_alcohol_no) %>%
  add_row(diff_alcohol_once) %>%
  add_row(diff_alcohol_4) %>%
  add_row(diff_alcohol_4_7) %>%
  add_row(diff_alcohol_7) %>%
  add_row(diff_alcohol_dont_know) %>%
  add_row("Difference with CI"=spacer) %>%
  add_row(diff_BMI_18) %>%
  add_row(diff_BMI_18_24) %>%
  add_row(diff_BMI_25_29) %>%
  add_row(diff_BMI_30) %>%
  add_row("Difference with CI"=spacer) %>%
  add_row(diff_fam_no) %>%
  add_row(diff_fam_yes) %>%
  add_row(diff_fam_dont_know) %>%
  add_row(diff_Swelling) %>%
  add_row(diff_Pain) %>%
  add_row(diff_MorningStiffness) %>%
  add_row(diff_AllDayStiffness) %>%
  add_row(diff_Exhaustion) %>%
  add_row(diff_ReducedEndurance) %>%
  add_row(diff_NoneOfTheAbove) %>%
  add_row("Difference with CI"=spacer) %>%
  add_row(diff_diag_no) %>%
  add_row(diff_diag_yes) %>%
  add_row(diff_diag_dont_know) 

library("writexl")
write_xlsx(diff_percent_all,"../Table_dataset_differences.xlsx")

### Forest plot/Box and whiskers plot
diff_dataset_plot <- data.frame(estimate=100*c(0,diff1_sex,
                                               0,
  diff1_under_40,
  diff1_under_50,
  diff1_50_60,
  diff1_over_60,
  0,
  diff1_smo_yes,
  diff1_smo_no,
  diff1_smo_prev,
  0,
  diff1_alcohol_no,
  diff1_alcohol_once,
  diff1_alcohol_4,
  diff1_alcohol_4_7,
  diff1_alcohol_7,
  diff1_alcohol_dont_know,
  0,
  diff1_BMI_18,
  diff1_BMI_18_24,
  diff1_BMI_25_29,
  diff1_BMI_30,
  0,
  diff1_fam_no,
  diff1_fam_yes,
  diff1_fam_dont_know,
  diff1_Swelling,
  diff1_Pain,
  diff1_MorningStiffness,
  diff1_AllDayStiffness,
  diff1_Exhaustion,
  diff1_ReducedEndurance,
  diff1_NoneOfTheAbove,
  0,
  diff1_diag_no,
  diff1_diag_yes,
  diff1_diag_dont_know),
  CI_low =100*c(0,diff1_sex-1.96*se_dif_sex,
                0,
                diff1_under_40-1.96*se_dif_under_40,
                diff1_under_50-1.96*se_dif_under_50,
                diff1_50_60-1.96*se_dif_50_60,
                diff1_over_60-1.96*se_dif_over_60,
                0,
                diff1_smo_yes-1.96*se_dif_smo_yes,
                diff1_smo_no-1.96*se_dif_smo_no,
                diff1_smo_prev-1.96*se_dif_smo_prev,
                0,
                diff1_alcohol_no-1.96*se_dif_alcohol_no,
                diff1_alcohol_once-1.96*se_dif_alcohol_once,
                diff1_alcohol_4-1.96*se_dif_alcohol_4,
                diff1_alcohol_4_7-1.96*se_dif_alcohol_4_7,
                diff1_alcohol_7-1.96*se_dif_alcohol_7,
                diff1_alcohol_dont_know-1.96*se_dif_alcohol_dont_know,
                0,
                diff1_BMI_18-1.96*se_dif_BMI_18,
                diff1_BMI_18_24-1.96*se_dif_BMI_18_24,
                diff1_BMI_25_29-1.96*se_dif_BMI_25_29,
                diff1_BMI_30-1.96*se_dif_BMI_30,
                0,
                diff1_fam_no-1.96*se_dif_fam_no,
                diff1_fam_yes-1.96*se_dif_fam_yes,
                diff1_fam_dont_know-1.96*se_dif_fam_dont_know,
                diff1_Swelling-1.96*se_dif_Swelling,
                diff1_Pain-1.96*se_dif_Pain,
                diff1_MorningStiffness-1.96*se_dif_MorningStiffness,
                diff1_AllDayStiffness-1.96*se_dif_AllDayStiffness,
                diff1_Exhaustion-1.96*se_dif_Exhaustion,
                diff1_ReducedEndurance-1.96*se_dif_ReducedEndurance,
                diff1_NoneOfTheAbove-1.96*se_dif_NoneOfTheAbove,
                0,
                diff1_diag_no-1.96*se_dif_diag_no,
                diff1_diag_yes-1.96*se_dif_diag_yes,
                diff1_diag_dont_know-1.96*se_dif_diag_dont_know),
  CI_high =100*c(0,diff1_sex+1.96*se_dif_sex,
                 0,
                diff1_under_40+1.96*se_dif_under_40,
                diff1_under_50+1.96*se_dif_under_50,
                diff1_50_60+1.96*se_dif_50_60,
                diff1_over_60+1.96*se_dif_over_60,
                0,
                diff1_smo_yes+1.96*se_dif_smo_yes,
                diff1_smo_no+1.96*se_dif_smo_no,
                diff1_smo_prev+1.96*se_dif_smo_prev,
                0,
                diff1_alcohol_no+1.96*se_dif_alcohol_no,
                diff1_alcohol_once+1.96*se_dif_alcohol_once,
                diff1_alcohol_4+1.96*se_dif_alcohol_4,
                diff1_alcohol_4_7+1.96*se_dif_alcohol_4_7,
                diff1_alcohol_7+1.96*se_dif_alcohol_7,
                diff1_alcohol_dont_know+1.96*se_dif_alcohol_dont_know,
                0,
                diff1_BMI_18+1.96*se_dif_BMI_18,
                diff1_BMI_18_24+1.96*se_dif_BMI_18_24,
                diff1_BMI_25_29+1.96*se_dif_BMI_25_29,
                diff1_BMI_30+1.96*se_dif_BMI_30,
                0,
                diff1_fam_no+1.96*se_dif_fam_no,
                diff1_fam_yes+1.96*se_dif_fam_yes,
                diff1_fam_dont_know+1.96*se_dif_fam_dont_know,
                diff1_Swelling+1.96*se_dif_Swelling,
                diff1_Pain+1.96*se_dif_Pain,
                diff1_MorningStiffness+1.96*se_dif_MorningStiffness,
                diff1_AllDayStiffness+1.96*se_dif_AllDayStiffness,
                diff1_Exhaustion+1.96*se_dif_Exhaustion,
                diff1_ReducedEndurance+1.96*se_dif_ReducedEndurance,
                diff1_NoneOfTheAbove+1.96*se_dif_NoneOfTheAbove,
                0,
                diff1_diag_no+1.96*se_dif_diag_no,
                diff1_diag_yes+1.96*se_dif_diag_yes,
                diff1_diag_dont_know+1.96*se_dif_diag_dont_know))
diff_dataset_plot

# Give each row a label
diff_dataset_plot <- diff_dataset_plot %>% add_column(Characteristic = table_dataset_characteristics$"_data"$label[1:38])

########
# Make the levels right
########
# Rename those with the same levels

diff_dataset_plot[25,"Characteristic"] <- "No family diagnosis"
diff_dataset_plot[26,"Characteristic"] <- "Yes family diagnosis"
diff_dataset_plot[27,"Characteristic"] <- "Don't know family diagnosis"

diff_dataset_plot[36,"Characteristic"] <- "No diagnosis T0 or T0b"
diff_dataset_plot[37,"Characteristic"] <- "Yes diagnosis T0 or T0b"
diff_dataset_plot[38,"Characteristic"] <- "Don't know diagnosis T0 or T0b"

diff_dataset_plot$Characteristic <- factor(diff_dataset_plot$Characteristic, levels = rev(diff_dataset_plot$Characteristic))

a <- ggplot(diff_dataset_plot, aes(x=estimate, y=factor(Characteristic), color=factor(Characteristic))) + #, levels = rev(characteristics)), color=factor(Characteristic, levels = rev(characteristics)))) +
  geom_point(shape=1, size=4)+
  geom_errorbar(aes(xmin = CI_low, xmax = CI_high), width = 0.5, linewidth=1.5) +
  labs(x="Difference and 95% CI", y = "Characteristic") +
  theme_classic() +
  theme(legend.position = "none") +
  geom_vline(xintercept = 0, linetype = "longdash")
ggsave("../Plots/forest_plot.pdf",width = 15, height = 10)
ggsave("../Plots/forest_plot.png",width = 15, height = 10)



#################################################################################################
#################################################################################################
#################################################################################################

#### Remake dataset and plot for better visualisation for presentations
#diff_dataset_plot$Characteristic <- as.factor(diff_dataset_plot$Characteristic)
diff_dataset_plot[4,"Characteristic"] <- "< 40 years old"
diff_dataset_plot[5,"Characteristic"] <- "40-50 years old"
diff_dataset_plot[6,"Characteristic"] <- "50-60 years old"
diff_dataset_plot[7,"Characteristic"] <- "60+ years old"

diff_dataset_plot[9,"Characteristic"] <- "Smoker"
diff_dataset_plot[10,"Characteristic"] <- "Non-smoker"

diff_dataset_plot[13,"Characteristic"] <- "No alcohol"
diff_dataset_plot[14,"Characteristic"] <- "Alcohol once in a while"
diff_dataset_plot[15,"Characteristic"] <- "< 4 glasses/week"
diff_dataset_plot[16,"Characteristic"] <- "4-7 glasses/week"
diff_dataset_plot[17,"Characteristic"] <- "> 7 glasses/week"
diff_dataset_plot[18,"Characteristic"] <- "Don't know alcohol"

diff_dataset_plot[20,"Characteristic"] <- "BMI < 18.5"
diff_dataset_plot[21,"Characteristic"] <- "BMI between 18.5 and 24.9"
diff_dataset_plot[22,"Characteristic"] <- "BMI between 25 and 29.9"
diff_dataset_plot[23,"Characteristic"] <- "BMI > 30"

diff_dataset_plot[38,"Characteristic"] <- "No previous diagnosis"
diff_dataset_plot[39,"Characteristic"] <- "Previous diagnosis"
diff_dataset_plot[40,"Characteristic"] <- "Don't know previous diagnosis"

diff_dataset_plot$Characteristic <- factor(diff_dataset_plot$Characteristic, levels = rev(diff_dataset_plot$Characteristic))

### select only the relevant rows
# So drop those with estimate==0
diff_dataset_plot_presentation <- subset(diff_dataset_plot, diff_dataset_plot$estimate !=0)

a <- ggplot(diff_dataset_plot_presentation, aes(x=estimate, y=factor(Characteristic), color=factor(Characteristic))) + #, levels = rev(characteristics)), color=factor(Characteristic, levels = rev(characteristics)))) +
  geom_point(shape=1, size=5)+
  geom_errorbar(aes(xmin = CI_low, xmax = CI_high), width = 0.7, linewidth=2) +
  #geom_point(size = 4) + 
  ggtitle("Difference in % and 95% CI")+
  labs(x="<-----    No follow-up        |||        Follow-up    ----->", y = "Characteristic") +
  theme_classic() +
  theme(legend.position = "none") +
  geom_vline(xintercept = 0, linetype = "longdash")+ 
  theme(plot.title = element_text(size = 16, face = "bold"),axis.text=element_text(size=14, face = "bold"),
        axis.title=element_text(size=15, face = "bold"))
ggsave("../Plots/forest_plot_pres.pdf",width = 15, height = 10)
ggsave("../Plots/forest_plot_pres.png",width = 15, height = 10)


