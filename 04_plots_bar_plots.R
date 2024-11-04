# Here, we make bar plots of complaints and previous diagnosis according to source

library(tidyverse)
library(ggthemes) # For scale_fill_colourblind()
library(data.table) # For setnames()


setwd("//vf-DataSafe/DataSafe$/Div0/ict/HebikReuma_deidentified_2216/Floor/Attrition_selection_article_no_1/Data")
load("dat_all_03.RData")


# Remake the different datasets
dat_all_T0 <- subset(dat_all, dat_all$dataset=="Non-responders")
dat_all_T0b <- subset(dat_all, dat_all$dataset=="Responders")


# Make the dataset that indicates if someone answered only T0 or also T0b
# Delete the entries that were appended (so duplicates of each individual from T0b)
dat_all_sub <- dat_all %>%
  arrange(desc(dataset)) %>%
  filter(!duplicated(Castor.Record.ID) )

table(dat_all_sub$dataset)

dat_all_T0b$Source_group2 <- droplevels(dat_all_T0b$Source_group2)

dat_all_T0b <- dat_all_T0b %>% 
  mutate(Source_group2 = factor(Source_group2, levels = c("Facebook", "Instagram", "Google", "ReumaNL",  "Other online", "GP", "Hospital", "Other")))



# Combine diagnosis at T0 and T0b
dat_all_sub <- dat_all_sub %>%
  mutate(Diag_T0_T0b = if_else(Diagnosis_T0=="Yes" | Diagnosis_T0b=="Yes", "Yes",
                               if_else(Diagnosis_T0=="Don't know" | Diagnosis_T0b=="Don't know", "Don't know", "No")))

dat_all_T0b <- dat_all_T0b %>%
  mutate(Diag_T0_T0b = if_else(Diagnosis_T0=="Yes" | Diagnosis_T0b=="Yes", "Yes",
                               if_else(Diagnosis_T0=="Don't know" | Diagnosis_T0b=="Don't know", "Don't know", "No")))

# Diagnosis T0b
ggplot(dat_all_T0b, aes(x=Source_group2,  fill = factor(Diag_T0_T0b, level=c("No","Yes","Don't know"))))+
  geom_bar(position="fill") +
  theme_minimal() +
  theme( axis.text.x = element_text(angle =-45, hjust = -0.01 )) +
  scale_fill_colorblind() +
  guides(fill=guide_legend(title="Previous diagnosis")) +
  ggtitle("Previous diagnosis by source") +
  xlab("Source") +
  ylab("Proportion")
ggsave("Diagnosis_T0_T0b.pdf")
ggsave("Diagnosis_T0_T0b.jpg")

# For the complaints-by-source-plot, we need to make the dataset of percentages ourselves
# Complaints
complaints <- c("Swelling","Pain","MorningStiffness","AllDayStiffness","Exhaustion","ReducedEndurance","NoneOfTheAbove")

# Make dataset of percentages
table_complaints_source <- dat_all_T0b %>%
  tbl_summary(include=all_of(complaints),  missing_text = "Missing", by = "Source_group2",statistic =all_categorical()~"{p}")
percent_complaints_source <- table_complaints_source$table_body[,c(5:13)]
percent_complaints_source <- setnames(percent_complaints_source, old=c("label","stat_1","stat_2","stat_3","stat_4","stat_5","stat_6","stat_7","stat_8"), new = c("Complaint","Facebook", "Instagram", "Google", "ReumaNL",  "Other online", "GP", "Hospital", "Other"))
percent_complaints_source <- pivot_longer(percent_complaints_source, cols = 2:9, names_to = "Source", values_to = "Percent")
percent_complaints_source <- percent_complaints_source %>% 
  mutate(Source = factor(Source, levels = c("Facebook", "Instagram", "Google", "ReumaNL",  "Other online", "GP", "Hospital", "Other")))

# Plot
ggplot(percent_complaints_source, aes(x = factor(Complaint, level = complaints), y = as.numeric(Percent), group=Source,fill = Source)) +
  geom_bar(stat = "identity", position = position_dodge2())+
  xlab("Complaint") +
  theme_minimal() +
  theme(text = element_text(size=20)) +
  scale_fill_colorblind() +
  scale_y_continuous(limits = c(0,90), name = "Percent",breaks=seq(0,90,10))
ggsave("Complaints.pdf", width = 20, height = 10)
ggsave("Complaints.jpg", width = 20, height = 10)

