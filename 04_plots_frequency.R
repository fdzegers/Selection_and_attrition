# Here, the plot of the daily inclusion rate is made, both combined and stratified

library(tidyverse)

library(ggplot2)
library(lubridate) # For dates
library(zoo) # For the function rollmean()

setwd("//vf-DataSafe/DataSafe$/Div0/ict/HebikReuma_deidentified_2216/Floor/Attrition_selection_article_no_1/Data")
load("dat_all_03.RData")


# Remake the different datasets
dat_all_T0 <- subset(dat_all, dat_all$dataset=="Non-responders")
dat_all_T0b <- subset(dat_all, dat_all$dataset=="Responders")

# A dataset that indicates if someone answered only T0 or also T0+T0b
# Delete the entries that were appended (so duplicates of each individual)
dat_all_sub <- dat_all %>%
  arrange(desc(dataset)) %>%
  filter(!duplicated(Castor.Record.ID) )

table(dat_all_sub$dataset)

# Check if they all have a creation date
which(is.na(dat_all_sub$Participant.Creation.Date_adj))

# Make new dataset counting the number of participants each day, using dat_all_sub
freq_date_all_participants <- data.frame(table(dat_all_sub$Participant.Creation.Date_adj))
# Check if all participants are represented
sum(freq_date_all_participants$Freq) # Yes, all are here


# Inclusion per day - frequency plot
# With a smooth week average on top

freq_date_all_participants$Var1 <- as.Date(freq_date_all_participants$Var1)

ggplot(freq_date_all_participants, aes(x=Var1, y=Freq, group=1))+
  geom_line(color = "cadetblue", linewidth = 1) +
  geom_line(aes(y = rollmean(Freq, 7, na.pad = TRUE, align = "right")), linewidth = 1) +
  theme( axis.text.x = element_text(angle =-45, hjust = -0.01 )) +
  ggtitle("Inclusion frequency with 7-day rolling mean") +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  xlab("Date") + ylab("Frequency")
ggsave("../Plots/Inclusion_freq.pdf",width = 10, height = 5)
ggsave("../Plots/Inclusion_freq.jpg",width = 10, height = 5)
  # The NA's are just the first 6 days of the rolling mean, of course they are missing


#### 
## Frequency plot stratified on the indicated source 
####

dat_all_T0b$Source_group2 <- droplevels(dat_all_T0b$Source_group2)

dat_all_T0b <- dat_all_T0b %>% 
  mutate(Source_group2 = factor(Source_group2, levels = c("Facebook", "Instagram", "Google", "ReumaNL",  "Other online", "GP", "Hospital", "Other")))

# As before, make new dataset counting the number of participants each day for each Source_group2, using dat_all_T0b
freq_date_all_participants_T0b <- dat_all_T0b %>%
  group_by(Source_group2) %>%
  count(Participant.Creation.Date_adj)
sum(freq_date_all_participants_T0b$n) # Yes, all are here

freq_date_all_participants_T0b$Participant.Creation.Date_adj <- as.Date(freq_date_all_participants_T0b$Participant.Creation.Date_adj)

ggplot(freq_date_all_participants_T0b, aes(x=Participant.Creation.Date_adj, y=n, group=1))+
  facet_wrap(~ Source_group2, scales = "free_y", nrow = 3) +
  geom_line(color = "cadetblue", linewidth = 1) +
  geom_line(aes(y = rollmean(n, 7, na.pad = TRUE, align = "right")), linewidth = 1) +
  theme( axis.text.x = element_text(angle =-45, hjust = -0.01 )) +
  ggtitle("Inclusion frequency with 7-day rolling mean") +
  scale_x_date(date_breaks = "2 months" , date_labels = "%b-%y") +
  xlab("Date") + ylab("Frequency")
ggsave("../Plots/Inclusion_freq_source.pdf",width = 10, height = 5)
ggsave("../Plots/Inclusion_freq_source.jpg",width = 10, height = 5)
