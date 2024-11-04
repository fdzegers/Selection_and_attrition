# The variables we are interested in from the data set made in 01_cohort.R are prettified and put into categories.

# Initial packages
library(readr)
library(dplyr)
library(ggplot2)

setwd("//vf-DataSafe/DataSafe$/Div0/ict/HebikReuma_deidentified_2216/Floor/Attrition_selection_article_no_1/Data")
load("dat_all.Rdata")

##################################
#   Source of origin (T0b) 
#################################

#       "T0_1_Oorspong"

#Via de polikliniek reumatologie van het Medische Centrum Leeuwarden	1
#Via de polikliniek reumatologie van het Hagaziekenhuis	2
#Via de polikliniek reumatologie van het LUMC	3
#Via een andere polikliniek reumatologie namelijk: ……	4
#Via mijn huisarts	5
#Online gevonden op ReumaNederland	6
#Elders online gevonden nl…..	7
#Anders	77
#Reumazorg ZWN	8
#Poli reumatologie Maastricht UMC+	9
#Stadspoli Maastricht	99

source_ref <- data.frame(Source = c("Poli_Leeuwarden",
                                    "Poli_Haga",
                                    "Poli_LUMC",
                                    "Poli_Maastricht",
                                    "Poli_Anders",
                                    "Stadspoli_Maastricht",
                                    "Huisarts",
                                    "ReumaNL",
                                    "Reumazorg_ZWN",
                                    "Online_Elders",
                                    "Other"),
                         Code = c(1:3, 9, 4, 99, 5:6, 8, 7, 77))

# We are interested in more detailed where on the internet
dat_all$Source <- source_ref$Source[match(dat_all$T0_1_Oorspong, source_ref$Code)]
dat_all[grep("face|fb|fac", dat_all$T0_1_OorspongElders, ignore.case = T), "Source"] <- "Facebook"
dat_all[grep("goo|zoek|zocht", dat_all$T0_1_OorspongElders, ignore.case = T), "Source"] <- "Google"
dat_all[grep("insta", dat_all$T0_1_OorspongElders, ignore.case = T), "Source"] <- "Instagram"
dat_all[grep("thuisarts", dat_all$T0_1_OorspongElders, ignore.case = T), "Source"] <- "Thuisarts"
dat_all[grep("inkedin", dat_all$T0_1_OorspongElders, ignore.case = T), "Source"] <- "LinkedIn"

dat_all$Source <- factor(dat_all$Source, levels = c("Poli_Leeuwarden",
                                                    "Poli_Haga",
                                                    "Poli_LUMC",
                                                    "Poli_Maastricht",
                                                    "Poli_Anders",
                                                    "Stadspoli_Maastricht",
                                                    "Huisarts",
                                                    "ReumaNL",
                                                    "Reumazorg_ZWN",
                                                    "Facebook",
                                                    "Google",
                                                    "Instagram",
                                                    "Thuisarts",
                                                    "LinkedIn",
                                                    "Online_Elders",
                                                    "Other"))
table(subset(dat_all$Source, dat_all$Survey.Progress==100), exclude = F)

dat_all <- dat_all %>% mutate(Source_group = if_else( Source== "Reumazorg_ZWN" | Source=="Poli_Leeuwarden" | Source=="Poli_Haga" | Source=="Poli_LUMC" | Source=="Poli_Maastricht" | Source== "Poli_Anders" | Source=="Stadspoli_Maastricht", "Hospital",
                                                     if_else(Source== "Huisarts", "GP",
                                                             if_else(Source== "ReumaNL", "ReumaNL",
                                                                     if_else( Source== "Facebook"| Source== "Google"| Source=="Instagram"| Source== "Thuisarts"| Source== "LinkedIn" | Source== "Online_Elders", "Online", "Other")))) )

# Indicator for the online source as we ant to examine those separately
dat_all <- dat_all %>% mutate(Source_online = if_else(Source_group == "Online",1,0))
dat_all <- dat_all %>% mutate(Source_group2 = if_else( Source== "Reumazorg_ZWN" | Source=="Poli_Leeuwarden" | Source=="Poli_Haga" | Source=="Poli_LUMC" | Source=="Poli_Maastricht" | Source== "Poli_Anders" | Source=="Stadspoli_Maastricht", "Hospital",
                                                               if_else(Source== "Huisarts", "GP",
                                                                       if_else(Source=="Thuisarts" | Source=="LinkedIn" | Source== "Online_Elders", "Other online",
                                                                          if_else(Source== "ReumaNL", "ReumaNL", Source)))))
#################### 
# T0 Eerdere diagnose
##################
#           Ja=1, nee=2

dat_all <- dat_all %>%
  mutate(Diagnosis_T0=if_else(Diagnose1.Nee..ik.heb.geen.diagnose.van.een.van.deze.ziekten.gehad....OjAp9X.==1,0,
                              if_else(Diagnose1.Weet.ik.niet..OXPF1G.==1,2,1)))
table(dat_all$Diagnosis_T0, exclude = F)
#  0 = Nee, 1 = Ja, 2 = weet niet
dat_all$Diagnosis_T0 <- factor(dat_all$Diagnosis_T0, levels = c(0,1,2), labels = c("No", "Yes", "Don't know"))

#################### 
# T0b Eerdere diagnose
##################
#           Ja=1, nee=2

dat_all$Diagnosis_T0b <- ifelse(dat_all$T0_8_DiagnoseJN==2, 0, 
                                ifelse(dat_all$T0_8_Diagnose.Weet.ik.niet==1, 2, 1) )
dat_all$Diagnosis_T0b <- factor(dat_all$Diagnosis_T0b, levels = c(0,1,2), labels = c("No", "Yes", "Don't know"))
table(dat_all$Diagnosis_T0b, exclude = F)
#   Dus 0 = Nee, 1 = Ja, 2 = weet niet
#   Ze zijn NA als Survey.Progress 0 is


##############################
#   Familie diagnosis
##############################

dat_all$Family_diag_Yes_No <- ifelse(dat_all$DiagFam.Nee..OpyD.y==1, 0, 
                                    ifelse(dat_all$DiagFam.Weet.ik.niet..Ou2iC..==1, 2, 1) )
dat_all$Family_diag_Yes_No <- factor(dat_all$Family_diag_Yes_No, levels = c(0,1,2), labels = c("No", "Yes", "Don't know"))
table(dat_all$DiagFam.Nee..OpyD.y., exclude = F)
table(dat_all$DiagFam.Weet.ik.niet..Ou2iC.., exclude = F)
table(dat_all$Family_diag_Yes_No, exclude = F)

# Which diagnosis

dat_all <- dat_all %>% rename(
  Family_diag_RA=DiagFam.reumatoïde.artritis..OXJMs5.,                     
  Family_diag_Sjogren = DiagFam.morbus.Sjögren..OmsliI.,                           
  Family_diag_SLE=DiagFam.SLE..OmKZYp.,    
  Family_diag_Myositis=DiagFam.Myositis..OeIjCL.,                    
  Family_diag_sys_sclerose=DiagFam.systemische.sclerose..OJTrZ9.,         
  Family_diag_SpA=DiagFam.spondylartropathie..vroeger..m.Bechterew...OxZrKk.,
  Family_diag_Artrose=DiagFam.artrose..OKaFEM.,
  Family_diag_Jicht=DiagFam.jicht..Ob126I.,     
  Family_diag_Spierreuma=DiagFam.spierreuma..PMR..polymyalgia.reumatica...OteGAa.,  
  Family_diag_APs=DiagFam.artritis.psoriatica..OOEgLa.,
  Family_diag_Fibromyalgie=DiagFam.Fibromyalgia..OPV3j..,
)


#######################################
#   Aantal gezwollen gewrichten
######################################
# Sum of dat_all$ZwelWaar.XX

dat_all$Number_zwel <- rowSums(dat_all[, c("ZwelWaar.Rechtervoet..O9bBW5.", "ZwelWaar.Linkervoet..O0AyC7.","ZwelWaar.Rechterhand..O5jzeV.", "ZwelWaar.Linkerhand..OXDH7E.", "ZwelWaar.Rechterbeen..OFVAyw.", "ZwelWaar.Linkerbeen..OWA9N5.", "ZwelWaar.Rechterarm..OvWSvk.", "ZwelWaar.Linkerarm..OfZqKZ.", "ZwelWaar.Hoofd.nek..OkGPzu.", "ZwelWaar.Borst..OikyrC.", "ZwelWaar.Maag..OBMKsj.","ZwelWaar.Rug..OyfyQk.")])

dat_all$Number_zwel <- as.factor(dat_all$Number_zwel)

table(dat_all$Number_zwel,exclude = F)


#######################################
#   Aantal pijnlijke gewrichten
#######################################
# Sum of dat_all$PijnWaar.XX

dat_all$Number_pain <- rowSums(dat_all[, c("PijnWaar.Rechtervoet..O.fjfh.","PijnWaar.Linkervoet..Oo9SvN.", "PijnWaar.Rechterhand..OJCntd.", "PijnWaar.Linkerhand..OEEER8.", "PijnWaar.Rechterbeen.en.heup..Ol7ZP0.", "PijnWaar.Linkerbeen.en.heup..OJK8sg.","PijnWaar.Rechterarm..O.PQkN.", "PijnWaar.Linkerarm..O7XoT2.", "PijnWaar.Hoofd.nek..OQJKb7.", "PijnWaar.Borst..OOlAA9.", "PijnWaar.Maag..OfiZuL.", "PijnWaar.Rug..O4uaaZ.")])

dat_all$Number_pain <- as.factor(dat_all$Number_pain)

table(dat_all$Number_pain, exclude = F)

#######################################
#   BMI
#######################################
# Calculate BMI range
# The middle posibility of a persons BMI (mid of length- and height interval)

table(dat_all$Lengte, exclude = F)
dat_all$Length_mid <- ifelse(dat_all$Lengte==1,145+2.5,
                               ifelse(dat_all$Lengte==2,150+2.5,
                                      ifelse(dat_all$Lengte==3,155+2.5,
                                             ifelse(dat_all$Lengte==4,160+2.5,
                                                    ifelse(dat_all$Lengte==5,165+2.5,
                                                           ifelse(dat_all$Lengte==6,170+2.5,
                                                                  ifelse(dat_all$Lengte==7,175+2.5,
                                                                         ifelse(dat_all$Lengte==8,180+2.5,
                                                                                ifelse(dat_all$Lengte==9,185+2.5,
                                                                                       ifelse(dat_all$Lengte==10,190+2.5,
                                                                                              ifelse(dat_all$Lengte==11,195+2.5,
                                                                                                     ifelse(dat_all$Lengte==12,200+2.5,NA))))))))))))
table(dat_all$Length_mid, exclude = F)

dat_all$Weight_mid <- ifelse(dat_all$Gewicht==1,50+2.5,
                                 ifelse(dat_all$Gewicht==2,55+2.5,
                                        ifelse(dat_all$Gewicht==3,60+2.5,
                                               ifelse(dat_all$Gewicht==4,65+2.5,
                                                      ifelse(dat_all$Gewicht==5,70+2.5,
                                                             ifelse(dat_all$Gewicht==6,75+2.5,
                                                                    ifelse(dat_all$Gewicht==7,80+2.5,
                                                                           ifelse(dat_all$Gewicht==8,85+2.5,
                                                                                  ifelse(dat_all$Gewicht==9,90+2.5,
                                                                                         ifelse(dat_all$Gewicht==10,100+2.5,
                                                                                                ifelse(dat_all$Gewicht==11,110+2.5,
                                                                                                       ifelse(dat_all$Gewicht==12,115+2.5,NA))))))))))))
table(dat_all$Gewicht, exclude = F)
table(dat_all$Weight_mid, exclude = F)


dat_all$BMI <- (dat_all$Weight_mid)/((dat_all$Length_mid*0.01)^2) # 0.01 to make centimeters to meters
ggplot(dat_all, aes(x=BMI))+
  geom_histogram(bins = 50)
ggsave("Hist_BMI.pdf")

# Put into four categories
dat_all$BMI_4cat <- ifelse(dat_all$BMI<18.5, "<18.5",
                          ifelse(dat_all$BMI>= 18.5 & dat_all$BMI<25, "18.5-24.9",
                                 ifelse(dat_all$BMI >= 25 & dat_all$BMI<30, "25-29.9", "30+")))
table(dat_all$BMI_4cat, exclude = F)


# Group age 
# <40, 40-50, 50-60, 60+
dat_all$Agegroup <- ifelse(dat_all$Leeft==1 | dat_all$Leeft== 2| dat_all$Leeft== 3, "< 40",
                           ifelse(dat_all$Leeft==4,"40-50",
                                  ifelse(dat_all$Leeft==5,"50-60","60+")))

# Rename complaints questions
dat_all <- dat_all %>% rename(
  Swelling = EersteVraag.Zwelling..O1XRfS.,
  Pain = EersteVraag..Zeurende..Pijn..OgeZnz.,
  MorningStiffness = EersteVraag.Ochtendstijfheid..OkaXzN.,
  AllDayStiffness = EersteVraag.Stijfheid.gedurende.de.gehele.dag..On6qYM.,
  Exhaustion = EersteVraag.Vermoeidheid.of.uitputting..OBNmV2.,
  ReducedEndurance = EersteVraag.Verminderd.uithoudingsvermogen..ObfocR.,
  NoneOfTheAbove = EersteVraag.Geen.van.bovenstaande..OiC.ue.,
)



# To include missing values in the calculation of percentages, make them explicit
table(dat_all$Source_group2, exclude = F)


dat_all <- dat_all %>% 
  mutate(Source_group = replace(Source_group, is.na(Source_group),"Missing")) %>%
  mutate(Source_group = factor(Source_group, levels = c("Hospital", "GP", "ReumaNL", "Online", "Other", "Missing")))

dat_all <- dat_all %>% 
  mutate(Source_group2 = replace(Source_group2, is.na(Source_group2),"Missing")) %>%
  mutate(Source_group2 = factor(Source_group2, levels = c("Hospital", "GP", "ReumaNL", "Facebook","Google", "Instagram", "Other online", "Other", "Missing")))

dat_all$Diagnosis_T0b <- as.character(dat_all$Diagnosis_T0b)

dat_all <- dat_all %>% 
  mutate(Diagnosis_T0b = replace(Diagnosis_T0b, is.na(Diagnosis_T0b),"Missing")) %>%
  mutate(Diagnosis_T0b = factor(Diagnosis_T0b, levels = c("Yes", "No", "Don't know", "Missing")))

table(dat_all$Diagnosis_T0b, exclude = F)

dat_all <- dat_all %>%
  mutate(Smoking = if_else(Roken==1,"Yes",
                          if_else(Roken==2, "No", "Previous smoker"))) %>%
  mutate(Smoking = factor(Smoking, levels = c("Yes", "No", "Previous smoker")))

dat_all$Sex <- ifelse(dat_all$Geslacht==1,"Female","Male")
table(dat_all$Sex, dat_all$Geslacht)

dat_all <- dat_all %>%
  rename(Alcohol_original = Alcohol) %>%
  mutate(Alcohol = if_else(Alcohol_original ==1, "Once in a while",
                                                if_else(Alcohol_original==2,"< 4 glasses/week",
                                                        if_else(Alcohol_original==3,"4 - 7 glasses/week",
                                                                if_else(Alcohol_original==4,"> 7 glasses/week",
                                                                        if_else(Alcohol_original==5,"Never","Don’t know")))))) %>%
  mutate(Alcohol = factor(Alcohol, levels = c("Never", "Once in a while", "< 4 glasses/week", "4 - 7 glasses/week", "> 7 glasses/week", "Don’t know")))
                                             

# Total score
# This will not be used in the article
totalscore = read.csv("totalscore13072023.csv", sep = ";")

totalscore <- totalscore %>% 
  select(c("RA","Inflammatorisk_ryggsjukdom_.Axial_Spondylartrit.","SLE","Artros","Systemisk_skleros_new","Myosit_new","Sjögrens_syndrom_new","Castor.Record.ID"))

totalscore <- totalscore %>%
  rename(DiseasescoreRA = RA) %>%
  rename(DiseasescoreAxSpA = Inflammatorisk_ryggsjukdom_.Axial_Spondylartrit.) %>%
  rename(DiseasescoreSLE = SLE) %>%
  rename(DiseasescoreArt = Artros) %>%
  rename(DiseasescoreSysScl = Systemisk_skleros_new) %>%
  rename(DiseasescoreMyo = Myosit_new) %>%
  rename(DiseasescoreSjo = Sjögrens_syndrom_new)

# Make the total score, which is the sum of all these except artrosis

totalscore$DiseasescoreTotal <- rowSums(totalscore[,c("DiseasescoreRA","DiseasescoreAxSpA", "DiseasescoreSLE","DiseasescoreSysScl", "DiseasescoreMyo", "DiseasescoreSjo")])

dat_all <- merge(dat_all,totalscore, by="Castor.Record.ID")

rm(totalscore, source_ref)

# Total number of complaints
dat_all$Sum_of_complaints <- rowSums(dat_all[,c("Swelling", "Pain", "MorningStiffness", "AllDayStiffness", "Exhaustion", "ReducedEndurance")])
  
dat_all$Sum_of_complaints <- as.numeric(dat_all$Sum_of_complaints)


save(dat_all, file = "dat_all_03.Rdata")
