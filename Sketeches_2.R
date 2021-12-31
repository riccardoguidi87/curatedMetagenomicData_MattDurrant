# new Sketcheks

# I confirmed there is lots of data in the published dataset
# curate the table ready for merge

library(tidyverse)
library(readxl)

# Metabolomic Table
WirbelJ_2018_SupplDataMOESM3sh3_1 <- read_excel("raw-data/WirbelJ_2018_PMID30936547/41591_2019_406_MOESM3_ESM.xlsx",sheet = 3, skip = 18)

WirbelJ_2018_SupplDataMOESM3sh3_2 <- YachidaS_2019_SupplData13_1 %>%
  pivot_longer(cols = !c("metabolite"), names_to = "Subject_ID") %>%
  select(Subject_ID, everything())

# I have now a well formatted table

YachidaS_2019_SupplData13_2

# MERGING

YachidaS_2019_SupplData1_1 <- bind_rows(YachidaS_2019_SupplData2_2,YachidaS_2019_SupplData3_2)
YachidaS_2019_SupplData1_2 <- inner_join(YachidaS_2019_SupplData1_1,YachidaS_2019_SupplData13_2,by="Subject_ID")

# this table now has metabolomic and clinical data
YachidaS_2019_SupplData1_2

# some visual

YachidaS_2019_SupplData1_2$metabolite

# Distribution of matabolites
relevantMetab <- YachidaS_2019_SupplData1_2 %>%
  select(metabolite,value) %>%
  group_by(metabolite) %>%
  summarise(mean = mean(value)) %>%
  filter(mean >= 10) %>% 
  pivot_wider(names_from = metabolite, values_from = mean) %>% names()
  
YachidaS_2019_SupplData1_2 %>%
  filter(metabolite %in% relevantMetab) %>%
  mutate(valueLOG10 = log10(value)) %>%
  
  ggplot(aes(x=metabolite,y=valueLOG10)) +
  geom_boxplot(outlier.shape = NA)


# Does any of these metabolites correlate with tumour stage?
YachidaS_2019_SupplData1_2$Stage %>% unique()

YachidaS_2019_SupplData1_2 %>%
  filter(metabolite %in% relevantMetab) %>%
  mutate(valueLOG10 = log10(value)) %>%
  filter(metabolite == "C06423_Octanoate") %>%

  ggplot(aes(x=Stage,y=valueLOG10)) +
  geom_violin() +
  geom_jitter(size = 1)

###

GuptaA_2019_SupplData1_1 <- read_excel("raw-data/GuptaA_2019_PMID31719139/mSystems.00438-19-st001.xlsx", skip = 1)
n<-dim(GuptaA_2019_SupplData1_1)[1]
GuptaA_2019_SupplData1_2 <- GuptaA_2019_SupplData1_1[1:(n-2),] #remove last two empty rows

# Adjust data type
GuptaA_2019_SupplData1_3 <- GuptaA_2019_SupplData1_2 %>%
  mutate(across(names(GuptaA_2019_SupplData1_2[,c(2,4,8:13)]), as.factor))

# I have now a well formatted table

GuptaA_2019_SupplData1_3

# Visual

GuptaA_2019_SupplData1_3 %>% 
  
  ggplot(aes(x=AGE,y=BMI)) +
  geom_point()+
  geom_smooth(method = lm)



#####

curatedmd_CRCnames

var <- names(curatedmd)[-c(1:3)]

toStringUnique <- function(vector) {
  toString(unique(vector))
}

varSummaryTbl <- curatedmd %>%
  filter(study_name %in% curatedmd_CRCnames) %>%
  group_by(study_name) %>%
  summarise(across(.col = var, .fns = toStringUnique)) %>%
  ungroup() %>% 
  select(!c(12:22)) %>% #remove columsn that are not clinical annotation values
  
  pivot_longer(cols = !c(study_name),names_to = "Variables") %>%
  pivot_wider(names_from = study_name)


  










