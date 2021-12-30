# new Sketcheks

# I confirmed there is lots of data in the published dataset
# curate the table ready for merge

# Metabolomic Table
YachidaS_2019_SupplData13_1 <- read_excel("raw-data/YachidaS_2019_PMID31171880/41591_2019_458_MOESM3_ESM.xlsx",sheet = 18,skip = 3)
names(YachidaS_2019_SupplData13_1)[1] <- "metabolite" 

YachidaS_2019_SupplData13_2 <- YachidaS_2019_SupplData13_1 %>%
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



