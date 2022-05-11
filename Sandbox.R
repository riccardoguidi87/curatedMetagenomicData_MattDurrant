# Sandbox

library(readxl)
library(tidyverse)

# WirbelJ_2018_30936547_1.csv: clinical metadata with "subject_id" as key column

df <- read_csv("curated-data/YachidaS_2019_31171880_2.csv")

df1 <- df %>%
  mutate(subject_id = case_when(str_detect(sample, "Healthy")~ paste0("HanniganGD_2017_","H",str_extract(sample,"\\d+$")),
                                str_detect(sample, "Adenoma") ~ paste0("HanniganGD_2017_","A",str_extract(sample,"\\d+$")),
                                str_detect(sample, "Cancer") ~ paste0("HanniganGD_2017_","C",str_extract(sample,"\\d+$")))) %>%
  relocate(subject_id, .before = sample)

curatedmdHanniganGD_2017 <- curatedmd %>%
  filter(study_name == "HanniganGD_2017")

df2 <- right_join(curatedmdHanniganGD_2017,df1, by= "subject_id")

write_csv(df2,"curated-data/HanniganGD_2017_30459201_1.csv")
