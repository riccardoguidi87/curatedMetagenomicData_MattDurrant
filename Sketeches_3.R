# Sketches 3
library(tidyverse)

smdf <- read_tsv("raw-data/Baruch_2021/RNA-Seq/GSE162436_series_matrix_table.csv", skip = 32, name_repair = "universal")

names(smdf)[1] <- sub(".","",names(smdf)[1])
 
smdf_v2 <- smdf %>%
  mutate(Sample_title = str_replace(Sample_title,"\\!", ""))

# gene count

gc_gut <- read_excel("raw-data/Baruch_2021/RNA-Seq/GSE162436_stranded_rev_CPM2_tumor_TMM_counts.xlsx")
names(gc_gut)[1] <- "ensembl_gene_id"


## NEEDS INTERNET ACCESS !!
library(biomaRt)
head(listMarts(), 10)                      ## list the marts
head(listDatasets(useMart("ensembl")), 3) ## mart datasets
ensembl <-                                ## fully specified mart
  useMart("ensembl", dataset = "hsapiens_gene_ensembl")

head(listFilters(ensembl), 5)             ## filters
listFilters(ensembl)
myFilter <- "chromosome_name"
substr(filterOptions(myFilter, ensembl), 1, 50) ## return values
myValues <- c("21", "22")
head(listAttributes(ensembl), 3)          ## attributes
myAttributes <- c("ensembl_gene_id","chromosome_name","external_gene_name","entrezgene_accession")

## assemble and query the mart
res <- getBM(attributes =  myAttributes, filters =  myFilter,
             values =  myValues, mart = ensembl)

## assemble and query the mart
res <- getBM(attributes =  myAttributes, mart = ensembl)

res1 <- res %>%
  mutate(external_gene_name = str_remove(external_gene_name,'"')) %>%
  dplyr::select(ensembl_gene_id,external_gene_name)

write_csv(res1,"curated-data/bioMartEnsemblHsapiens.csv")

df <- left_join(gc_gut,res1, by = "ensembl_gene_id")

df %>%
  mutate(geneID = external_gene_name, .before =  ensembl_gene_id) %>%
  mutate(across(c(3:20),log10)) %>%
  select(-last_col()) %>%
  pivot_longer(cols = !c("geneID","ensembl_gene_id")) %>%
  filter(value >= 1) %>%
  mutate(groupFacet = case_when(str_detect(name,"R1_") ~ "R1",
                            str_detect(name,"R3_") ~ "R3",
                            str_detect(name,"R4_") ~ "R4",
                            str_detect(name,"R5_") ~ "R5",
                            str_detect(name,"R6_") ~ "R6",
                            str_detect(name,"R7_") ~ "R7",
                            str_detect(name,"R8_") ~ "R8",
                            str_detect(name,"R9_") ~ "R9",
                            str_detect(name,"R10_") ~ "R10")) %>%
  mutate(timepointFacet = case_when(str_detect(name,"pre") ~ "pre",
                                str_detect(name,"post") ~ "post"))

p1 <- df %>%
  mutate(geneID = external_gene_name, .before =  ensembl_gene_id) %>%
  mutate(across(c(3:20),log10)) %>%
  select(-last_col()) %>%
  ggplot(aes(x = R1_tumor_pre, y = R1_tumor_post)) + 
  geom_point(alpha = 0.1) +
  theme_bw()

p2 <- df %>%
  mutate(geneID = external_gene_name, .before =  ensembl_gene_id) %>%
  mutate(across(c(3:20),log10)) %>%
  select(-last_col()) %>%
  ggplot(aes(x = R3_tumor_pre, y = R3_tumor_post)) + 
  geom_point(alpha = 0.1) +
  theme_bw()

p3 <- df %>%
  mutate(geneID = external_gene_name, .before =  ensembl_gene_id) %>%
  mutate(across(c(3:20),log10)) %>%
  select(-last_col()) %>%
  ggplot(aes(x = R4_tumor_pre, y = R4_tumor_post)) + 
  geom_point(alpha = 0.1) +
  theme_bw()

p4 <- df %>%
  mutate(geneID = external_gene_name, .before =  ensembl_gene_id) %>%
  mutate(across(c(3:20),log10)) %>%
  select(-last_col()) %>%
  ggplot(aes(x = R5_tumor_pre, y = R5_tumor_post)) + 
  geom_point(alpha = 0.1) +
  theme_bw()

p4
p3
p2
