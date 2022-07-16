getwd()
setwd("/Users/dogdogt/Downloads")
rm(list = ls())


library(here)
library(socialmixr)
library(tibble)
library(readr)
library(dplyr)
## If Belgium data have not already been downloaded, redownload.
if(!file.exists(here("data","be.rds")) || !file.exists(here("data","be_sday.csv"))){
  be_URLs <- capture.output(be_survey <- get_survey("10.5281/zenodo.4147585"), type="message") # Use Version 2 per Pietro Coletti.
  sday_URL <-  be_URLs[min(grep("_sday.csv", be_URLs))] %>% sub("Downloading ", "", .)
  dir.create(here("data"), FALSE)
  download.file(sday_URL, here("data", "be_sday.csv"))
  saveRDS(be_survey, here("data","be.rds"))
}

be_survey <- readRDS(here("data","be.rds"))
be_sday <- read_csv(here("data","be_sday.csv"))
be_part <- as_tibble(be_survey[[1]])
be_part <- be_part[!duplicated(be_part),]
be_con <- as_tibble(be_survey[[2]])
be_con <- be_con[!duplicated(be_con),]


library(here)

sday <- be_sday
part <- be_part
part$part_age_group <- cut(part$part_age,c(0, 18, seq(30,70,10),100),right=FALSE)
con <- be_con







##classification by age group
options(digits = 2)	
ageconnum <- inner_join(sday, part, by="part_id") %>%
  left_join(con, by="part_id") %>%
  group_by(wave, part_age_group, part_id) %>%
  filter(part_age_group!="[0,18)")%>%
  summarise(ncontacts = sum(!is.na(cont_id))) %>%
  summarise(mean = mean(ncontacts), Quartile.1 = quantile(ncontacts,0.25), Quartile.3 = quantile(ncontacts,0.75))
#install.packages("flextable")
library(flextable)
data <- flextable(ageconnum, col_keys = names(ageconnum))

data <- valign(data, valign = "center", part = "header")

save_as_docx("三线表结果" = data, path = "ageThree_Line_Table.docx")

##classification by househole size
hhconnum <- inner_join(sday, part,by="part_id") %>%
  left_join(con, by="part_id") %>%
  group_by(wave, hh_size, part_id) %>%
  filter(part_age_group!="[0,18)")%>%
  summarise(ncontacts = sum(!is.na(cont_id))) %>%
  summarise(mean = mean(ncontacts), Quartile.1 = quantile(ncontacts,0.25), Quartile.3 = quantile(ncontacts,0.75))

data <- flextable(hhconnum, col_keys = names(hhconnum))

data <- valign(data, valign = "center", part = "header")
save_as_docx("三线表结果" = data, path = "hhThree_Line_Table.docx")

##classification by gender
genderconnum <- inner_join(sday, part,by="part_id") %>%
  left_join(con, by="part_id") %>%
  group_by(wave,part_gender, part_id) %>%
  filter(part_age_group!="[0,18)")%>%
  summarise(ncontacts = sum(!is.na(cont_id))) %>%
  summarise(mean = mean(ncontacts), Quartile.1 = quantile(ncontacts,0.25), Quartile.3 = quantile(ncontacts,0.75))

data <- flextable(genderconnum, col_keys = names(genderconnum))

data <- valign(data, valign = "center", part = "header")
save_as_docx("三线表结果" = data, path = "genderThree_Line_Table.docx")

#overall
allconnum <- inner_join(sday, part,by="part_id") %>%
  left_join(con, by="part_id") %>%
  group_by(wave, part_id) %>%
  filter(part_age_group!="[0,18)")%>%
  summarise(ncontacts = sum(!is.na(cont_id))) %>%
  summarise(mean = mean(ncontacts), Quartile.1 = quantile(ncontacts,0.25), Quartile.3 = quantile(ncontacts,0.75))

data <- flextable(allconnum, col_keys = names(allconnum))

data <- valign(data, valign = "center", part = "header")
save_as_docx("三线表结果" = data, path = "allThree_Line_Table.docx")
