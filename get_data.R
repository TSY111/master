library(here)
library(tibble)
library(readr)

## If Belgium data have not already been downloaded, redownload.
if(!file.exists(here("data","be.rds")) || !file.exists(here("data","be_sday.csv"))){
  be_URLs <- capture.output(be_survey <- get_survey("https://doi.org/10.5281/zenodo.4035001"), type="message")
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
