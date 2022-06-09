library(here)

## If Belgium data have not already been downloaded, redownload.
if(!file.exists(here("data","be.rds")) || !file.exists(here("data","be_sday.csv"))){
  be_URLs <- capture.output(be_survey <- get_survey("https://doi.org/10.5281/zenodo.4035001"), type="message")
  sday_URL <-  be_URLs[min(grep("_sday.csv", be_URLs))] %>% sub("Downloading ", "", .)
  dir.create(here("data"), FALSE)
  download.file(sday_URL, here("data", "be_sday.csv"))
  saveRDS(be_survey, here("data","be.rds"))
}
