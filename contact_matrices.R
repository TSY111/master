rm(list = ls())

library(socialmixr)
library(here)
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





data=be_survey


part=as.data.frame(data[1])
con=as.data.frame(data[2])
getwd()
setwd("/Users/dogdogt/Desktop/article/be")
data.sday2 <- read.csv("CoMix_be_sday.csv",head=TRUE)
sday=data.sday2

names(part)=c("hh_id",      
              "part_id",                                  
              "part_age" ,                                
              "part_gender" ,                             
              "part_occupation",                          
              "multiple_contacts_child_work" ,            
              "multiple_contacts_child_school" ,          
              "multiple_contacts_child_other" ,           
              "multiple_contacts_adult_work",             
              "multiple_contacts_adult_school",           
              "multiple_contacts_adult_other" ,           
              "multiple_contacts_older_adult_work" ,      
              "multiple_contacts_older_adult_school",    
              "multiple_contacts_older_adult_other" ,     
              "multiple_contacts_child_work_phys" ,       
              "multiple_contacts_child_school_phys" ,     
              "multiple_contacts_child_other_phys"  ,     
              "multiple_contacts_adult_work_phys" ,       
              "multiple_contacts_adult_school_phys" ,     
              "multiple_contacts_adult_other_phys" ,      
              "multiple_contacts_older_adult_work_phys" , 
              "multiple_contacts_older_adult_school_phys",
              "multiple_contacts_older_adult_other_phys" ,
              "part_education" ,                          
              "panel_id"    ,                             
              "country"  ,                                
              "hh_size",
              "hh_type")                              
names(con)=c("cont_id",
             "part_id",                  
             "cnt_age_exact",  
             "cnt_age_est_min",    
             "cnt_age_est_max",  
             "cnt_gender",         
             "frequency_multi",  
             "phys_contact",        
             "cnt_home",    
             "cnt_work",         
             "cnt_school",   
             "cnt_transport",      
             "cnt_leisure",   
             "cnt_otherplace",     
             "duration_multi",   
             "cnt_outside_other",  
             "cnt_other_house", 
             "cnt_worship",      
             "cnt_supermarket", 
             "cnt_shop",        
             "cnt_public_market", 
             "individually_reported",
             "cnt_gender_all", 
             "frequency_multi_all")

df_list1 <- list(part,sday)
df1=Reduce(function(d1, d2) 
  merge(d1, d2, by = "part_id", all.x = TRUE, all.y = FALSE), 
  df_list1)
partw1=subset(df1,wave=="1")
partw1=partw1[!duplicated(partw1),]

df_list2 <- list(con,sday)
df2=Reduce(function(d1, d2) 
  merge(d1, d2, by = "part_id", all.x = TRUE, all.y = FALSE), 
  df_list2)
conw1=subset(df2,wave=="1")

sur1=survey(partw1, conw1, reference = NULL)

contact_matrix(sur1, age.limits = c(0, 5, 18,30,40,50,60,70))
mat=contact_matrix(sur1, age.limits = c(0, 5, 18,30,40,50,60,70))

mr <- Reduce("+", lapply(mat$matrices, function(x) {x$matrix})) / length(mat$matrices)
mr=mat$matrix

library("reshape2")
library("ggplot2")
df <- melt(mr, varnames = c("par", "con"), value.name = "contacts")
ggplot(df, aes(x = par, y = con, fill = contacts),method="number")+theme(legend.position = "bottom") + geom_tile()
#wave1
#dont konw how to Label data on a graph like paper

