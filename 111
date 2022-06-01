rm(list = ls())
#clear
install.packages('socialmixr')
install.packages('Hmisc')

install.packages('pastecs')

library("socialmixr")
library("Hmisc")

library("pastecs")

be_survey <- get_survey("https://doi.org/10.5281/zenodo.4035001")
saveRDS(be_survey, "be.rds")
be_survey <- readRDS("be.rds")
#download survey online,but don't know why it doesn't contain "wave"

survey_countries(be_survey)

?contact_matrix

#import data
#try to make table1(page2)
getwd()
setwd("/Users/dogdogt/Desktop/article/be")

data.con.c <- read.csv("CoMix_be_contact_common.csv",head=TRUE)
data.con.e <- read.csv("CoMix_be_contact_extra.csv",head=TRUE)
data.hh.c <- read.csv("CoMix_be_hh_common.csv",head=TRUE)
data.hh.e <- read.csv("CoMix_be_hh_extra.csv",head=TRUE)
data.par.c <- read.csv("CoMix_be_participant_common.csv",head=TRUE)
data.par.e <- read.csv("CoMix_be_participant_extra.csv",head=TRUE)
data.sday2 <- read.csv("CoMix_be_sday.csv",head=TRUE)

t(head(data.con.c))
t(head(data.con.e))
t(head(data.hh.c))
t(head(data.hh.e))
t(head(data.par.c))
t(head(data.par.e))
t(head(data.sday2))
attach(data.con.c)
attach(data.con.e)
attach(data.hh.c)
attach(data.hh.e)
attach(data.par.c)
attach(data.par.e)
attach(data.sday2)

df_list1 <- list(data.par.c,data.sday2)
df1=Reduce(function(d1, d2) 
  merge(d1, d2, by = "part_id", all.x = TRUE, all.y = FALSE), 
  df_list1)
#combine par.c and sday



dwave1=subset(df1,wave=="1")


library("socialmixr")
sur1=survey(dwave1, data.con.c, reference = NULL)
# creat survey
contact_matrix(sur1, age.limits = c(0, 10, 20,30,40,50,60,70))


library("Hmisc")
describe(dwave1)
describe(dwave1)$part_gender
#table 1

library("pastecs")
stat.desc(dwave1$part_age)
quantile(dwave1$part_age)
#age median,IQR


#2 household category
df_list2 <- list(dwave1,data.hh.c,data.hh.e)
df2=Reduce(function(d1, d2) 
  merge(d1, d2, by = "hh_id", all.x = TRUE, all.y = FALSE), 
  df_list2)
#combine par.c and sday and hh.c in wave1
#due to hh_id, it has 4290 varibles

table(df2$hh_type)
#the number of it is right, I still try to correct it

#3par occupation
df_list3 <- list(dwave1,data.par.e)
df3=Reduce(function(d1, d2) 
  merge(d1, d2, by = "part_id", all.x = TRUE, all.y = FALSE), 
  df_list3)

table(df3$part_occupation)
#the NA is 471, but in the artcile no missing data
#try to use prop.table,but occupation is type of character
#try to use ddply and grep, failed

#question:how to calculate the frequency of character data
#try to use part_id and cont_id to calculate the contacted number of participant
