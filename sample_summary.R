df_list1 <- list(be_part,be_sday)
be_part_sday=Reduce(function(d1, d2) 
  merge(d1, d2, by = "part_id", all.x = TRUE, all.y = FALSE), 
  df_list1)
partw1=subset(be_part_sday,wave=="1")
partw1$age_group=cut(partw1$part_age,c(seq(0,70,10),100),right=F)

library("Hmisc")
describe(partw1)$part_gender
describe(partw1)$age_group

table(partw1$hh_type)/nrow(partw1)
table(partw1$part_occupation)/nrow(partw1)
