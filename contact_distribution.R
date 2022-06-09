library(here)
source(here("get_data.R"))

sday <- be_sday
part <- be_part
con <- be_con

library(dplyr)
partw1 <- inner_join(sday, part, by="part_id") %>% filter(wave==1)
conw1 <- inner_join(sday, con, by="part_id") %>% filter(wave==1)

connum_active <- conw1 %>% group_by(part_id) %>% summarise(ncontacts = n())
table(connum_active$ncontacts)
summary(connum_active)

connum_all <- left_join(partw1, conw1, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
table(connum_all$ncontacts)
summary(connum_all)
