##classification by age group

partw1age1 <- inner_join(sday, part, by="part_id") %>% filter(wave==1&age_group=="[20,30)")

id=which(conw1$part_id%in%partw1age1$part_id)

conw1age1=conw1[c(id),]

conw1age1 %>% group_by(part_id) %>% summarise(ncontacts = n())

connum_activeage1 <- conw1age1 %>% group_by(part_id) %>% summarise(ncontacts = n())
table(connum_activeage1$ncontacts)
summary(connum_activeage1)

connum_allage1 <- left_join(partw1age1, conw1age1, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
table(connum_allage1$ncontacts)
summary(connum_allage1)
