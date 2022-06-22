##classification by househole size

partw1h1 <- inner_join(sday, part, by="part_id") %>% filter(wave==1&hh_size=="1")

id=which(conw1$part_id%in%partw1h1$part_id)

conw1h1=conw1[c(id),]

conw1h1 %>% group_by(part_id) %>% summarise(ncontacts = n())

connum_activeh1 <- conw1h1 %>% group_by(part_id) %>% summarise(ncontacts = n())
table(connum_activeh1$ncontacts)
summary(connum_activeh1)

connum_allh1 <- left_join(partw1h1, conw1h1, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
table(connum_allh1$ncontacts)
summary(connum_allh1)
