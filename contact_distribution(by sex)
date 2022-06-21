##classification by gender(Male)
partw1m <- inner_join(sday, part, by="part_id") %>% filter(wave==1&part_gender=="M")
id=which(conw1$part_id%in%partw1m$part_id)
conw1m=conw1[c(id),]
conw1m %>% group_by(part_id) %>% summarise(ncontacts = n())

connum_activem <- conw1m %>% group_by(part_id) %>% summarise(ncontacts = n())
table(connum_activem$ncontacts)
summary(connum_activem)

connum_allm <- left_join(partw1m, conw1m, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
table(connum_all$ncontacts)
summary(connum_all)
