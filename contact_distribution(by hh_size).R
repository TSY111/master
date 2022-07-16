##classification by househole size
hhconnum <- inner_join(sday, part,by="part_id") %>%
  left_join(con, by="part_id") %>%
  left_join(at_hh.c,by="hh_id")%>%
  group_by(wave, hh_size, part_id) %>%
  filter(part_age!="Under 18")%>%
  summarise(ncontacts = sum(!is.na(cont_id))) %>%
  summarise(mean = mean(ncontacts), Quartile.1 = quantile(ncontacts,0.25), Quartile.3 = quantile(ncontacts,0.75))

data <- flextable(hhconnum, col_keys = names(hhconnum))

data <- valign(data, valign = "center", part = "header")
save_as_docx("三线表结果" = data, path = "hhThree_Line_Table.docx")
