#overall
allconnum <- inner_join(sday, part,by="part_id") %>%
  left_join(con, by="part_id") %>%
  group_by(wave, part_id) %>%
  filter(part_age!="Under 18")%>%
  summarise(ncontacts = sum(!is.na(cont_id))) %>%
  summarise(mean = mean(ncontacts), Quartile.1 = quantile(ncontacts,0.25), Quartile.3 = quantile(ncontacts,0.75))

data <- flextable(allconnum, col_keys = names(allconnum))

data <- valign(data, valign = "center", part = "header")
save_as_docx("三线表结果" = data, path = "allThree_Line_Table.docx")
