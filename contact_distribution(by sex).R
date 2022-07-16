##classification by gender
genderconnum <- inner_join(sday, part,by="part_id") %>%
  left_join(con, by="part_id") %>%
  group_by(wave, part_gender, part_id) %>%
  filter(part_age!="Under 18")%>%
  summarise(ncontacts = sum(!is.na(cont_id))) %>%
  summarise(mean = mean(ncontacts), Quartile.1 = quantile(ncontacts,0.25), Quartile.3 = quantile(ncontacts,0.75))

data <- flextable(genderconnum, col_keys = names(genderconnum))

data <- valign(data, valign = "center", part = "header")
save_as_docx("三线表结果" = data, path = "genderThree_Line_Table.docx")
