##classification by age group
options(digits = 2)	
connum_waveIQR <- inner_join(sday, part, by="part_id") %>%
  left_join(con, by="part_id") %>%
  group_by(wave, part_age, part_id) %>%
  filter(part_age!="Under 18")%>%
  summarise(ncontacts = sum(!is.na(cont_id))) %>%
  summarise(mean = mean(ncontacts), Quartile.1 = quantile(ncontacts,0.25), Quartile.3 = quantile(ncontacts,0.75))
#install.packages("flextable")
library(flextable)
data <- flextable(connum_waveIQR, col_keys = names(connum_waveIQR))

data <- valign(data, valign = "center", part = "header")

save_as_docx("三线表结果" = data, path = "Three_Line_Table.docx")

