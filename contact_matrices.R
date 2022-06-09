library(here)
source(here("get_data.R"))

library(socialmixr)

sday <- be_sday
part <- be_part
con <- be_con

library(dplyr)
partw1 <- inner_join(sday, part, by="part_id") %>% filter(wave==1)
conw1 <- inner_join(sday, con, by="part_id") %>% filter(wave==1)

sur1 <- survey(partw1, conw1, reference = NULL)

contact_matrix(sur1, age.limits = c(0, 5, 18,30,40,50,60,70))
mat <- contact_matrix(sur1, age.limits = c(0, 5, 18,30,40,50,60,70))$matrix

library(reshape2)
library(ggplot2)
df <- melt(mat, varnames = c("par", "con"), value.name = "contacts")
ggplot(df, aes(x = par, y = con, fill = contacts),method="number")+theme(legend.position = "bottom") + geom_tile()
