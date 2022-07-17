library(here)
source(here("get_data.R"))

getwd()
setwd("/Users/dogdogt/Downloads")
rm(list = ls())

library(here)
library(socialmixr)
library(tibble)
library(readr)

## If Belgium data have not already been downloaded, redownload.
if(!file.exists(here("data","be.rds")) || !file.exists(here("data","be_sday.csv"))){
  be_URLs <- capture.output(be_survey <- get_survey("10.5281/zenodo.4147585"), type="message") # Use Version 2 per Pietro Coletti.
  sday_URL <-  be_URLs[min(grep("_sday.csv", be_URLs))] %>% sub("Downloading ", "", .)
  dir.create(here("data"), FALSE)
  download.file(sday_URL, here("data", "be_sday.csv"))
  saveRDS(be_survey, here("data","be.rds"))
}

be_survey <- readRDS(here("data","be.rds"))
be_sday <- read_csv(here("data","be_sday.csv"))
be_part <- as_tibble(be_survey[[1]])
be_part <- be_part[!duplicated(be_part),]
be_con <- as_tibble(be_survey[[2]])
be_con <- be_con[!duplicated(be_con),]


library(here)

sday <- be_sday
part <- be_part
part$part_age_group <- cut(part$part_age,c(0, 18, seq(30,70,10),100),right=FALSE)
con <- be_con



###########wave1
comix1=inner_join(sday,part,by="part_id")%>%filter(wave=="1")
sur1=survey(comix1,con, reference = NULL)

contact_matrix(sur1, age.limits = c(0,18,30,40,50,60,70))
mat <- contact_matrix(sur1, age.limits = c(0,18,30,40,50,60,70))$matrix

library(reshape2)
library(ggplot2)
df <- melt(mat, varnames = c("par", "con"), value.name = "contacts")

df$text_color <- ifelse(df$contacts>mean(range(df$contacts, na.rm=TRUE)),"up" ,"low" )
wave1=ggplot(df, aes(x = par, y = con, fill = contacts),method="number")+
  theme(legend.position = "none") + 
  geom_tile()+ geom_tile()+ 
  geom_text(aes(label=paste0(round(100*contacts)),color=text_color), show.legend = FALSE) + 
  scale_colour_manual(values=c("cadetblue3","black"))+
  xlab("participant age group")+
  ylab("contact age group")+
  labs(title = "Comix Wave1")
wave1
###########wave2
comix1=inner_join(sday,part,by="part_id")%>%filter(wave=="2")
sur1=survey(comix1,con, reference = NULL)

contact_matrix(sur1, age.limits = c(0,18,30,40,50,60,70))
mat <- contact_matrix(sur1, age.limits = c(0,18,30,40,50,60,70))$matrix

library(reshape2)
library(ggplot2)
df <- melt(mat, varnames = c("par", "con"), value.name = "contacts")

df$text_color <- ifelse(df$contacts>mean(range(df$contacts, na.rm=TRUE)),"up" ,"low" )
wave2=ggplot(df, aes(x = par, y = con, fill = contacts),method="number")+
  theme(legend.position = "none") + 
  geom_tile()+ geom_tile()+ 
  geom_text(aes(label=paste0(round(100*contacts)),color=text_color), show.legend = FALSE) + 
  scale_colour_manual(values=c("cadetblue3","black"))+
  xlab("participant age group")+
  ylab("contact age group")+
  labs(title = "Comix Wave2")
wave2

###########wave3
comix1=inner_join(sday,part,by="part_id")%>%filter(wave=="3")
sur1=survey(comix1,con, reference = NULL)

contact_matrix(sur1, age.limits = c(0,18,30,40,50,60,70))
mat <- contact_matrix(sur1, age.limits = c(0,18,30,40,50,60,70))$matrix

library(reshape2)
library(ggplot2)
df <- melt(mat, varnames = c("par", "con"), value.name = "contacts")

df$text_color <- ifelse(df$contacts>mean(range(df$contacts, na.rm=TRUE)),"up" ,"low" )
wave3=ggplot(df, aes(x = par, y = con, fill = contacts),method="number")+
  theme(legend.position = "none") + 
  geom_tile()+ geom_tile()+ 
  geom_text(aes(label=paste0(round(100*contacts)),color=text_color), show.legend = FALSE) + 
  scale_colour_manual(values=c("cadetblue3","black"))+
  xlab("participant age group")+
  ylab("contact age group")+
  labs(title = "Comix Wave3")
wave3

###########wave4
comix1=inner_join(sday,part,by="part_id")%>%filter(wave=="4")
sur1=survey(comix1,con, reference = NULL)

contact_matrix(sur1, age.limits = c(0,18,30,40,50,60,70))
mat <- contact_matrix(sur1, age.limits = c(0,18,30,40,50,60,70))$matrix

library(reshape2)
library(ggplot2)
df <- melt(mat, varnames = c("par", "con"), value.name = "contacts")

df$text_color <- ifelse(df$contacts>mean(range(df$contacts, na.rm=TRUE)),"up" ,"low" )
wave4=ggplot(df, aes(x = par, y = con, fill = contacts),method="number")+
  theme(legend.position = "none") + 
  geom_tile()+ geom_tile()+ 
  geom_text(aes(label=paste0(round(100*contacts)),color=text_color), show.legend = FALSE) + 
  scale_colour_manual(values=c("cadetblue3","black"))+
  xlab("participant age group")+
  ylab("contact age group")+
  labs(title = "Comix Wave4")
wave4

###########wave5
comix1=inner_join(sday,part,by="part_id")%>%filter(wave=="5")
sur1=survey(comix1,con, reference = NULL)

contact_matrix(sur1, age.limits = c(0,18,30,40,50,60,70))
mat <- contact_matrix(sur1, age.limits = c(0,18,30,40,50,60,70))$matrix

library(reshape2)
library(ggplot2)
df <- melt(mat, varnames = c("par", "con"), value.name = "contacts")

df$text_color <- ifelse(df$contacts>mean(range(df$contacts, na.rm=TRUE)),"up" ,"low" )
wave5=ggplot(df, aes(x = par, y = con, fill = contacts),method="number")+
  theme(legend.position = "none") + 
  geom_tile()+ geom_tile()+ 
  geom_text(aes(label=paste0(round(100*contacts)),color=text_color), show.legend = FALSE) + 
  scale_colour_manual(values=c("cadetblue3","black"))+
  xlab("participant age group")+
  ylab("contact age group")+
  labs(title = "Comix Wave5")
wave5

###########wave6
comix1=inner_join(sday,part,by="part_id")%>%filter(wave=="6")
sur1=survey(comix1,con, reference = NULL)

contact_matrix(sur1, age.limits = c(0,18,30,40,50,60,70),symmetric = TRUE)
mat <- contact_matrix(sur1, age.limits = c(0,18,30,40,50,60,70))$matrix

library(reshape2)
library(ggplot2)
df <- melt(mat, varnames = c("par", "con"), value.name = "contacts")

df$text_color <- ifelse(df$contacts>mean(range(df$contacts, na.rm=TRUE)),"up" ,"low" )
wave6=ggplot(df, aes(x = par, y = con, fill = contacts),method="number")+
  theme(legend.position = "none") + 
  geom_tile()+ geom_tile()+ 
  geom_text(aes(label=paste0(round(100*contacts)),color=text_color), show.legend = FALSE) + 
  scale_colour_manual(values=c("cadetblue3","black"))+
  xlab("participant age group")+
  ylab("contact age group")+
  labs(title = "Comix Wave6")
wave6

###########wave7
comix1=inner_join(sday,part,by="part_id")%>%filter(wave=="7")
sur1=survey(comix1,con, reference = NULL)

contact_matrix(sur1, age.limits = c(0,18,30,40,50,60,70),symmetric = TRUE)
mat <- contact_matrix(sur1, age.limits = c(0,18,30,40,50,60,70))$matrix

library(reshape2)
library(ggplot2)
df <- melt(mat, varnames = c("par", "con"), value.name = "contacts")

df$text_color <- ifelse(df$contacts>mean(range(df$contacts, na.rm=TRUE)),"up" ,"low" )
wave7=ggplot(df, aes(x = par, y = con, fill = contacts),method="number")+
  theme(legend.position = "none") + 
  geom_tile()+ geom_tile()+ 
  geom_text(aes(label=paste0(round(100*contacts)),color=text_color), show.legend = FALSE) + 
  scale_colour_manual(values=c("cadetblue3","black"))+
  xlab("participant age group")+
  ylab("contact age group")+
  labs(title = "Comix Wave7")
wave7
###########wave8
comix1=inner_join(sday,part,by="part_id")%>%filter(wave=="8")
sur1=survey(comix1,con, reference = NULL)

contact_matrix(sur1, age.limits = c(0,18,30,40,50,60,70),symmetric = TRUE)
mat <- contact_matrix(sur1, age.limits = c(0,18,30,40,50,60,70))$matrix
as.data.frame(mat)
library(reshape2)
library(ggplot2)
df <- melt(mat, varnames = c("par", "con"), value.name = "contacts")

df$text_color <- ifelse(df$contacts>mean(range(df$contacts, na.rm=TRUE)),"up" ,"low" )
wave8=ggplot(df, aes(x = par, y = con, fill = contacts),method="number")+
  theme(legend.position = "none") + 
  geom_tile()+ geom_tile()+ 
  geom_text(aes(label=paste0(round(100*contacts)),color=text_color), show.legend = FALSE) + 
  scale_colour_manual(values=c("cadetblue3","black"))+
  xlab("participant age group")+
  ylab("contact age group")+
  labs(title = "Comix Wave8")
wave8

library(patchwork)
wave1+wave2+wave3+wave4+wave5+wave6+wave7+wave8+plot_layout(nrow = 2)

png(filename = "becomix.png",
    width = 15,
    height = 8,
    units = "in",
    res = 300)
wave1+wave2+wave3+wave4+wave5+wave6+wave7+wave8+plot_layout(nrow = 2)
dev.off()

