###by age
library(dplyr)
####wave
partw1 <- inner_join(sday, part, by="part_id") %>% filter(wave==1)
partw2 <- inner_join(sday, part, by="part_id") %>% filter(wave==2)
partw3 <- inner_join(sday, part, by="part_id") %>% filter(wave==3)
partw4 <- inner_join(sday, part, by="part_id") %>% filter(wave==4)
partw5 <- inner_join(sday, part, by="part_id") %>% filter(wave==5)
partw6 <- inner_join(sday, part, by="part_id") %>% filter(wave==6)
partw7 <- inner_join(sday, part, by="part_id") %>% filter(wave==7)
partw8 <- inner_join(sday, part, by="part_id") %>% filter(wave==8)

conw1 <- inner_join(sday, con, by="part_id") %>% filter(wave==1)
conw2 <- inner_join(sday, con, by="part_id") %>% filter(wave==2)
conw3 <- inner_join(sday, con, by="part_id") %>% filter(wave==3)
conw4 <- inner_join(sday, con, by="part_id") %>% filter(wave==4)
conw5 <- inner_join(sday, con, by="part_id") %>% filter(wave==5)
conw6 <- inner_join(sday, con, by="part_id") %>% filter(wave==6)
conw7 <- inner_join(sday, con, by="part_id") %>% filter(wave==7)
conw8 <- inner_join(sday, con, by="part_id") %>% filter(wave==8)

######age group1
partw1age1 <- inner_join(sday, part, by="part_id") %>% filter(wave==1&age_group=="[20,30)")
partw2age1 <- inner_join(sday, part, by="part_id") %>% filter(wave==2&age_group=="[20,30)")
partw3age1 <- inner_join(sday, part, by="part_id") %>% filter(wave==3&age_group=="[20,30)")
partw4age1 <- inner_join(sday, part, by="part_id") %>% filter(wave==4&age_group=="[20,30)")
partw5age1 <- inner_join(sday, part, by="part_id") %>% filter(wave==5&age_group=="[20,30)")
partw6age1 <- inner_join(sday, part, by="part_id") %>% filter(wave==6&age_group=="[20,30)")
partw7age1 <- inner_join(sday, part, by="part_id") %>% filter(wave==7&age_group=="[20,30)")
partw8age1 <- inner_join(sday, part, by="part_id") %>% filter(wave==8&age_group=="[20,30)")

age1id1=which(conw1$part_id%in%partw1age1$part_id)
age1id2=which(conw2$part_id%in%partw2age1$part_id)
age1id3=which(conw3$part_id%in%partw3age1$part_id)
age1id4=which(conw4$part_id%in%partw4age1$part_id)
age1id5=which(conw5$part_id%in%partw5age1$part_id)
age1id6=which(conw6$part_id%in%partw6age1$part_id)
age1id7=which(conw7$part_id%in%partw7age1$part_id)
age1id8=which(conw8$part_id%in%partw8age1$part_id)


conw1age1=conw1[c(age1id1),]
conw2age1=conw2[c(age1id2),]
conw3age1=conw3[c(age1id3),]
conw4age1=conw4[c(age1id4),]
conw5age1=conw5[c(age1id5),]
conw6age1=conw6[c(age1id6),]
conw7age1=conw7[c(age1id7),]
conw8age1=conw8[c(age1id8),]

connum_allw1age1 <- left_join(partw1age1, conw1age1, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw2age1 <- left_join(partw2age1, conw2age1, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw3age1 <- left_join(partw3age1, conw3age1, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw4age1 <- left_join(partw4age1, conw4age1, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw5age1 <- left_join(partw5age1, conw5age1, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw6age1 <- left_join(partw6age1, conw6age1, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw7age1 <- left_join(partw7age1, conw7age1, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw8age1 <- left_join(partw8age1, conw8age1, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))

meanw1age1=mean(connum_allw1age1$ncontacts)
meanw2age1=mean(connum_allw2age1$ncontacts)
meanw3age1=mean(connum_allw3age1$ncontacts)
meanw4age1=mean(connum_allw4age1$ncontacts)
meanw5age1=mean(connum_allw5age1$ncontacts)
meanw6age1=mean(connum_allw6age1$ncontacts)
meanw7age1=mean(connum_allw7age1$ncontacts)
meanw8age1=mean(connum_allw8age1$ncontacts)

age1t1=t.test(connum_allw1age1$ncontacts)
age1t2=t.test(connum_allw2age1$ncontacts)
age1t3=t.test(connum_allw3age1$ncontacts)
age1t4=t.test(connum_allw4age1$ncontacts)
age1t5=t.test(connum_allw5age1$ncontacts)
age1t6=t.test(connum_allw6age1$ncontacts)
age1t7=t.test(connum_allw7age1$ncontacts)
age1t8=t.test(connum_allw8age1$ncontacts)

age1int11=age1t1[4]$conf.int[1]
age1int21=age1t2[4]$conf.int[1]
age1int31=age1t3[4]$conf.int[1]
age1int41=age1t4[4]$conf.int[1]
age1int51=age1t5[4]$conf.int[1]
age1int61=age1t6[4]$conf.int[1]
age1int71=age1t7[4]$conf.int[1]
age1int81=age1t8[4]$conf.int[1]

age1int12=age1t1[4]$conf.int[2]
age1int22=age1t2[4]$conf.int[2]
age1int32=age1t3[4]$conf.int[2]
age1int42=age1t4[4]$conf.int[2]
age1int52=age1t5[4]$conf.int[2]
age1int62=age1t6[4]$conf.int[2]
age1int72=age1t7[4]$conf.int[2]
age1int82=age1t8[4]$conf.int[2]

age1connumage.mean=c(meanw1age1,meanw2age1,meanw3age1,meanw4age1,meanw5age1,meanw6age1,meanw7age1,meanw8age1)
age1interval.t1=c(age1int11,age1int21,age1int31,age1int41,age1int51,age1int61,age1int71,age1int81)
age1interval.t2=c(age1int12,age1int22,age1int32,age1int42,age1int52,age1int62,age1int72,age1int82)
age1interval.name=c("wave1","wave2","wave3","wave4","wave5","wave6","wave7","wave8")

##install.packages("pacman")
#library(pacman)
#pacman::p_load(forestplot)
##?forestplot

x1=interval.data[,1]
x2=interval.data[,2:4]
##figure=forestplot(age1interval.data[,1],age1interval.data[,2:4],
#           ci.vertices=TRUE,
#           ci.vertices.height=0.1,
#           graph.pos=2,
#           graphwidth=unit(100,"mm"),
#           clip=c(0,20),
#           xticks=c(0,5,10,15,20),
#           txt_gp=fpTxtGp(ticks = gpar(cex=1),xlab = gpar(cex=1.2),cex = 1.2),
#           col=fpColors(box = "black",line = "black",summary = "black"),
#           fn.ci_norm=fpDrawCircleCI,
#           boxsize=0.1,
#           vertices=TRUE,
#           xlab="Number of Contacts",
#           title="participant age 18-29")
##figure
##install.packages("ggplot2")
library(ggplot2)

age1interval.data=data.frame(wave=age1interval.name,
              number=age1connumage.mean,
              group=age1interval.name,
              upper=age1interval.t2,
              lower=age1interval.t1
              )

age1plot <- ggplot(age1interval.data, aes(wave, number, colour = group))
age1plot + geom_pointrange(aes(ymin = lower, ymax = upper))+
  ggtitle("participant age 18-29") +
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("number of contacts")

######age group2

