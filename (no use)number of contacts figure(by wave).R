library(dplyr)
partw1 <- inner_join(sday, part, by="part_id") %>% filter(wave==1)
conw1 <- inner_join(sday, con, by="part_id") %>% filter(wave==1)

partw1age1 <- inner_join(sday, part, by="part_id") %>% filter(wave==1&age_group=="[20,30)")
partw1age2 <- inner_join(sday, part, by="part_id") %>% filter(wave==1&age_group=="[30,40)")
partw1age3 <- inner_join(sday, part, by="part_id") %>% filter(wave==1&age_group=="[40,50)")
partw1age4 <- inner_join(sday, part, by="part_id") %>% filter(wave==1&age_group=="[50,60)")
partw1age5 <- inner_join(sday, part, by="part_id") %>% filter(wave==1&age_group=="[60,70)")
partw1age6 <- inner_join(sday, part, by="part_id") %>% filter(wave==1&age_group=="[70,100)")

id1=which(conw1$part_id%in%partw1age1$part_id)
id2=which(conw1$part_id%in%partw1age2$part_id)
id3=which(conw1$part_id%in%partw1age3$part_id)
id4=which(conw1$part_id%in%partw1age4$part_id)
id5=which(conw1$part_id%in%partw1age5$part_id)
id6=which(conw1$part_id%in%partw1age6$part_id)

conw1age1=conw1[c(id1),]
conw1age2=conw1[c(id2),]
conw1age3=conw1[c(id3),]
conw1age4=conw1[c(id4),]
conw1age5=conw1[c(id5),]
conw1age6=conw1[c(id6),]

connum_allage1 <- left_join(partw1age1, conw1age1, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allage2 <- left_join(partw1age2, conw1age2, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allage3 <- left_join(partw1age3, conw1age3, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allage4 <- left_join(partw1age4, conw1age4, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allage5 <- left_join(partw1age5, conw1age5, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allage6 <- left_join(partw1age6, conw1age6, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))

mean1=mean(connum_allage1$ncontacts)
mean2=mean(connum_allage2$ncontacts)
mean3=mean(connum_allage3$ncontacts)
mean4=mean(connum_allage4$ncontacts)
mean5=mean(connum_allage5$ncontacts)
mean6=mean(connum_allage6$ncontacts)

t1=t.test(connum_allage1$ncontacts)
t2=t.test(connum_allage2$ncontacts)
t3=t.test(connum_allage3$ncontacts)
t4=t.test(connum_allage4$ncontacts)
t5=t.test(connum_allage5$ncontacts)
t6=t.test(connum_allage6$ncontacts)

int11=t1[4]$conf.int[1]
int21=t2[4]$conf.int[1]
int31=t3[4]$conf.int[1]
int41=t4[4]$conf.int[1]
int51=t5[4]$conf.int[1]
int61=t6[4]$conf.int[1]

int12=t1[4]$conf.int[2]
int22=t2[4]$conf.int[2]
int32=t3[4]$conf.int[2]
int42=t4[4]$conf.int[2]
int52=t5[4]$conf.int[2]
int62=t6[4]$conf.int[2]

connumage.mean=c(mean1,mean2,mean3,mean4,mean5,mean6)
interval.t1=c(int11,int21,int31,int41,int51,int61)
interval.t2=c(int12,int22,int32,int42,int52,int62)
interval.name=c("[20,30)","[30,40)","[40,50)","[50,60)","[60,70)","[70,100)")
interval.data=data.frame(interval.name,connumage.mean,interval.t1,interval.t2)

##install.packages("pacman")
library(pacman)
pacman::p_load(forestplot)
##?forestplot
x1=interval.data[,1]
x2=interval.data[,2:4]
forestplot(interval.data[,1],interval.data[,2:4],
           ci.vertices=TRUE,
           ci.vertices.height=0.1,
           graph.pos=2,
           graphwidth=unit(100,"mm"),
           clip=c(0,5),
           xticks=c(0,1,2,3,4,5),
           txt_gp=fpTxtGp(ticks = gpar(cex=1),xlab = gpar(cex=1.2),cex = 1.2),
           col=fpColors(box = "black",line = "black",summary = "black"),
           fn.ci_norm=fpDrawCircleCI,
           boxsize=0.1,
           vertices=TRUE,
           xlab="Number of Contacts")
