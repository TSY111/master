library(here)

sday <- be_sday
part <- be_part
part$age_group=cut(part$part_age,c(seq(0,70,10),100),right=F)
con <- be_con

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

ageinterval.name1=c("1","2","3","4","5","6","7","8")
ageinterval.name2=c("wave1","wave2","wave3","wave4","wave5","wave6","wave7","wave8")

##install.packages("pacman")
#library(pacman)
#pacman::p_load(forestplot)
##?forestplot

#x1=interval.data[,1]
#x2=interval.data[,2:4]
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



######age group2
partw1age2 <- inner_join(sday, part, by="part_id") %>% filter(wave==1&age_group=="[20,30)")
partw2age2 <- inner_join(sday, part, by="part_id") %>% filter(wave==2&age_group=="[20,30)")
partw3age2 <- inner_join(sday, part, by="part_id") %>% filter(wave==3&age_group=="[20,30)")
partw4age2 <- inner_join(sday, part, by="part_id") %>% filter(wave==4&age_group=="[20,30)")
partw5age2 <- inner_join(sday, part, by="part_id") %>% filter(wave==5&age_group=="[20,30)")
partw6age2 <- inner_join(sday, part, by="part_id") %>% filter(wave==6&age_group=="[20,30)")
partw7age2 <- inner_join(sday, part, by="part_id") %>% filter(wave==7&age_group=="[20,30)")
partw8age2 <- inner_join(sday, part, by="part_id") %>% filter(wave==8&age_group=="[20,30)")


age2id1=which(conw1$part_id%in%partw1age2$part_id)
age2id2=which(conw2$part_id%in%partw2age2$part_id)
age2id3=which(conw3$part_id%in%partw3age2$part_id)
age2id4=which(conw4$part_id%in%partw4age2$part_id)
age2id5=which(conw5$part_id%in%partw5age2$part_id)
age2id6=which(conw6$part_id%in%partw6age2$part_id)
age2id7=which(conw7$part_id%in%partw7age2$part_id)
age2id8=which(conw8$part_id%in%partw8age2$part_id)


conw1age2=conw1[c(age2id1),]
conw2age2=conw2[c(age2id2),]
conw3age2=conw3[c(age2id3),]
conw4age2=conw4[c(age2id4),]
conw5age2=conw5[c(age2id5),]
conw6age2=conw6[c(age2id6),]
conw7age2=conw7[c(age2id7),]
conw8age2=conw8[c(age2id8),]

connum_allw1age2 <- left_join(partw1age2, conw1age2, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw2age2 <- left_join(partw2age2, conw2age2, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw3age2 <- left_join(partw3age2, conw3age2, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw4age2 <- left_join(partw4age2, conw4age2, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw5age2 <- left_join(partw5age2, conw5age2, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw6age2 <- left_join(partw6age2, conw6age2, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw7age2 <- left_join(partw7age2, conw7age2, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw8age2 <- left_join(partw8age2, conw8age2, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))

meanw1age2=mean(connum_allw1age2$ncontacts)
meanw2age2=mean(connum_allw2age2$ncontacts)
meanw3age2=mean(connum_allw3age2$ncontacts)
meanw4age2=mean(connum_allw4age2$ncontacts)
meanw5age2=mean(connum_allw5age2$ncontacts)
meanw6age2=mean(connum_allw6age2$ncontacts)
meanw7age2=mean(connum_allw7age2$ncontacts)
meanw8age2=mean(connum_allw8age2$ncontacts)

age2t1=t.test(connum_allw1age2$ncontacts)
age2t2=t.test(connum_allw2age2$ncontacts)
age2t3=t.test(connum_allw3age2$ncontacts)
age2t4=t.test(connum_allw4age2$ncontacts)
age2t5=t.test(connum_allw5age2$ncontacts)
age2t6=t.test(connum_allw6age2$ncontacts)
age2t7=t.test(connum_allw7age2$ncontacts)
age2t8=t.test(connum_allw8age2$ncontacts)

age2int11=age2t1[4]$conf.int[1]
age2int21=age2t2[4]$conf.int[1]
age2int31=age2t3[4]$conf.int[1]
age2int41=age2t4[4]$conf.int[1]
age2int51=age2t5[4]$conf.int[1]
age2int61=age2t6[4]$conf.int[1]
age2int71=age2t7[4]$conf.int[1]
age2int81=age2t8[4]$conf.int[1]

age2int12=age2t1[4]$conf.int[2]
age2int22=age2t2[4]$conf.int[2]
age2int32=age2t3[4]$conf.int[2]
age2int42=age2t4[4]$conf.int[2]
age2int52=age2t5[4]$conf.int[2]
age2int62=age2t6[4]$conf.int[2]
age2int72=age2t7[4]$conf.int[2]
age2int82=age2t8[4]$conf.int[2]

age2connumage.mean=c(meanw1age2,meanw2age2,meanw3age2,meanw4age2,meanw5age2,meanw6age2,meanw7age2,meanw8age2)
age2interval.t1=c(age2int11,age2int21,age2int31,age2int41,age2int51,age2int61,age2int71,age2int81)
age2interval.t2=c(age2int12,age2int22,age2int32,age2int42,age2int52,age2int62,age2int72,age2int82)


age2interval.data=data.frame(wave=ageinterval.name1,
                             number=age2connumage.mean,
                             dataset=ageinterval.name2,
                             upper=age2interval.t2,
                             lower=age2interval.t1
)



######age group3
partw1age3 <- inner_join(sday, part, by="part_id") %>% filter(wave==1&age_group=="[30,40)")
partw2age3 <- inner_join(sday, part, by="part_id") %>% filter(wave==2&age_group=="[30,40)")
partw3age3 <- inner_join(sday, part, by="part_id") %>% filter(wave==3&age_group=="[30,40)")
partw4age3 <- inner_join(sday, part, by="part_id") %>% filter(wave==4&age_group=="[30,40)")
partw5age3 <- inner_join(sday, part, by="part_id") %>% filter(wave==5&age_group=="[30,40)")
partw6age3 <- inner_join(sday, part, by="part_id") %>% filter(wave==6&age_group=="[30,40)")
partw7age3 <- inner_join(sday, part, by="part_id") %>% filter(wave==7&age_group=="[30,40)")
partw8age3 <- inner_join(sday, part, by="part_id") %>% filter(wave==8&age_group=="[30,40)")

age3id1=which(conw1$part_id%in%partw1age3$part_id)
age3id2=which(conw2$part_id%in%partw2age3$part_id)
age3id3=which(conw3$part_id%in%partw3age3$part_id)
age3id4=which(conw4$part_id%in%partw4age3$part_id)
age3id5=which(conw5$part_id%in%partw5age3$part_id)
age3id6=which(conw6$part_id%in%partw6age3$part_id)
age3id7=which(conw7$part_id%in%partw7age3$part_id)
age3id8=which(conw8$part_id%in%partw8age3$part_id)


conw1age3=conw1[c(age3id1),]
conw2age3=conw2[c(age3id2),]
conw3age3=conw3[c(age3id3),]
conw4age3=conw4[c(age3id4),]
conw5age3=conw5[c(age3id5),]
conw6age3=conw6[c(age3id6),]
conw7age3=conw7[c(age3id7),]
conw8age3=conw8[c(age3id8),]

connum_allw1age3 <- left_join(partw1age3, conw1age3, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw2age3 <- left_join(partw2age3, conw2age3, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw3age3 <- left_join(partw3age3, conw3age3, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw4age3 <- left_join(partw4age3, conw4age3, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw5age3 <- left_join(partw5age3, conw5age3, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw6age3 <- left_join(partw6age3, conw6age3, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw7age3 <- left_join(partw7age3, conw7age3, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw8age3 <- left_join(partw8age3, conw8age3, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))

meanw1age3=mean(connum_allw1age3$ncontacts)
meanw2age3=mean(connum_allw2age3$ncontacts)
meanw3age3=mean(connum_allw3age3$ncontacts)
meanw4age3=mean(connum_allw4age3$ncontacts)
meanw5age3=mean(connum_allw5age3$ncontacts)
meanw6age3=mean(connum_allw6age3$ncontacts)
meanw7age3=mean(connum_allw7age3$ncontacts)
meanw8age3=mean(connum_allw8age3$ncontacts)

age3t1=t.test(connum_allw1age3$ncontacts)
age3t2=t.test(connum_allw2age3$ncontacts)
age3t3=t.test(connum_allw3age3$ncontacts)
age3t4=t.test(connum_allw4age3$ncontacts)
age3t5=t.test(connum_allw5age3$ncontacts)
age3t6=t.test(connum_allw6age3$ncontacts)
age3t7=t.test(connum_allw7age3$ncontacts)
age3t8=t.test(connum_allw8age3$ncontacts)

age3int11=age3t1[4]$conf.int[1]
age3int21=age3t2[4]$conf.int[1]
age3int31=age3t3[4]$conf.int[1]
age3int41=age3t4[4]$conf.int[1]
age3int51=age3t5[4]$conf.int[1]
age3int61=age3t6[4]$conf.int[1]
age3int71=age3t7[4]$conf.int[1]
age3int81=age3t8[4]$conf.int[1]

age3int12=age3t1[4]$conf.int[2]
age3int22=age3t2[4]$conf.int[2]
age3int32=age3t3[4]$conf.int[2]
age3int42=age3t4[4]$conf.int[2]
age3int52=age3t5[4]$conf.int[2]
age3int62=age3t6[4]$conf.int[2]
age3int72=age3t7[4]$conf.int[2]
age3int82=age3t8[4]$conf.int[2]

age3connumage.mean=c(meanw1age3,meanw2age3,meanw3age3,meanw4age3,meanw5age3,meanw6age3,meanw7age3,meanw8age3)
age3interval.t1=c(age3int11,age3int21,age3int31,age3int41,age3int51,age3int61,age3int71,age3int81)
age3interval.t2=c(age3int12,age3int22,age3int32,age3int42,age3int52,age3int62,age3int72,age3int82)


age3interval.data=data.frame(wave=ageinterval.name1,
                             number=age3connumage.mean,
                             dataset=ageinterval.name2,
                             upper=age3interval.t2,
                             lower=age3interval.t1
)



######age group4
partw1age4 <- inner_join(sday, part, by="part_id") %>% filter(wave==1&age_group=="[40,50)")
partw2age4 <- inner_join(sday, part, by="part_id") %>% filter(wave==2&age_group=="[40,50)")
partw3age4 <- inner_join(sday, part, by="part_id") %>% filter(wave==3&age_group=="[40,50)")
partw4age4 <- inner_join(sday, part, by="part_id") %>% filter(wave==4&age_group=="[40,50)")
partw5age4 <- inner_join(sday, part, by="part_id") %>% filter(wave==5&age_group=="[40,50)")
partw6age4 <- inner_join(sday, part, by="part_id") %>% filter(wave==6&age_group=="[40,50)")
partw7age4 <- inner_join(sday, part, by="part_id") %>% filter(wave==7&age_group=="[40,50)")
partw8age4 <- inner_join(sday, part, by="part_id") %>% filter(wave==8&age_group=="[40,50)")

age4id1=which(conw1$part_id%in%partw1age4$part_id)
age4id2=which(conw2$part_id%in%partw2age4$part_id)
age4id3=which(conw3$part_id%in%partw3age4$part_id)
age4id4=which(conw4$part_id%in%partw4age4$part_id)
age4id5=which(conw5$part_id%in%partw5age4$part_id)
age4id6=which(conw6$part_id%in%partw6age4$part_id)
age4id7=which(conw7$part_id%in%partw7age4$part_id)
age4id8=which(conw8$part_id%in%partw8age4$part_id)


conw1age4=conw1[c(age4id1),]
conw2age4=conw2[c(age4id2),]
conw3age4=conw3[c(age4id3),]
conw4age4=conw4[c(age4id4),]
conw5age4=conw5[c(age4id5),]
conw6age4=conw6[c(age4id6),]
conw7age4=conw7[c(age4id7),]
conw8age4=conw8[c(age4id8),]

connum_allw1age4 <- left_join(partw1age4, conw1age4, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw2age4 <- left_join(partw2age4, conw2age4, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw3age4 <- left_join(partw3age4, conw3age4, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw4age4 <- left_join(partw4age4, conw4age4, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw5age4 <- left_join(partw5age4, conw5age4, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw6age4 <- left_join(partw6age4, conw6age4, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw7age4 <- left_join(partw7age4, conw7age4, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw8age4 <- left_join(partw8age4, conw8age4, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))

meanw1age4=mean(connum_allw1age4$ncontacts)
meanw2age4=mean(connum_allw2age4$ncontacts)
meanw3age4=mean(connum_allw3age4$ncontacts)
meanw4age4=mean(connum_allw4age4$ncontacts)
meanw5age4=mean(connum_allw5age4$ncontacts)
meanw6age4=mean(connum_allw6age4$ncontacts)
meanw7age4=mean(connum_allw7age4$ncontacts)
meanw8age4=mean(connum_allw8age4$ncontacts)

age4t1=t.test(connum_allw1age4$ncontacts)
age4t2=t.test(connum_allw2age4$ncontacts)
age4t3=t.test(connum_allw3age4$ncontacts)
age4t4=t.test(connum_allw4age4$ncontacts)
age4t5=t.test(connum_allw5age4$ncontacts)
age4t6=t.test(connum_allw6age4$ncontacts)
age4t7=t.test(connum_allw7age4$ncontacts)
age4t8=t.test(connum_allw8age4$ncontacts)

age4int11=age4t1[4]$conf.int[1]
age4int21=age4t2[4]$conf.int[1]
age4int31=age4t3[4]$conf.int[1]
age4int41=age4t4[4]$conf.int[1]
age4int51=age4t5[4]$conf.int[1]
age4int61=age4t6[4]$conf.int[1]
age4int71=age4t7[4]$conf.int[1]
age4int81=age4t8[4]$conf.int[1]

age4int12=age4t1[4]$conf.int[2]
age4int22=age4t2[4]$conf.int[2]
age4int32=age4t3[4]$conf.int[2]
age4int42=age4t4[4]$conf.int[2]
age4int52=age4t5[4]$conf.int[2]
age4int62=age4t6[4]$conf.int[2]
age4int72=age4t7[4]$conf.int[2]
age4int82=age4t8[4]$conf.int[2]

age4connumage.mean=c(meanw1age4,meanw2age4,meanw3age4,meanw4age4,meanw5age4,meanw6age4,meanw7age4,meanw8age4)
age4interval.t1=c(age4int11,age4int21,age4int31,age4int41,age4int51,age4int61,age4int71,age4int81)
age4interval.t2=c(age4int12,age4int22,age4int32,age4int42,age4int52,age4int62,age4int72,age4int82)


age4interval.data=data.frame(wave=ageinterval.name1,
                             number=age4connumage.mean,
                             dataset=ageinterval.name2,
                             upper=age4interval.t2,
                             lower=age4interval.t1
)



######age group5
partw1age5 <- inner_join(sday, part, by="part_id") %>% filter(wave==1&age_group=="[50,60)")
partw2age5 <- inner_join(sday, part, by="part_id") %>% filter(wave==2&age_group=="[50,60)")
partw3age5 <- inner_join(sday, part, by="part_id") %>% filter(wave==3&age_group=="[50,60)")
partw4age5 <- inner_join(sday, part, by="part_id") %>% filter(wave==4&age_group=="[50,60)")
partw5age5 <- inner_join(sday, part, by="part_id") %>% filter(wave==5&age_group=="[50,60)")
partw6age5 <- inner_join(sday, part, by="part_id") %>% filter(wave==6&age_group=="[50,60)")
partw7age5 <- inner_join(sday, part, by="part_id") %>% filter(wave==7&age_group=="[50,60)")
partw8age5 <- inner_join(sday, part, by="part_id") %>% filter(wave==8&age_group=="[50,60)")

age5id1=which(conw1$part_id%in%partw1age5$part_id)
age5id2=which(conw2$part_id%in%partw2age5$part_id)
age5id3=which(conw3$part_id%in%partw3age5$part_id)
age5id4=which(conw4$part_id%in%partw4age5$part_id)
age5id5=which(conw5$part_id%in%partw5age5$part_id)
age5id6=which(conw6$part_id%in%partw6age5$part_id)
age5id7=which(conw7$part_id%in%partw7age5$part_id)
age5id8=which(conw8$part_id%in%partw8age5$part_id)


conw1age5=conw1[c(age5id1),]
conw2age5=conw2[c(age5id2),]
conw3age5=conw3[c(age5id3),]
conw4age5=conw4[c(age5id4),]
conw5age5=conw5[c(age5id5),]
conw6age5=conw6[c(age5id6),]
conw7age5=conw7[c(age5id7),]
conw8age5=conw8[c(age5id8),]

connum_allw1age5 <- left_join(partw1age5, conw1age5, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw2age5 <- left_join(partw2age5, conw2age5, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw3age5 <- left_join(partw3age5, conw3age5, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw4age5 <- left_join(partw4age5, conw4age5, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw5age5 <- left_join(partw5age5, conw5age5, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw6age5 <- left_join(partw6age5, conw6age5, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw7age5 <- left_join(partw7age5, conw7age5, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw8age5 <- left_join(partw8age5, conw8age5, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))

meanw1age5=mean(connum_allw1age5$ncontacts)
meanw2age5=mean(connum_allw2age5$ncontacts)
meanw3age5=mean(connum_allw3age5$ncontacts)
meanw4age5=mean(connum_allw4age5$ncontacts)
meanw5age5=mean(connum_allw5age5$ncontacts)
meanw6age5=mean(connum_allw6age5$ncontacts)
meanw7age5=mean(connum_allw7age5$ncontacts)
meanw8age5=mean(connum_allw8age5$ncontacts)

age5t1=t.test(connum_allw1age5$ncontacts)
age5t2=t.test(connum_allw2age5$ncontacts)
age5t3=t.test(connum_allw3age5$ncontacts)
age5t4=t.test(connum_allw4age5$ncontacts)
age5t5=t.test(connum_allw5age5$ncontacts)
age5t6=t.test(connum_allw6age5$ncontacts)
age5t7=t.test(connum_allw7age5$ncontacts)
age5t8=t.test(connum_allw8age5$ncontacts)

age5int11=age5t1[4]$conf.int[1]
age5int21=age5t2[4]$conf.int[1]
age5int31=age5t3[4]$conf.int[1]
age5int41=age5t4[4]$conf.int[1]
age5int51=age5t5[4]$conf.int[1]
age5int61=age5t6[4]$conf.int[1]
age5int71=age5t7[4]$conf.int[1]
age5int81=age5t8[4]$conf.int[1]

age5int12=age5t1[4]$conf.int[2]
age5int22=age5t2[4]$conf.int[2]
age5int32=age5t3[4]$conf.int[2]
age5int42=age5t4[4]$conf.int[2]
age5int52=age5t5[4]$conf.int[2]
age5int62=age5t6[4]$conf.int[2]
age5int72=age5t7[4]$conf.int[2]
age5int82=age5t8[4]$conf.int[2]

age5connumage.mean=c(meanw1age5,meanw2age5,meanw3age5,meanw4age5,meanw5age5,meanw6age5,meanw7age5,meanw8age5)
age5interval.t1=c(age5int11,age5int21,age5int31,age5int41,age5int51,age5int61,age5int71,age5int81)
age5interval.t2=c(age5int12,age5int22,age5int32,age5int42,age5int52,age5int62,age5int72,age5int82)


age5interval.data=data.frame(wave=ageinterval.name1,
                             number=age5connumage.mean,
                             dataset=ageinterval.name2,
                             upper=age5interval.t2,
                             lower=age5interval.t1
)


######age group6
partw1age6 <- inner_join(sday, part, by="part_id") %>% filter(wave==1&age_group=="[60,70)")
partw2age6 <- inner_join(sday, part, by="part_id") %>% filter(wave==2&age_group=="[60,70)")
partw3age6 <- inner_join(sday, part, by="part_id") %>% filter(wave==3&age_group=="[60,70)")
partw4age6 <- inner_join(sday, part, by="part_id") %>% filter(wave==4&age_group=="[60,70)")
partw5age6 <- inner_join(sday, part, by="part_id") %>% filter(wave==5&age_group=="[60,70)")
partw6age6 <- inner_join(sday, part, by="part_id") %>% filter(wave==6&age_group=="[60,70)")
partw7age6 <- inner_join(sday, part, by="part_id") %>% filter(wave==7&age_group=="[60,70)")
partw8age6 <- inner_join(sday, part, by="part_id") %>% filter(wave==8&age_group=="[60,70)")

age6id1=which(conw1$part_id%in%partw1age6$part_id)
age6id2=which(conw2$part_id%in%partw2age6$part_id)
age6id3=which(conw3$part_id%in%partw3age6$part_id)
age6id4=which(conw4$part_id%in%partw4age6$part_id)
age6id5=which(conw5$part_id%in%partw5age6$part_id)
age6id6=which(conw6$part_id%in%partw6age6$part_id)
age6id7=which(conw7$part_id%in%partw7age6$part_id)
age6id8=which(conw8$part_id%in%partw8age6$part_id)


conw1age6=conw1[c(age6id1),]
conw2age6=conw2[c(age6id2),]
conw3age6=conw3[c(age6id3),]
conw4age6=conw4[c(age6id4),]
conw5age6=conw5[c(age6id5),]
conw6age6=conw6[c(age6id6),]
conw7age6=conw7[c(age6id7),]
conw8age6=conw8[c(age6id8),]

connum_allw1age6 <- left_join(partw1age6, conw1age6, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw2age6 <- left_join(partw2age6, conw2age6, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw3age6 <- left_join(partw3age6, conw3age6, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw4age6 <- left_join(partw4age6, conw4age6, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw5age6 <- left_join(partw5age6, conw5age6, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw6age6 <- left_join(partw6age6, conw6age6, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw7age6 <- left_join(partw7age6, conw7age6, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw8age6 <- left_join(partw8age6, conw8age6, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))

meanw1age6=mean(connum_allw1age6$ncontacts)
meanw2age6=mean(connum_allw2age6$ncontacts)
meanw3age6=mean(connum_allw3age6$ncontacts)
meanw4age6=mean(connum_allw4age6$ncontacts)
meanw5age6=mean(connum_allw5age6$ncontacts)
meanw6age6=mean(connum_allw6age6$ncontacts)
meanw7age6=mean(connum_allw7age6$ncontacts)
meanw8age6=mean(connum_allw8age6$ncontacts)

age6t1=t.test(connum_allw1age6$ncontacts)
age6t2=t.test(connum_allw2age6$ncontacts)
age6t3=t.test(connum_allw3age6$ncontacts)
age6t4=t.test(connum_allw4age6$ncontacts)
age6t5=t.test(connum_allw5age6$ncontacts)
age6t6=t.test(connum_allw6age6$ncontacts)
age6t7=t.test(connum_allw7age6$ncontacts)
age6t8=t.test(connum_allw8age6$ncontacts)

age6int11=age6t1[4]$conf.int[1]
age6int21=age6t2[4]$conf.int[1]
age6int31=age6t3[4]$conf.int[1]
age6int41=age6t4[4]$conf.int[1]
age6int51=age6t5[4]$conf.int[1]
age6int61=age6t6[4]$conf.int[1]
age6int71=age6t7[4]$conf.int[1]
age6int81=age6t8[4]$conf.int[1]

age6int12=age6t1[4]$conf.int[2]
age6int22=age6t2[4]$conf.int[2]
age6int32=age6t3[4]$conf.int[2]
age6int42=age6t4[4]$conf.int[2]
age6int52=age6t5[4]$conf.int[2]
age6int62=age6t6[4]$conf.int[2]
age6int72=age6t7[4]$conf.int[2]
age6int82=age6t8[4]$conf.int[2]

age6connumage.mean=c(meanw1age6,meanw2age6,meanw3age6,meanw4age6,meanw5age6,meanw6age6,meanw7age6,meanw8age6)
age6interval.t1=c(age6int11,age6int21,age6int31,age6int41,age6int51,age6int61,age6int71,age6int81)
age6interval.t2=c(age6int12,age6int22,age6int32,age6int42,age6int52,age6int62,age6int72,age6int82)

age6interval.data=data.frame(wave=ageinterval.name1,
                             number=age6connumage.mean,
                             dataset=ageinterval.name2,
                             upper=age6interval.t2,
                             lower=age6interval.t1
)


######age group7
partw1age7 <- inner_join(sday, part, by="part_id") %>% filter(wave==1&age_group=="[70,100)")
partw2age7 <- inner_join(sday, part, by="part_id") %>% filter(wave==2&age_group=="[70,100)")
partw3age7 <- inner_join(sday, part, by="part_id") %>% filter(wave==3&age_group=="[70,100)")
partw4age7 <- inner_join(sday, part, by="part_id") %>% filter(wave==4&age_group=="[70,100)")
partw5age7 <- inner_join(sday, part, by="part_id") %>% filter(wave==5&age_group=="[70,100)")
partw6age7 <- inner_join(sday, part, by="part_id") %>% filter(wave==6&age_group=="[70,100)")
partw7age7 <- inner_join(sday, part, by="part_id") %>% filter(wave==7&age_group=="[70,100)")
partw8age7 <- inner_join(sday, part, by="part_id") %>% filter(wave==8&age_group=="[70,100)")

age7id1=which(conw1$part_id%in%partw1age7$part_id)
age7id2=which(conw2$part_id%in%partw2age7$part_id)
age7id3=which(conw3$part_id%in%partw3age7$part_id)
age7id4=which(conw4$part_id%in%partw4age7$part_id)
age7id5=which(conw5$part_id%in%partw5age7$part_id)
age7id6=which(conw6$part_id%in%partw6age7$part_id)
age7id7=which(conw7$part_id%in%partw7age7$part_id)
age7id8=which(conw8$part_id%in%partw8age7$part_id)


conw1age7=conw1[c(age7id1),]
conw2age7=conw2[c(age7id2),]
conw3age7=conw3[c(age7id3),]
conw4age7=conw4[c(age7id4),]
conw5age7=conw5[c(age7id5),]
conw6age7=conw6[c(age7id6),]
conw7age7=conw7[c(age7id7),]
conw8age7=conw8[c(age7id8),]

connum_allw1age7 <- left_join(partw1age7, conw1age7, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw2age7 <- left_join(partw2age7, conw2age7, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw3age7 <- left_join(partw3age7, conw3age7, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw4age7 <- left_join(partw4age7, conw4age7, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw5age7 <- left_join(partw5age7, conw5age7, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw6age7 <- left_join(partw6age7, conw6age7, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw7age7 <- left_join(partw7age7, conw7age7, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))
connum_allw8age7 <- left_join(partw8age7, conw8age7, "part_id") %>% group_by(part_id) %>% summarise(ncontacts = sum(!is.na(sday_id.y)))

meanw1age7=mean(connum_allw1age7$ncontacts)
meanw2age7=mean(connum_allw2age7$ncontacts)
meanw3age7=mean(connum_allw3age7$ncontacts)
meanw4age7=mean(connum_allw4age7$ncontacts)
meanw5age7=mean(connum_allw5age7$ncontacts)
meanw6age7=mean(connum_allw6age7$ncontacts)
meanw7age7=mean(connum_allw7age7$ncontacts)
meanw8age7=mean(connum_allw8age7$ncontacts)

age7t1=t.test(connum_allw1age7$ncontacts)
age7t2=t.test(connum_allw2age7$ncontacts)
age7t3=t.test(connum_allw3age7$ncontacts)
age7t4=t.test(connum_allw4age7$ncontacts)
age7t5=t.test(connum_allw5age7$ncontacts)
age7t6=t.test(connum_allw6age7$ncontacts)
age7t7=t.test(connum_allw7age7$ncontacts)
age7t8=t.test(connum_allw8age7$ncontacts)

age7int11=age7t1[4]$conf.int[1]
age7int21=age7t2[4]$conf.int[1]
age7int31=age7t3[4]$conf.int[1]
age7int41=age7t4[4]$conf.int[1]
age7int51=age7t5[4]$conf.int[1]
age7int61=age7t6[4]$conf.int[1]
age7int71=age7t7[4]$conf.int[1]
age7int81=age7t8[4]$conf.int[1]

age7int12=age7t1[4]$conf.int[2]
age7int22=age7t2[4]$conf.int[2]
age7int32=age7t3[4]$conf.int[2]
age7int42=age7t4[4]$conf.int[2]
age7int52=age7t5[4]$conf.int[2]
age7int62=age7t6[4]$conf.int[2]
age7int72=age7t7[4]$conf.int[2]
age7int82=age7t8[4]$conf.int[2]

age7connumage.mean=c(meanw1age7,meanw2age7,meanw3age7,meanw4age7,meanw5age7,meanw6age7,meanw7age7,meanw8age7)
age7interval.t1=c(age7int11,age7int21,age7int31,age7int41,age7int51,age7int61,age7int71,age7int81)
age7interval.t2=c(age7int12,age7int22,age7int32,age7int42,age7int52,age7int62,age7int72,age7int82)

age7interval.data=data.frame(wave=ageinterval.name1,
                             number=age7connumage.mean,
                             dataset=ageinterval.name2,
                             upper=age7interval.t2,
                             lower=age7interval.t1
)

##figure
##install.packages("ggplot2")
library(ggplot2)

age2plot <- ggplot(age2interval.data, aes(wave, number, colour = dataset))
figure2=age2plot + geom_pointrange(aes(ymin = lower, ymax = upper),size=0.3)+
  ggtitle("participant age:
18-29") +
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("number of contacts")+
  xlab(NULL)+
  ylim(0,15)+
  coord_fixed(ratio=1.5)+
  theme(legend.position = "none")+
  theme(axis.text.x = element_blank())
figure2

age3plot <- ggplot(age3interval.data, aes(wave, number, colour = dataset))
figure3=age3plot + geom_pointrange(aes(ymin = lower, ymax = upper),size=0.3)+
  ggtitle("participant age:
30-39") +
  theme(plot.title = element_text(hjust = 0.5))+
  ylab(NULL)+
  xlab(NULL)+
  ylim(0,15)+
  coord_fixed(ratio=1.5)+
  theme(legend.position = "none")+
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())
figure3

age4plot <- ggplot(age4interval.data, aes(wave, number, colour = dataset))
figure4=age4plot + geom_pointrange(aes(ymin = lower, ymax = upper),size=0.3)+
  ggtitle("participant age:
40-49") +
  theme(plot.title = element_text(hjust = 0.5))+
  ylab(NULL)+
  xlab(NULL)+
  ylim(0,15)+
  coord_fixed(ratio=1.5)+
  theme(legend.position = "none")+
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())
figure4

age5plot <- ggplot(age5interval.data, aes(wave, number, colour = dataset))
figure5=age5plot + geom_pointrange(aes(ymin = lower, ymax = upper),size=0.3)+
  ggtitle("participant age:
50-59") +
  theme(plot.title = element_text(hjust = 0.5))+
  ylab(NULL)+
  xlab(NULL)+
  ylim(0,15)+
  coord_fixed(ratio=1.5)+
  theme(legend.position = "none")+
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())
figure5

age6plot <- ggplot(age6interval.data, aes(wave, number, colour = dataset))
figure6=age6plot + geom_pointrange(aes(ymin = lower, ymax = upper),size=0.3)+
  ggtitle("participant age:
60-69") +
  theme(plot.title = element_text(hjust = 0.5))+
  ylab(NULL)+
  xlab(NULL)+
  ylim(0,15)+
  coord_fixed(ratio=1.5)+
  theme(legend.position = "none")+
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())
figure6

age7plot <- ggplot(age7interval.data, aes(wave, number, colour = dataset))
figure7=age7plot + 
  geom_pointrange(aes(ymin = lower, ymax = upper),size=0.3)+
  ggtitle("participant age:
70+") +
  theme(plot.title = element_text(hjust = 0.5))+
  ylab(NULL)+
  xlab(NULL)+
  ylim(0,15)+
  coord_fixed(ratio=1.5)+
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())

figure7



##install.packages("patchwork")
library(patchwork)
figure2|figure3|figure4|figure5|figure6|figure7


#setwd("/Users/dogdogt/Desktop")
png(filename = "Fig1.png",
    width = 10,
    height = 10,
    units = "in",
    res = 300)
figure2|figure3|figure4|figure5|figure6|figure7
dev.off()
