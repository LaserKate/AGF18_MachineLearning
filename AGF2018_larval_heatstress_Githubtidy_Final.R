
#Map----
#http://www.flutterbys.com.au/stats/tut/tut5.4.html #downloaded his zip file mapping.01
library(maps)
library(mapdata)
library(maptools)  #for shapefiles
library(scales)  #for transparency
library(dismo) #for spatial distribution analysis
library(mapping) ##install Murray's custom package mapping_0.1 using install packages prmpt
library(sp)
library(tidyr)

AGF18.Sites<-read.csv("AGF18.Sites_Github.csv")
plot(qld, xlab="lon", ylab="lat", xaxt="n", yaxt="n", type="n")
class(qld)
points(AGF18.Sites$lon, AGF18.Sites$lat, pch=19, col="red", cex=0.5)
xs <- pretty(c(140,155),2)
ys <- pretty(c(-5,-25),2)
axis(1, at=xs, lab=parse(text=degreeLabelsEW(xs)), mgp=c(0,0.5,0),tck=-0.003)
axis(2, at=ys, lab=parse(text=degreeLabelsNS(ys)), mgp=c(0,0.5,0),tck=-0.003)


######################START of larval survivorhship data----
larvSurv<-read.csv("larvaldataAGF2018_Github.csv")

larvSurv$Perc_surv<-as.numeric(larvSurv$Perc_surv)
larvSurv.result<-summarySE(larvSurv, measurevar="Perc_surv", groupvars=c("Treatment","Timepoint", "Parentage", "Mom", "Dad"), na.rm=TRUE)
#if get arror above during SummarySE, make sure you unattached dplyr. Then reattach below when you do unite
summarySE(larvSurv, measurevar="Perc_surv", groupvars=c("Treatment","Timepoint"), na.rm=TRUE)

larvSurv.result$Treatment2<-larvSurv.result$Treatment
larvSurv.result$Timepoint2<-larvSurv.result$Timepoint
larvSurv.result<-larvSurv.result %>% unite(trTim, c("Treatment2", "Timepoint2")) #unite in tidyr

library(ggrepel)
Allcrosses_larvalsurv2018_CURATE27<-larvSurv.result %>% 
  filter(trTim %in% c("27C_0", "27C_1", "27C_2", "27C_4", "35.5C_0", "35.5C_1", "35.5C_2", "35.5C_3", "35.5C_4")) %>% 
  ggplot(aes(x=Timepoint, y=Perc_surv, group=Parentage)) + 
  geom_ribbon(aes(ymin=Perc_surv-se, ymax=Perc_surv+se, fill=Mom), width=.1, alpha=1/4)+
  theme_bw()+
  geom_line(aes(color=Mom))+#linetype=Treatment
  geom_point(aes(color=Mom))+facet_grid(.~Treatment)+
  xlim(0, 6)+
  scale_colour_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))+
  scale_fill_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))+
  geom_text_repel(aes(label=Parentage, color=Mom), 
                  data= subset(larvSurv.result, Timepoint > 3),
                  nudge_x = 0.8, #
                  size = 3.5, 
                  segment.colour="black",
                  direction = "y", 
                  hjust= 0)


larvSurv.result<-summarySE(Rankplot_larv, measurevar="Survival", groupvars=c("Cross","Treatment"), na.rm=TRUE)
larvSurv.result %>%
  arrange(Perc_surv) %>%filter(Timepoint %in% c("4"))%>%
  #mutate(Parentage = factor(Parentage, levels=c("LS x SB","LS x LS","DR x DR","SB x SB","LS x CU","DR x SB","BK x BK","CU x CU", "CU x SB", "CU x BK", "SB x BK", "NA"))) %>%
  ggplot( aes(x=Parentage, y=Perc_surv)) +
  geom_segment(aes(xend=Parentage, yend=0)) +
  geom_point(size=4, aes(colour=Treatment)) +scale_colour_manual(values=c("#3B9AB2", "#F21A00","#EBCC2A")) +
  theme_bw()+#coord_flip()+
  xlab("")

larvSurv.result$Parentage<-factor(larvSurv.result$Parentage, levels=c("LS x SB", "SB x BK", "CU x BK", "CU x SB", "CU x CU", "CU x LS", "SB x DR", "BK x BK", "BK x CU", "LS x BK", "DR x CU", "CU x DR", "BK x LS", "SB x LS", "LS x DR", "DR x LS", "DR x BK", "DR x SB", "BK x SB",  "SB x CU", "BK x DR", "LS x CU", "SB x SB", "DR x DR", "LS x LS"))
Onlyfinaltime_Allcrosses_larvalsurv2018_CURATE27<-larvSurv.result %>%
  arrange(Perc_surv) %>%filter(Timepoint %in% c("4"))%>%
  #mutate(Parentage = factor(Parentage, levels=c("LS x SB", "SB x BK", "CU x BK", "CU x SB", "CU x CU", "CU x LS", "SB x DR", "BK x BK", "BK x CU", "LS x BK", "DR x CU", "CU x DR", "BK x LS", "SB x LS", "LS x DR", "DR x LS", "DR x BK", "DR x SB", "BK x SB",  "SB x CU", "BK x DR", "LS x CU", "SB x SB", "DR x DR", "LS x LS"))) +
  #"LS x LS","DR x DR","SB x SB","LS x CU","DR x SB","BK x BK","CU x CU", "CU x SB", "CU x BK", "SB x BK", "NA"))) %>%
  ggplot( aes(x=Parentage, y=Perc_surv)) +
  geom_segment(aes(xend=Parentage, yend=0), linetype=2) +
  geom_point(size=4, aes(shape=Treatment, colour=Mom))+
  scale_colour_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))+
  scale_fill_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))+
  theme_bw()+geom_linerange(aes(ymin = Perc_surv - se, ymax = Perc_surv + se, colour=Mom, width=0.9))+
  xlab("")+theme(axis.text.x=element_text(angle=90,hjust=1))


library(ggpubr)
my_comparisons_larv18 <- list( c("27C", "35.5C"))
larvSurv  %>% filter(Timepoint %in% c("4")) %>%
  ggboxplot(x = "Treatment", y = "Perc_surv",group="Treatment",
            color = "Treatment",
            add = "jitter") +facet_grid(Timepoint~Parentage)+stat_compare_means(comparisons = my_comparisons_larv18, method = "wilcox.test") +
  scale_colour_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))+
  scale_fill_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))# Add pairwise comparisons 


onlyT2_larv<-larvSurv  %>% filter(Timepoint %in% c("4"))
statz_larv<-compare_means(Perc_surv ~ Treatment, data = onlyT2_larv, method = "wilcox.test", p.adjust.method = "bonferroni", group.by=c("Parentage"))

dose.labs2 <- c("27C", "35.5C")
names(dose.labs2) <- c("27C", "35.5C")
larvSurv_regvals<-larvSurv %>% filter(Timepoint  %in% c("4")) %>%
  ggboxplot(x = "Treatment", y = "Perc_surv",group="Treatment",
            fill = "Treatment",
            add = "jitter") +facet_grid(Timepoint~Parentage, labeller = labeller(Zoox = dose.labs2))+theme(axis.text.x=element_blank())+
  scale_fill_grey(start = .5, end = 1, labels = c("27", "35.5"))+ylab("Percent survival")#+
  #stat_compare_means(comparisons = my_comparisons_larv18, method = "wilcox.test", label="p.format", fill="white")
(larvSurv_regvals.padj<-larvSurv_regvals + stat_pvalue_manual(statz_larv, y.position = 110,label = "{p.adj}"))+stat_compare_means(comparisons = my_comparisons_larv18, method = "wilcox.test")
#10x15 dimensions


######################END of larval survivorhship data----







#####################START of juvenile survivorship data----
#AGF_yr2_Juvs_cur.singles_aliveatT1<-read.csv("AGF_yr2_Juvs_cur.singles_aliveatT1_onlySurv_Github.csv")
AGF_yr2_Juvs_cur.singles_aliveatT1<-read.csv("AGF_yr2_Juvs_cur.singles_aliveatT1_onlySurv_Github2.csv")

AGF_yr2_Juvs_cur.singles_aliveatT1$Survival.T1<-as.integer(AGF_yr2_Juvs_cur.singles_aliveatT1$Survival.T1)

alljuveniles.raw<-AGF_yr2_Juvs_cur.singles_aliveatT1 %>%filter(Timepoint %in% c("T8"))
alljuveniles.raw %>%ggboxplot(x = "Treatment", y = "Survival.T1",add = "jitter") +
  facet_grid(Zoox~.)#+
  #stat_compare_means(method = "wilcox.test",aes(label=..p.adj..), label.y = c(29, 35, 40)) 

raw.juvenile.numbers.treat.zoox<-summarySE(alljuveniles.raw, measurevar="Survival.T1", groupvars=c("Treatment", "Zoox"), na.rm=TRUE)
raw.juvenile.numbers.treat.family.zoox<-summarySE(alljuveniles.raw, measurevar="Survival.T1", groupvars=c("Treatment", "Zoox", "Family"), na.rm=TRUE)
summarySE(alljuveniles.raw, measurevar="Survival.T1", groupvars=c("Treatment", "Zoox", "Family", "Tank"), na.rm=TRUE)

dose.labs <- c("C1", "D1a", "SED", "SS1")
names(dose.labs) <- c("C1", "D1", "SED", "SS")
alljuveniles.raw %>% 
  ggboxplot(x = "Treatment", y = "Survival.T1",group="Treatment",
            fill = "Treatment",
            add = "jitter") +facet_grid(Zoox~Family, labeller = labeller(Zoox = dose.labs))+theme(axis.text.x=element_blank())+
  scale_fill_grey(start = .5, end = 1, labels = c("27", "32"))+ylab("Percent survival")+
  geom_text(data = raw.juvenile.numbers.treat.family.zoox, 
            position=position_jitter(width=0.5), aes(y =3, x = 0.6, label = N, group="Treatment"))

View(compare_means(Survival.T1 ~ Treatment, data = alljuveniles.raw, method = "wilcox.test", p.adjust.method = "bonferroni", group.by=c("Zoox", "Family")))
View(compare_means(Survival.T1 ~ Treatment, data = alljuveniles.raw, method = "wilcox.test", p.adjust.method = "bonferroni", group.by=c("Zoox")))

raw.juvenile.numbers.treat.family<-summarySE(alljuveniles.raw, measurevar="Survival.T1", groupvars=c("Treatment","Family"), na.rm=TRUE)
raw.juvenile.numbers.treat.family.fig<-alljuveniles.raw %>% 
  ggboxplot(x = "Treatment", y = "Survival.T1",group="Treatment",
            fill = "Treatment",
            add = "jitter") +facet_grid(.~Family, labeller = labeller(Zoox = dose.labs))+theme(axis.text.x=element_blank())+
  scale_fill_grey(start = .5, end = 1, labels = c("27", "32"))+ylab("Percent survival")+
  geom_text(data = raw.juvenile.numbers.treat.family, 
            position=position_jitter(width=0.5), aes(y =3, x = 0.6, label = N, group="Treatment"))


alljuveniles.raw %>% 
  ggboxplot(x = "Treatment", y = "Survival.T1",group="Treatment",
            fill = "Treatment",
            add = "jitter") +facet_grid(.~Family)+theme(axis.text.x=element_blank())+
  scale_fill_grey(start = .5, end = 1, labels = c("27", "32"))+ylab("Percent survival") + stat_compare_means(method = "wilcox.test")
#already only T8, final timepoint
Alljuvs_raw.fam<-compare_means(Survival.T1 ~ Treatment, data = alljuveniles.raw, method = "wilcox.test", p.adjust.method = "bonferroni", group.by=c("Family"))






alljuveniles.raw.figure<-alljuveniles.raw %>% 
  ggboxplot(x = "Treatment", y = "Survival.T1",group="Treatment",
            fill = "Treatment",
            add = "jitter") +facet_grid(Zoox~., labeller = labeller(Zoox = dose.labs))+
  scale_fill_grey(start = .5, end = 1, labels = c("27", "32"))+ylab("Percent survival")+
  geom_text(data = raw.juvenile.numbers.treat.zoox, aes(y =1, x = 0.6, label = N, group="Treatment"), vjust=1)

ggplot(alljuveniles.raw.mutate, aes(x=Survival.T1)) +
  geom_density(alpha=0.6)+ facet_grid(Treatment~ Zoox)
  scale_fill_manual(name="Temperature" ,values = c("35.5C"="coral","27.5C"="skyblue"))+
  ggtitle("Larvae Survival")+xlab("Larvae survival percentage")+
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11))

#Survival.T1 where 2 is alive, and 1 is dead. for binomial, must be zero to 1, so change 1 to zero (Dead) and 2 to 1 (alive)
alljuveniles.raw$Survival.T1
#alive 2= 1607
#dead 1 = 474
(1607 + 474)
#summarySE(alljuveniles.raw, measurevar="Survival.T1", groupvars=c("Treatment"), na.rm=TRUE)
#Treatment    N Survival.T1        sd          se         ci
#1        26 1972    1.817444 0.3863999 0.008701288 0.01706469
#2        32 2081    1.772225 0.4194975 0.009195881 0.01803409

alljuveniles.raw.mutate<-alljuveniles.raw %>% mutate(Survival.T1 = str_replace(Survival.T1, "1", "0"))%>% mutate(Survival.T1 = str_replace(Survival.T1, "2", "1"))
#at hot
#alive 2= 1607
#dead 1 = 474
(1607 + 474)
summarySE(alljuveniles.raw.mutate, measurevar="Survival.T1", groupvars=c("Treatment"), na.rm=TRUE)
#Treatment    N Survival.T1        sd          se         ci
#1        26 1972    1.817444 0.3863999 0.008701288 0.01706469
#2        32 2081    1.772225 0.4194975 0.009195881 0.01803409

alljuveniles.raw.mutate$Survival.T1<-as.numeric(alljuveniles.raw.mutate$Survival.T1)
#dim(alljuveniles.raw.mutate) #4053 across all 
Survival_rawJuvneiles<-glmer(Survival.T1~Treatment : Zoox + (1|Mom) +(1|Dad) , data=alljuveniles.raw.mutate, family="binomial")
summary(Survival_rawJuvneiles)

Survival_rawJuvneiles.dropMom<-glmer(Survival.T1~Treatment : Zoox  +(1|Dad) , data=alljuveniles.raw.mutate, family="binomial")

anova(Survival_rawJuvneiles,Survival_rawJuvneiles.dropMom)

Survival_rawJuvneiles.family<-glm(Survival.T1~Treatment : Zoox: Family, data=alljuveniles.raw.mutate, family="binomial")
summary(Survival_rawJuvneiles.family)

survival_rawJuvneiles.cross<-glmer(Survival.T1~Treatment : Zoox  +(1|Tank) +(1|Family) , data=alljuveniles.raw.mutate, family="binomial")

summary(survival_rawJuvneiles.cross)

######

library(survival)#v.2.44-1.1
library(broom)
library(ggfortify)
library(patchwork)

#By Tank:
#survfit model juvenile----
surv_tank<-survfit(Surv(as.numeric(Timepoint), Survival.T1)~Zoox+Treatment+Tank+Family+Mom+Dad, data=AGF_yr2_Juvs_cur.singles_aliveatT1)
td_tank<-tidy(surv_tank)
td_tank_percent<-td_tank %>% mutate(PercAlive = n.event /(n.event+n.censor)*100)%>%
  mutate(PercDead = n.censor /(n.event+n.censor)*100)
td_tank_percent.seperate<-td_tank_percent%>% separate(strata, c("Zoox", "Treatment", "Tank", "Family", "Mom", "Dad"),"," )


library(stringr)#1.4.0
td_tank_percent.seperate2 <- td_tank_percent.seperate %>% 
  mutate(Treatment = str_replace(Treatment, "Treatment=", "")) %>%
  mutate(Zoox = str_replace(Zoox, "Zoox=", "")) %>%
  mutate(Tank = str_replace(Tank, "Tank=", "")) %>%
  mutate(Family = str_replace(Family, "Family=", "")) %>%
  mutate(Mom = str_replace(Mom, "Mom=", "")) %>%
  mutate(Dad = str_replace(Dad, "Dad=", ""))

td_tank_percent_Summary<-summarySE(td_tank_percent.seperate2, measurevar="PercAlive", groupvars=c("Treatment", "time","Zoox", "Family", "Mom", "Dad"), na.rm=TRUE)
td_tank_percent_Summary_treatments<-summarySE(td_tank_percent.seperate2, measurevar="PercAlive", groupvars=c("Treatment", "time","Zoox", "Mom", "Dad"), na.rm=TRUE)

cols <- c("26C" = "blue", "32C" = "red")

###Juvenile survivorship by symbiont type only----
td_tank_percent_Summary2<-summarySE(td_tank_percent.seperate2, measurevar="PercAlive", groupvars=c("Treatment", "time","Zoox"), na.rm=TRUE)
#  Treatment time Zoox  N PercAlive       sd       se       ci
#1         26    1   C1 62 100.00000  0.00000 0.000000 0.000000
#2         26    1   D1 61 100.00000  0.00000 0.000000 0.000000
#3         26    1  SED 59 100.00000  0.00000 0.000000 0.000000
#4         26    1   SS 60 100.00000  0.00000 0.000000 0.000000
#5         26    2   C1 62  87.12294 18.19840 2.311199 4.621529
#6         26    2   D1 61  82.49075 26.07866 3.339031 6.679056
#7         26    2  SED 59  75.49612 34.56858 4.500445 9.008620
#8         26    2   SS 59  82.56520 28.14478 3.664138 7.334570
#9         32    1   C1 65 100.00000  0.00000 0.000000 0.000000
#10        32    1   D1 65 100.00000  0.00000 0.000000 0.000000
#11        32    1  SED 57 100.00000  0.00000 0.000000 0.000000
#12        32    1   SS 60 100.00000  0.00000 0.000000 0.000000
#13        32    2   C1 65  70.20929 31.59309 3.918640 7.828383
#14        32    2   D1 65  77.22576 26.81230 3.325657 6.643764
#15        32    2  SED 57  77.43335 26.89671 3.562557 7.136659
#16        32    2   SS 60  76.68655 31.63640 4.084242 8.172548


summarySE(td_tank_percent.seperate2, measurevar="PercAlive", groupvars=c("Treatment","time"), na.rm=TRUE)
#Treatment time   N PercAlive       sd       se       ci
#1        26    1 242 100.00000  0.00000 0.000000 0.000000
#2        26    2 241  81.98828 27.41783 1.766138 3.479110
#3        32    1 248 100.00000  0.00000 0.000000 0.000000
#4        32    2 248  75.39047 29.31808 1.861700 3.666832

#Treatment time   N PercAlive       sd       se       ci
#1        26    1 242 100.00000  0.00000 0.000000 0.000000
#2        26    2 241  81.98828 27.41783 1.766138 3.479110
#3        32    1 247 100.00000  0.00000 0.000000 0.000000
#4        32    2 247  75.29624 29.33912 1.866804 3.676957

summaryZoox<-ggplot(data=td_tank_percent_Summary2, aes(x=as.factor(time), y=PercAlive, group=as.factor(Treatment), 
                                          Shape=as.factor(Treatment))) + #group=Treatment, color=Treatment#scale_colour_manual(values = cols)+scale_fill_manual(values = cols)+
  geom_point(aes(color=Treatment, shape=Treatment), size=7)+
  geom_line(aes(fill=Treatment),size=1)+#scale_shape_manual(values=c("C1"=16, "D1"=15, "SED" =8, "SS"=10))+
  facet_grid(.~Zoox)+
  scale_colour_manual(values = c("black", "black"))+
  geom_linerange(aes(ymin=PercAlive-se, ymax=PercAlive+se), size=1)+theme_bw()+theme(legend.position = "none")


summaryZooxC1<-td_tank_percent_Summary2%>%filter(Zoox %in% c("C1"))%>% ggplot(aes(x=as.factor(time), y=PercAlive, group=as.factor(Treatment), 
  Shape=as.factor(Treatment))) + #group=Treatment, color=Treatment#scale_colour_manual(values = cols)+scale_fill_manual(values = cols)+
  geom_point(aes(color=Treatment, shape=Treatment), size=4)+
  geom_line(aes(fill=Treatment),size=1)+#scale_shape_manual(values=c("C1"=16, "D1"=15, "SED" =8, "SS"=10))+
  #facet_grid(.~Zoox)+
  scale_colour_manual(values = c("black", "black"))+
  geom_linerange(aes(ymin=PercAlive-se, ymax=PercAlive+se), size=1)+theme_bw()+theme(legend.position = "none")+ theme(axis.text.x=element_text(angle=90,hjust=1))

summaryZooxD1<-td_tank_percent_Summary2%>%filter(Zoox %in% c("D1"))%>% ggplot(aes(x=as.factor(time), y=PercAlive, group=as.factor(Treatment), 
                                                                                  Shape=as.factor(Treatment))) + #group=Treatment, color=Treatment#scale_colour_manual(values = cols)+scale_fill_manual(values = cols)+
  geom_point(aes(color=Treatment, shape=Treatment), size=4)+
  geom_line(aes(fill=Treatment),size=1)+#scale_shape_manual(values=c("C1"=16, "D1"=15, "SED" =8, "SS"=10))+
  #facet_grid(.~Zoox)+
  scale_colour_manual(values = c("black", "black"))+
  geom_linerange(aes(ymin=PercAlive-se, ymax=PercAlive+se), size=1)+theme_bw()+theme(legend.position = "none")+ theme(axis.text.x=element_text(angle=90,hjust=1))

summaryZooxSED<-td_tank_percent_Summary2%>%filter(Zoox %in% c("SED"))%>% ggplot(aes(x=as.factor(time), y=PercAlive, group=as.factor(Treatment), 
                                                                                  Shape=as.factor(Treatment))) + #group=Treatment, color=Treatment#scale_colour_manual(values = cols)+scale_fill_manual(values = cols)+
  geom_point(aes(color=Treatment, shape=Treatment), size=4)+
  geom_line(aes(fill=Treatment),size=1)+#scale_shape_manual(values=c("C1"=16, "D1"=15, "SED" =8, "SS"=10))+
  #facet_grid(.~Zoox)+
  scale_colour_manual(values = c("black", "black"))+
  geom_linerange(aes(ymin=PercAlive-se, ymax=PercAlive+se), size=1)+theme_bw()+theme(legend.position = "none")+ theme(axis.text.x=element_text(angle=90,hjust=1))

summaryZooxSS<-td_tank_percent_Summary2%>%filter(Zoox %in% c("SS"))%>% ggplot(aes(x=as.factor(time), y=PercAlive, group=as.factor(Treatment), 
                                                                                  Shape=as.factor(Treatment))) + #group=Treatment, color=Treatment#scale_colour_manual(values = cols)+scale_fill_manual(values = cols)+
  geom_point(aes(color=Treatment, shape=Treatment), size=4)+
  geom_line(aes(fill=Treatment),size=1)+#scale_shape_manual(values=c("C1"=16, "D1"=15, "SED" =8, "SS"=10))+
  #facet_grid(.~Zoox)+
  scale_colour_manual(values = c("black", "black"))+
  geom_linerange(aes(ymin=PercAlive-se, ymax=PercAlive+se), size=1)+theme_bw()+theme(legend.position = "none")+ theme(axis.text.x=element_text(angle=90,hjust=1))



x_limits <- c(2, NA)

Juv_allcrosses_surv.colour<-ggplot(data=td_tank_percent_Summary, aes(x=as.factor(time), 
                                                                     y=PercAlive, 
                                                                     group=Family, 
                                                                     color=Mom)) + 
  geom_line(aes(group=Family, color=Mom))+
  geom_point(aes(color=Mom))+facet_grid(Zoox~Treatment)+
  geom_ribbon(aes(ymin=PercAlive-se, ymax=PercAlive+se, fill=Mom), width=.1, alpha=1/4)+
  scale_colour_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))+
  scale_fill_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))+
  geom_text_repel(aes(label=Family, color=Mom), 
                  data= subset(td_tank_percent_Summary, time > 1.5), 
                  nudge_x = 0.8, #
                  size = 3.5, 
                  segment.colour="black",
                  direction = "y", 
                  hjust= 0)+theme_bw()

td_tank_percent_Summary$Family<-factor(td_tank_percent_Summary$Family, levels=c(" LSxSB", " SBxBK", " CUxBK", " CUxSB", " CUxCU", " CUxLS", " SBxDR", " BKxBK", " BKxCU", " LSxBK", " DRxCU", " CUxDR", "	BKxLS", " SBxLS", " LSxDR", " DRxLS", " DRxBK", " DRxSB", " BKxSB",  " SBxCU", " BKxDR", " LSxCU", " SBxSB", " DRxDR", " LSxLS"))
Onlyfinaltime_Juv_allcrosses_surv.colour<-td_tank_percent_Summary %>%
  arrange(PercAlive) %>%filter(time %in% c("2"))%>%
  ggplot(aes(x=Family, y=PercAlive)) +
  geom_segment(aes(xend=Family, yend=0), linetype=2) +
  geom_point(size=4, aes(shape=Treatment, colour=Mom))+
  scale_colour_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))+
  scale_fill_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))+
  theme_bw()+facet_grid(Zoox~.)+scale_y_continuous(breaks = c(100,0))+
  geom_linerange(aes(ymin = PercAlive - se, ymax = PercAlive + se, colour=Mom, width=0.9))+
  xlab("")+ theme(axis.text.x=element_text(angle=90,hjust=1))

#####
td_tank_percent_Summary$Family<-factor(td_tank_percent_Summary$Family, levels=c(" LSxSB", " SBxBK", " CUxBK", " CUxSB", " CUxCU", " CUxLS", " SBxDR", " BKxBK", " BKxCU", " LSxBK", " DRxCU", " CUxDR", " BKxLS", " SBxLS", " LSxDR", " DRxLS", " DRxBK", " DRxSB", " BKxSB",  " SBxCU", " BKxDR", " LSxCU", " SBxSB", " DRxDR", " LSxLS"))# "SB x SB", "DR x DR", "LS x LS"))

td_tank_percent_Summary$Family<-factor(td_tank_percent_Summary$Family, levels=c(" BKxLS", " SBxDR", " CUxSB", " CUxLS", " SBxLS", " LSxCU", " CUxDR", " CUxCU", " LSxSB", " DRxCU", " LSxLS", " SBxBK", " SBxSB", " DRxBK", " DRxSB", " BKxDR", " DRxDR", " LSxBK", " BKxBK",  " BKxSB", " CUxBK", " BKxCU"))
C1_Onlyfinaltime_Juv_allcrosses_surv.colour<-td_tank_percent_Summary %>%
  arrange(PercAlive) %>%filter(time %in% c("2"))%>%filter(Zoox %in% c("C1"))%>%
  ggplot( aes(x=Family, y=PercAlive)) +
  geom_segment(aes(xend=Family, yend=0), linetype=2) +
  geom_point(size=4, aes(shape=Treatment, colour=Mom))+
  scale_colour_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))+
  scale_fill_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))+
  theme_bw()+facet_grid(Zoox~.)+
  geom_linerange(aes(ymin = PercAlive - se, ymax = PercAlive + se, colour=Mom, width=0.9))+
  xlab("")+ theme(axis.text.x=element_text(angle=90,hjust=1))

td_tank_percent_Summary.D1<-td_tank_percent_Summary %>% filter(Zoox %in% c("D1"))
td_tank_percent_Summary.D1$Family<-factor(td_tank_percent_Summary.D1$Family, levels=c(" CUxLS", " DRxBK", " CUxDR", " SBxLS", " CUxSB", " LSxSB", " DRxSB", " SBxSB", " LSxLS", " SBxBK", " LSxBK", " DRxCU", " LSxCU", " BKxBK", " DRxDR", " BKxDR", " CUxCU", " CUxBK", " BKxCU", " BKxLS", " BKxSB", " SBxDR"))
D1_Onlyfinaltime_Juv_allcrosses_surv.colour<-td_tank_percent_Summary.D1 %>%
  arrange(PercAlive) %>%filter(time %in% c("2"))%>%
  ggplot( aes(x=Family, y=PercAlive)) +
  geom_segment(aes(xend=Family, yend=0), linetype=2) +
  geom_point(size=4, aes(shape=Treatment, colour=Mom))+
  scale_colour_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))+
  scale_fill_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))+
  theme_bw()+facet_grid(Zoox~.)+
  geom_linerange(aes(ymin = PercAlive - se, ymax = PercAlive + se, colour=Mom, width=0.9))+
  xlab("")+ theme(axis.text.x=element_text(angle=90,hjust=1))

td_tank_percent_Summary.SED<-td_tank_percent_Summary %>% filter(Zoox %in% c("SED"))
td_tank_percent_Summary.SED$Family<-factor(td_tank_percent_Summary.SED$Family, levels=c(" DRxCU"," SBxSB"," CUxSB"," DRxSB"," LSxCU"," LSxLS"," CUxLS"," SBxBK", " DRxBK", " LSxSB"," SBxDR"," DRxDR"," BKxBK"," CUxCU"," SBxLS"," BKxLS"," CUxDR"," LSxBK"," BKxCU"," BKxDR"," CUxBK"," BKxSB"))
SED_Onlyfinaltime_Juv_allcrosses_surv.colour<-td_tank_percent_Summary.SED %>%
  arrange(PercAlive) %>%filter(time %in% c("2"))%>%
  ggplot( aes(x=Family, y=PercAlive)) +
  geom_segment(aes(xend=Family, yend=0), linetype=2) +
  geom_point(size=4, aes(shape=Treatment, colour=Mom))+
  scale_colour_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))+
  scale_fill_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))+
  theme_bw()+facet_grid(Zoox~.)+
  geom_linerange(aes(ymin = PercAlive - se, ymax = PercAlive + se, colour=Mom, width=0.9))+
  xlab("")+theme(axis.text.x=element_text(angle=90,hjust=1))

td_tank_percent_Summary.SS<-td_tank_percent_Summary %>% filter(Zoox %in% c("SS"))
td_tank_percent_Summary.SS$Family<-factor(td_tank_percent_Summary.SS$Family, levels=c(" BKxDR", " DRxSB", " CUxSB", " SBxLS", " LSxSB", " CUxLS", " LSxLS", " LSxCU", " BKxBK", " DRxBK", " SBxSB", " CUxCU", " CUxDR", " DRxDR", " BKxCU", " LSxBK", " DRxCU", " SBxBK", " SBxDR", " BKxSB", " CUxBK", " BKxLS"))
SS_Onlyfinaltime_Juv_allcrosses_surv.colour<-td_tank_percent_Summary.SS %>%
  arrange(PercAlive) %>%filter(time %in% c("2"))%>%
  ggplot( aes(x=Family, y=PercAlive)) +
  geom_segment(aes(xend=Family, yend=0), linetype=2) +
  geom_point(size=4, aes(shape=Treatment, colour=Mom))+
  scale_colour_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))+
  scale_fill_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))+
  theme_bw()+facet_grid(Zoox~.)+
  geom_linerange(aes(ymin = PercAlive - se, ymax = PercAlive + se, colour=Mom, width=0.9))+
  xlab("")+ theme(axis.text.x=element_text(angle=90,hjust=1))

onlyfinaltime_Juv_allcrosses_surv.colour<- C1_Onlyfinaltime_Juv_allcrosses_surv.colour + 
  D1_Onlyfinaltime_Juv_allcrosses_surv.colour + SED_Onlyfinaltime_Juv_allcrosses_surv.colour + 
  SS_Onlyfinaltime_Juv_allcrosses_surv.colour +plot_layout(guides = "collect", ncol = 1)

my_comparisons_juv18 <- list( c("26", "32"))
td_tank_percent.seperate2$Family <- as.factor(td_tank_percent.seperate2$Family)
td_tank_percent.seperate2$Zoox <- as.factor(td_tank_percent.seperate2$Zoox)

#____________
td_tank_percent_Summary2_wilcom_crosses<-td_tank_percent.seperate2 %>% filter(time %in% c("2")) %>%
  ggboxplot(x = "Treatment", y = "PercAlive",group="Treatment",
            color = "Treatment",
            add = "jitter") +facet_grid(Zoox~Family)+stat_compare_means(method = "wilcox.test", aes(label=..p.adj..), label.y = c(29, 35, 40)) +
  scale_colour_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))+
  scale_fill_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))# Add pairwise comparisons 


dose.labs <- c("C1", "D1a", "SED", "SS1")
names(dose.labs) <- c("C1", "D1", "SED", "SS")
td_tank_percent_Summary2_wilcom_crosses_novals<-td_tank_percent.seperate2 %>% filter(time %in% c("2")) %>%
  ggboxplot(x = "Treatment", y = "PercAlive",group="Treatment",
            fill = "Treatment",
            add = "jitter") +facet_grid(Zoox~Family, labeller = labeller(Zoox = dose.labs))+theme(axis.text.x=element_blank())+
  scale_fill_grey(start = .5, end = 1, labels = c("27", "32"))+ylab("Percent survival")
 #10x15 dimensions

onlyT2<-td_tank_percent.seperate2 %>% filter(time %in% c("2"))
statz<-compare_means(PercAlive ~ Treatment, data = onlyT2, method = "wilcox.test", p.adjust.method = "bonferroni", group.by=c("Zoox", "Family"))
#none of juvenle family comparisions signifncant

library(multcomp)
onlyT2$INT1<-interaction(onlyT2$Treatment, onlyT2$Zoox)
survival_rawJuvneiles.cross3<-glmer.nb(PercAlive~-1+INT1  +(1|Tank) +(1|Family) , data=onlyT2)
summary(glht(survival_rawJuvneiles.cross3, linfct=mcp(INT1="Tukey")))
summary(survival_rawJuvneiles.cross3)
# Variance-Covariance Matrix of fixed effects: 
VC.fixed.fullmodel <- as.matrix(vcov(survival_rawJuvneiles.cross3))
# Variance of fixed effects: 
Var.fixed.fullmodel <- diag(VC.fixed.fullmodel); Var.fixed.fullmodel
lme4::fixef(survival_rawJuvneiles.cross3)
X.var.totalvarFixed<- var(as.vector(lme4::fixef(survival_rawJuvneiles.cross3) %*% t(survival_rawJuvneiles.cross3@pp$X)))
mean(sqrt(diag(survival_rawJuvneiles.cross3@pp$X %*% vcov(survival_rawJuvneiles.cross3) %*% t(survival_rawJuvneiles.cross3@pp$X))))
#0.06706009

survival_rawJuvneiles.cross_noFam<-glmer.nb(PercAlive~-1+INT1  +(1|Tank) , data=onlyT2)
summary(glht(survival_rawJuvneiles.cross_noFam, linfct=mcp(INT1="Tukey")))
anova(survival_rawJuvneiles.cross3, survival_rawJuvneiles.cross_noFam)
summary(survival_rawJuvneiles.cross_noFam)
var(as.vector(lme4::fixef(survival_rawJuvneiles.cross_noFam) %*% t(survival_rawJuvneiles.cross_noFam@pp$X)))
mean(sqrt(diag(survival_rawJuvneiles.cross_noFam@pp$X %*% vcov(survival_rawJuvneiles.cross_noFam) %*% t(survival_rawJuvneiles.cross_noFam@pp$X))))
#0.0456636

#take too long runs forever
onlyT2$INT3<-interaction(onlyT2$Treatment, onlyT2$Zoox, onlyT2$Family)
survival_rawJuvneiles.cross4<-glmer.nb(PercAlive~-1+INT3  +(1|Tank) , data=onlyT2)
summary(glht(survival_rawJuvneiles.cross4, linfct=mcp(INT3="Tukey")))

#overall effect of reef

onlyT2$INT4<-interaction(onlyT2$Treatment, onlyT2$Zoox, onlyT2$Mom)
survival_rawJuvneiles.cross5<-glmer.nb(PercAlive~-1+INT4  +(1|Tank) +(1|Family) , data=onlyT2)
summary(glht(survival_rawJuvneiles.cross5, linfct=mcp(INT4="Tukey")))

onlyT2_C1<-onlyT2 %>% filter(Zoox %in% c("C1"))
onlyT2_C1$INT4<-interaction(onlyT2_C1$Treatment , onlyT2_C1$Mom)
survival_rawJuvneiles.cross_C1<-glmer.nb(PercAlive~-1+INT4  +(1|Tank) +(1|Family) , data=onlyT2_C1)
summary(glht(survival_rawJuvneiles.cross_C1, linfct=mcp(INT4="Tukey")))

dim(onlyT2_C1_hot<-onlyT2 %>% filter(Zoox %in% c("C1")) %>% filter(Treatment %in% c(" 32")))
onlyT2_C1_hot$INT5<-interaction(onlyT2_C1_hot$Mom)
survival_rawJuvneiles.cross_C1.2<-glmer.nb(PercAlive~-1+INT5  +(1|Tank) +(1|Family) , data=onlyT2_C1_hot)
summary(glht(survival_rawJuvneiles.cross_C1.2, linfct=mcp(INT5="Tukey")))

dim(onlyT2_D1_hot<-onlyT2 %>% filter(Zoox %in% c("D1")) %>% filter(Treatment %in% c(" 32")))
onlyT2_D1_hot$INT6<-interaction(onlyT2_D1_hot$Mom)
survival_rawJuvneiles.cross_D1.2<-glmer.nb(PercAlive~-1+INT6  +(1|Tank) +(1|Family) , data=onlyT2_D1_hot)
summary(glht(survival_rawJuvneiles.cross_D1.2, linfct=mcp(INT6="Tukey")))

dim(onlyT2_SED_hot<-onlyT2 %>% filter(Zoox %in% c("SED")) %>% filter(Treatment %in% c(" 32")))
onlyT2_SED_hot$INT7<-interaction(onlyT2_SED_hot$Mom)
survival_rawJuvneiles.cross_SED.2<-glmer.nb(PercAlive~-1+INT7  +(1|Tank) +(1|Family) , data=onlyT2_SED_hot)
summary(glht(survival_rawJuvneiles.cross_SED.2, linfct=mcp(INT7="Tukey")))

dim(onlyT2_SS_hot<-onlyT2 %>% filter(Zoox %in% c("SS")) %>% filter(Treatment %in% c(" 32")))
onlyT2_SS_hot$INT8<-interaction(onlyT2_SS_hot$Mom)
survival_rawJuvneiles.cross_SS.2<-glmer.nb(PercAlive~-1+INT8  +(1|Tank) +(1|Family) , data=onlyT2_SS_hot)
summary(glht(survival_rawJuvneiles.cross_SS.2, linfct=mcp(INT8="Tukey")))

#####
#survival_rawJuvneiles.cross.Zoox<-glmer.nb(PercAlive~Treatment * Zoox  +(1|Tank) +(1|Family) , data=onlyT2) #no percent
#summary(survival_rawJuvneiles.cross.Zoox)

#survival_rawJuvneiles.cross1<-glmer.nb(PercAlive~Treatment  +(1|Tank) +(1|Zoox:Family) , data=onlyT2) #no percent
#summary(survival_rawJuvneiles.cross1)

#survival_rawJuvneiles.cross2<-glmer.nb(PercAlive~Treatment  +(1|Tank) +(1|Family) + (1|Zoox) , data=onlyT2) #no percent
#summary(survival_rawJuvneiles.cross2)
#####




#overall wilcox test juvenile zoox survi td_tank_percent_Summary2_wilcom ----
my_comparisons_juv18 <- list( c("26", "32"))
td_tank_percent_Summary2_wilcom<-td_tank_percent.seperate2 %>% filter(time %in% c("2")) %>%
  ggboxplot(x = "Treatment", y = "PercAlive",group="Treatment",
            color = "Treatment",
            add = "jitter") +facet_grid(.~Zoox)+stat_compare_means(method = "wilcox.test") +
  scale_colour_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))+
  scale_fill_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))

td_tank_percent.seperate2 %>% filter(time %in% c("2")) %>%
  ggboxplot(x = "Treatment", y = "PercAlive",group="Treatment",
            color = "Treatment",
            add = "jitter") +facet_grid(Zoox~.)+stat_compare_means(method = "wilcox.test") +
  scale_colour_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))+
  scale_fill_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))

#Boxplots comparing percent survival of juveniles at the final timepoint (58 days) between each cross at 27 and 32?C. 
#Values above boxplots correspond to non-parametric Wilcox test. Note, no single or clumped juveniles settled for the SS treatment 
#at 27?C for BKxCU or 32?C for BKxLS, labelled as "/". Crosses and are labelled as "NA" are due to the Wilcox test  can also not able 
#to compute the test statistic for complete ties, i.e. all three replicates per treatment were the same at survival = 100%, both 27 
#and 32?C were 100% for CUxLS - D1, DRxCU - SED, BKxDR - SS)., 

#for Wilcox test with compelte ties: error will be :"Computation failed in `stat_compare_means()`: arguments imply differing number of rows: 0, 1 
#Computation failed in `stat_compare_means()`: argument "x" is missing, with no default = missind data






#18x18in
##***Final figure larva and juveniles----
Allcrosses_larvalsurv2018_CURATE27 + Juv_allcrosses_surv.colour +plot_layout(guides = "collect")

###
summary_Zoox<-summaryZooxC1 + summaryZooxD1+ summaryZooxSED +summaryZooxSS+plot_layout(guides = "collect", ncol=1)

part1<-Onlyfinaltime_Allcrosses_larvalsurv2018_CURATE27 + summaryZoox + plot_layout(guides = "collect", ncol = 2)
part1 + onlyfinaltime_Juv_allcrosses_surv.colour + plot_layout(guides = "collect", ncol = 3)

Onlyfinaltime_Allcrosses_larvalsurv2018_CURATE27 / summaryZoox | onlyfinaltime_Juv_allcrosses_surv.colour + plot_layout(guides = "collect", ncol = 2,widths = c(5, 5))

Onlyfinaltime_Allcrosses_larvalsurv2018_CURATE27 + summaryZoox | onlyfinaltime_Juv_allcrosses_surv.colour + plot_layout(guides = "collect", ncol = 2,widths = c(5, 5))


part1<-(Onlyfinaltime_Allcrosses_larvalsurv2018_CURATE27 / summaryZoox)  + plot_layout(guides = "collect", ncol = 1,widths = c(1, 4))


part1| onlyfinaltime_Juv_allcrosses_surv.colour + plot_layout(guides = "collect", ncol = 2,widths = c(5, 5))

((Onlyfinaltime_Allcrosses_larvalsurv2018_CURATE27 / summaryZoox) | onlyfinaltime_Juv_allcrosses_surv.colour) + plot_layout(guides = "collect",ncol = 2,widths = c(1, 4))

(Onlyfinaltime_Allcrosses_larvalsurv2018_CURATE27 | onlyfinaltime_Juv_allcrosses_surv.colour) + 
  plot_layout(guides = "collect",ncol = 2,widths = c(1, 2))

plot2<-(summary_Zoox|onlyfinaltime_Juv_allcrosses_surv.colour)+plot_layout(guides = "collect",ncol = 2,widths = c(1, 6))

(Onlyfinaltime_Allcrosses_larvalsurv2018_CURATE27 | (summary_Zoox|onlyfinaltime_Juv_allcrosses_surv.colour) + 
    plot_layout(guides = "collect",ncol = 2,widths = c(1, 4)))

(Onlyfinaltime_Allcrosses_larvalsurv2018_CURATE27+plot2)+ plot_layout(guides = "collect",ncol = 2,widths = c(1, 2))
ggsave("testmtcars.pdf", width = 183, height = 20, units = "mm") #10.5c14
####(1)NEW analysis comparing life-stages across families ----

###read in data combined larvae and juvenile data. Made new column with life-stage names, made sure reef names matched between two dataset, filtered both to the final timepoint for each.
#larv_juv_comparision.csv2<-read.csv("larv_juv_comparision_combo.csv", header=T, )
larv_juv_comparision.csv2<-read.csv("larv_juv_comparision_combo_v2.csv", header=T, )

larv_juv_comparision.csv2<-larv_juv_comparision.csv2%>% drop_na(Perc_surv)

larv_juv_comparision.csv2$lifestage<-factor(larv_juv_comparision.csv2$lifestage, levels=c("larvae", "juvenile"))

larv_juvenile_comparison_final_wilcox<-larv_juv_comparision.csv2 %>%
  ggboxplot(x = "lifestage", y = "Perc_surv",group="Parentage",
            #color = "Parentage",
            add = "jitter") +facet_grid(Treatment.1~Parentage)+stat_compare_means(method = "wilcox.test",aes(label=..p.adj..), label.y = c(29, 35, 40)) 

larv_juv_comparision.csv2 %>%ggboxplot(x = "lifestage", y = "Perc_surv",add = "jitter") +facet_grid(Treatment.1~.)+stat_compare_means(method = "wilcox.test",aes(label=..p.adj..), label.y = c(29, 35, 40)) 
summarySE(larv_juv_comparision.csv2, measurevar="Perc_surv", groupvars=c("Treatment", "lifestage"), na.rm=TRUE)
#Treatment lifestage   N Perc_surv        sd        se        ci
#1        26  juvenile 242  82.02139 27.426316 1.7630304 3.4729163
#2       27C    larvae 241  96.24481  5.783077 0.3725208 0.7338278
#3        32  juvenile 247  75.29624 29.339121 1.8668036 3.6769575
#4     35.5C    larvae 251  62.79928 21.869213 1.3803725 2.7186414

p_meds <- ddply(larv_juv_comparision.csv2, .(Treatment,lifestage), summarise, med = median(Perc_surv))


##with p.adj values

larv_juvenile_comparison_final_wilcox<-larv_juv_comparision.csv2 %>%
  ggboxplot(x = "lifestage", y = "Perc_surv",group="Parentage",
            #color = "Parentage",
            add = "jitter") +facet_grid(Treatment.1~Parentage)+stat_compare_means(method = "wilcox.test",aes(label=..p.adj..), label.y = c(29, 35, 40)) 

onlyfinal<-larv_juv_comparision.csv2  %>% filter(Timepoint %in% c("4", "2"))
statz_larv.juv<-compare_means(Perc_surv ~ lifestage, data = onlyfinal, method = "wilcox.test", p.adjust.method = "bonferroni", group.by=c("Parentage", "Treatment.1"))

dose.labs3 <- c("larvae", "juvenile")
names(dose.labs3) <- c("larvae", "juvenile")
larv_juvSurv_regvals<-larv_juv_comparision.csv2 %>% filter(Timepoint  %in% c("4", "2")) %>%
  ggboxplot(x = "lifestage", y = "Perc_surv",group="Parentage",
            fill = "Treatment.1",
            add = "jitter") +facet_grid(Treatment.1~Parentage, labeller = labeller(Treatment.1 = dose.labs3))+theme(axis.text.x=element_blank())+
  scale_fill_grey(start = .5, end = 1, labels = c("Mean ambient", "Elevated"))+ylab("Percent survival")#+
  #stat_compare_means(comparisons = my_comparisons_larv18, method = "wilcox.test", label="p.signif")
(larv_juvSurv_regvals.padj<-larv_juvSurv_regvals + stat_pvalue_manual(statz_larv.juv, y.position = 105,label = "{p.adj}"))+theme(strip.text.y = element_blank())
#10x15 dimensions




##############################done with larval and juvenile comparisions

####(2)NEW analysis comparing number of moms and dads and survival----
#Pearson because want interval scale
# Use R2 instead of R, One of "pearson" (default),
#cor_TotalMom<-ggscatter(larv_juv_comparision.csv2, x = "TotalMom", y = "Perc_surv", add = "reg.line") +
#  stat_cor(label.y = 120, 
#           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
#  stat_regline_equation(label.y = 130)+facet_grid(.~lifestage)

#cor_TotalDad<-ggscatter(larv_juv_comparision.csv2, x = "TotalDad", y = "Perc_surv", add = "reg.line") +
#  stat_cor(label.y = 120, 
#           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+facet_grid(.~lifestage)+stat_regline_equation(label.y = 130)

#cor_TotalMom + cor_TotalDad

##re checked to only include UNIQUE colonies for each cross
larv_juv_comparision.csv3<-read.csv("larv_juv_comparision_combo_v3.csv", header=T, )

larv_juv_comparision.csv3<-larv_juv_comparision.csv3%>% drop_na(Perc_surv)

larv_juv_comparision.csv3$lifestage<-factor(larv_juv_comparision.csv3$lifestage, levels=c("larvae", "juvenile"))

cor_TotalMom_v3<-ggscatter(larv_juv_comparision.csv3, x = "TotalMom", y = "Perc_surv", add = "reg.line") +
  stat_cor(label.y = 120, 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  stat_regline_equation(label.y = 130)+facet_grid(.~lifestage)

cor_TotalDad_v3<-ggscatter(larv_juv_comparision.csv3, x = "TotalDad", y = "Perc_surv", add = "reg.line") +
  stat_cor(label.y = 120, 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+facet_grid(.~lifestage)+stat_regline_equation(label.y = 130)

cor_TotalMom_v3 + cor_TotalDad_v3
###
larvSurv.result.pure<-larvSurv.result %>%filter(Parentage %in% c("CU x CU", "BK x BK", "LS x LS","DR x DR", "SB x SB"))
larvSurv.result.pure %>%
  arrange(Perc_surv) %>%filter(Timepoint %in% c("4"))%>%
  ggplot( aes(x=Parentage, y=Perc_surv)) +
  geom_segment(aes(xend=Parentage, yend=0), linetype=2) +
  geom_point(size=4, aes(shape=Treatment, colour=Mom))+
  scale_colour_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))+
  scale_fill_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))+
  theme_bw()+geom_linerange(aes(ymin = Perc_surv - se, ymax = Perc_surv + se, colour=Mom, width=0.9))+
  xlab("")+theme(axis.text.x=element_text(angle=90,hjust=1))

larvSurv.purenorth<-larvSurv %>%filter(Parentage %in% c("CU x CU", "LS x LS","SB x SB"))
larvSurv.pure<-larvSurv %>%filter(Parentage %in% c("CU x CU", "LS x LS","SB x SB","DR x DR", "BK x BK"))

larvSurv.result.purenorth<-summarySE(larvSurv.purenorth, measurevar="Perc_surv", groupvars=c("Treatment","Timepoint"), na.rm=TRUE)
#if get arror above during SummarySE, make sure you unattached dplyr. Then reattach below when you do unite
larvSurv.result.pure<-summarySE(larvSurv.pure, measurevar="Perc_surv", groupvars=c("Treatment","Timepoint"), na.rm=TRUE)

#40%
larvSurv.result.purenorth %>%
  arrange(Perc_surv) %>%#filter(Timepoint %in% c("4"))%>%
  ggplot( aes(x=Timepoint, y=Perc_surv)) +
  #geom_segment(aes(xend=Parentage, yend=0), linetype=2) +
  geom_point(size=4, aes(shape=Treatment))+
  scale_colour_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))+
  scale_fill_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))+
  theme_bw()+geom_linerange(aes(ymin = Perc_surv - se, ymax = Perc_surv + se, width=0.9))+
  xlab("")+theme(axis.text.x=element_text(angle=90,hjust=1))

larvSurv.result.pure %>%
  arrange(Perc_surv) %>%#filter(Timepoint %in% c("4"))%>%
  ggplot( aes(x=Timepoint, y=Perc_surv)) +
  #geom_segment(aes(xend=Parentage, yend=0), linetype=2) +
  geom_point(size=4, aes(shape=Treatment))+
  scale_colour_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))+
  scale_fill_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))+
  theme_bw()+geom_linerange(aes(ymin = Perc_surv - se, ymax = Perc_surv + se, width=0.9))+
  xlab("")+theme(axis.text.x=element_text(angle=90,hjust=1))

larvSurv.purecentral<-larvSurv %>%filter(Parentage %in% c("DR x DR", "BK x BK"))
larvSurv.result.purecentral<-summarySE(larvSurv.purecentral, measurevar="Perc_surv", groupvars=c("Treatment","Timepoint"), na.rm=TRUE)
larvSurv.result.purecentral %>%
  arrange(Perc_surv) %>%#filter(Timepoint %in% c("4"))%>%
  ggplot( aes(x=Timepoint, y=Perc_surv)) +
  #geom_segment(aes(xend=Parentage, yend=0), linetype=2) +
  geom_point(size=4, aes(shape=Treatment))+
  scale_colour_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))+
  scale_fill_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))+
  theme_bw()+geom_linerange(aes(ymin = Perc_surv - se, ymax = Perc_surv + se, width=0.9))+
  xlab("")+theme(axis.text.x=element_text(angle=90,hjust=1))

larvSurv.result.hyb.north<-larvSurv.result %>%
  filter(Parentage %in% c("LS x SB", "SB x BK", "CU x BK", "CU x SB", "CU x LS", 
  "SB x DR", "BK x CU", "LS x BK", "DR x CU", 
  "CU x DR", "BK x LS", "SB x LS", "LS x DR", "DR x LS", 
  "DR x BK", "DR x SB", "BK x SB",  "SB x CU", "BK x DR", 
  "LS x CU","DR x SB", "CU x SB", "CU x BK", "SB x BK", "NA"))
larvSurv.result.hyb.north<-summarySE(larvSurv.result.hyb.north, measurevar="Perc_surv", groupvars=c("Treatment","Timepoint"), na.rm=TRUE)

larvSurv.result.hyb.north %>%
  arrange(Perc_surv) %>%#filter(Timepoint %in% c("4"))%>%
  ggplot( aes(x=Timepoint, y=Perc_surv)) +
  #geom_segment(aes(xend=Parentage, yend=0), linetype=2) +
  geom_point(size=4, aes(shape=Treatment))+
  scale_colour_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))+
  scale_fill_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))+
  theme_bw()+geom_linerange(aes(ymin = Perc_surv - se, ymax = Perc_surv + se, width=0.9))+
  xlab("")+theme(axis.text.x=element_text(angle=90,hjust=1))

larvSurv.hyb.northbycentral<-larvSurv.result %>%
  filter(Parentage %in% c( "CU x BK",  
                          "SB x DR",  "LS x BK",  
                          "CU x DR", "LS x DR", "SB x BK", "NA"))
larvSurv.result.hyb.northbycentral<-summarySE(larvSurv.hyb.northbycentral, measurevar="Perc_surv", groupvars=c("Treatment","Timepoint"), na.rm=TRUE)

larvSurv.result.hyb.northbycentral %>%
  arrange(Perc_surv) %>%#filter(Timepoint %in% c("4"))%>%
  ggplot( aes(x=Timepoint, y=Perc_surv)) +
  #geom_segment(aes(xend=Parentage, yend=0), linetype=2) +
  geom_point(size=4, aes(shape=Treatment))+
  scale_colour_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))+
  scale_fill_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))+
  theme_bw()+geom_linerange(aes(ymin = Perc_surv - se, ymax = Perc_surv + se, width=0.9))+
  xlab("")+theme(axis.text.x=element_text(angle=90,hjust=1))
