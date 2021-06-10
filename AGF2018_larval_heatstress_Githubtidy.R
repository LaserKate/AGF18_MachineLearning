#Map----
#http://www.flutterbys.com.au/stats/tut/tut5.4.html #downloaded his zip file mapping.01
library(maps)
library(mapdata)
library(maptools)  #for shapefiles
library(scales)  #for transparency
library(dismo) #for spatial distribution analysis
library(mapping) ##install Murray's custom package mapping_0.1 using install packages prmpt
library(sp)

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

library(ggpubr)
my_comparisons_larv18 <- list( c("27C", "35.5C"))
larvSurv  %>% filter(Timepoint %in% c("4")) %>%
  ggboxplot(x = "Treatment", y = "Perc_surv",group="Treatment",
            color = "Treatment",
            add = "jitter") +facet_grid(Timepoint~Parentage)+stat_compare_means(comparisons = my_comparisons_larv18, method = "wilcox.test") +
  scale_colour_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))+
  scale_fill_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))# Add pairwise comparisons 

######################END of larval survivorhship data----

#####################START of juvenile survivorship data----
AGF_yr2_Juvs_cur.singles_aliveatT1<-read.csv("AGF_yr2_Juvs_cur.singles_aliveatT1_onlySurv_Github.csv")

AGF_yr2_Juvs_cur.singles_aliveatT1$Survival.T1<-as.integer(AGF_yr2_Juvs_cur.singles_aliveatT1$Survival.T1)

library(survival)#v.2.44-1.1
library(broom)
library(ggfortify)
library(patchwork)

#By Tank:
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

ggplot(data=td_tank_percent_Summary2, aes(x=as.factor(time), y=PercAlive, group=as.factor(Treatment), 
                                          Shape=as.factor(Treatment))) + #group=Treatment, color=Treatment#scale_colour_manual(values = cols)+scale_fill_manual(values = cols)+
  geom_point(aes(color=Treatment, shape=Treatment), size=7)+
  geom_line(aes(fill=Treatment),size=1)+#scale_shape_manual(values=c("C1"=16, "D1"=15, "SED" =8, "SS"=10))+
  facet_grid(.~Zoox)+
  scale_colour_manual(values = c("black", "black"))+
  geom_linerange(aes(ymin=PercAlive-se, ymax=PercAlive+se), size=1)+theme_bw()+theme(legend.position = "none")


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

my_comparisons_juv18 <- list( c("26", "32"))
td_tank_percent.seperate2$Family <- as.factor(td_tank_percent.seperate2$Family)
td_tank_percent.seperate2$Zoox <- as.factor(td_tank_percent.seperate2$Zoox)

td_tank_percent_Summary2_wilcom_crosses<-td_tank_percent.seperate2 %>% filter(time %in% c("2")) %>%
  ggboxplot(x = "Treatment", y = "PercAlive",group="Treatment",
            color = "Treatment",
            add = "jitter") +facet_grid(Zoox~Family)+stat_compare_means(method = "wilcox.test", aes(label=..p.adj..), label.y = c(29, 35, 40)) +
  scale_colour_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))+
  scale_fill_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))# Add pairwise comparisons 

my_comparisons_juv18 <- list( c("26", "32"))
td_tank_percent_Summary2_wilcom<-td_tank_percent.seperate2 %>% filter(time %in% c("2")) %>%
  ggboxplot(x = "Treatment", y = "PercAlive",group="Treatment",
            color = "Treatment",
            add = "jitter") +facet_grid(.~Zoox)+stat_compare_means(method = "wilcox.test") +
  scale_colour_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))+
  scale_fill_manual(values=c("#0E0A0E", "#BB8996", "#55C1E2", "#F6CCA0", "#616594"))

#Boxplots comparing percent survival of juveniles at the final timepoint (58 days) between each cross at 27 and 32°C. 
#Values above boxplots correspond to non-parametric Wilcox test. Note, no single or clumped juveniles settled for the SS treatment 
#at 27°C for BKxCU or 32°C for BKxLS, labelled as "/". Crosses and are labelled as "NA" are due to the Wilcox test  can also not able 
#to compute the test statistic for complete ties, i.e. all three replicates per treatment were the same at survival = 100%, both 27 
#and 32°C were 100% for CUxLS - D1, DRxCU - SED, BKxDR - SS)., 

#for Wilcox test with compelte ties: error will be :"Computation failed in `stat_compare_means()`: arguments imply differing number of rows: 0, 1 
#Computation failed in `stat_compare_means()`: argument "x" is missing, with no default = missind data


#18x18in
##***Final figure larva and juveniles----
Allcrosses_larvalsurv2018_CURATE27 + Juv_allcrosses_surv.colour +plot_layout(guides = "collect")

####(1)NEW analysis comparing life-stages across families ----

###read in data combined larvae and juvenile data. Made new column with life-stage names, made sure reef names matched between two dataset, filtered both to the final timepoint for each.
larv_juv_comparision.csv2<-read.csv("larv_juv_comparision_combo.csv", header=T, )

larv_juv_comparision.csv2<-larv_juv_comparision.csv2%>% drop_na(Perc_surv)

larv_juv_comparision.csv2$lifestage<-factor(larv_juv_comparision.csv2$lifestage, levels=c("larvae", "juvenile"))

larv_juvenile_comparison_final_wilcox<-larv_juv_comparision.csv2 %>%
  ggboxplot(x = "lifestage", y = "Perc_surv",group="Parentage",
            #color = "Parentage",
            add = "jitter") +facet_grid(Treatment.1~Parentage)+stat_compare_means(method = "wilcox.test",aes(label=..p.adj..), label.y = c(29, 35, 40)) 

##############################done with larval and juvenile comparisions

####(2)NEW analysis comparing number of moms and dads and survival----
#Pearson because want interval scale
# Use R2 instead of R, One of "pearson" (default),
cor_TotalMom<-ggscatter(larv_juv_comparision.csv2, x = "TotalMom", y = "Perc_surv", add = "reg.line") +
  stat_cor(label.y = 120, 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  stat_regline_equation(label.y = 130)+facet_grid(.~lifestage)

cor_TotalDad<-ggscatter(larv_juv_comparision.csv2, x = "TotalDad", y = "Perc_surv", add = "reg.line") +
  stat_cor(label.y = 120, 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+facet_grid(.~lifestage)+stat_regline_equation(label.y = 130)

cor_TotalMom + cor_TotalDad



