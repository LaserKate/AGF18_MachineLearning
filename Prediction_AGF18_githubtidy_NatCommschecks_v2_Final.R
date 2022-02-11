#R version [1] "C:/Users/kquigley/Documents/R/R-3.6.0/library"
#Tools, Global Options, General --> [64-bit] C:\Users\kquigley\Documents\R\R-3.6.0
#Install packages ----
library(rlang) #0.4.10
library(caret) #6.0-86
library(gbm) #2.1.5
library(rsample) #_0.0.6
library(dplyr) #_0.8.0.1
library(plotmo) #_3.5.7
library(pdp) #_0.7.0 
library(ggpubr) #_0.1.8
#sessionInfo() #check versions are the same

#Check your Random number generator ----
#make sure random and rounding number generator. These defaults changed after R (v.3.6.1)
RNGkind()
sessionInfo()
#RNGkind("Rounding")
RNGkind(sample.kind = "Rounding")

###
#use random values for training. Random search of hyperparameters
fitControl_rando <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10, 
  repeats = 10,
  search = "random")

#Download the larval data with satellite data attached appended----
larv6<-read.csv("larv6.csv")

#####################(1 Purebred larvae)####-----
#Subset the larval purebreds----
Raw_larvae2018_filt_temp.metrics5_pure<-larv6%>%
  filter(Parentage %in% c("BK x BK", "LS x LS","CU x CU","SB x SB","DR x DR"))

set.seed(88901)
Larv.pure_splits <- initial_split(Raw_larvae2018_filt_temp.metrics5_pure, .99, strata = "Perc_surv", na.rm=TRUE)
set.seed(88901)
Larv18.train.pure <- training(Larv.pure_splits)
set.seed(88901)
Larv18._test.pure <- testing(Larv.pure_splits)

caret_grid.larv.hypergrid_purer.testNatComms <- data.frame(n.trees = 388, interaction.depth = 3, 
                                                           shrinkage = 0.01,n.minobsinnode = 5)

#interaction depth from recomended 1 to 3 because plot squared error loss flattens out
larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom$results #check params

set.seed(25)
larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom <- 
  caret::train(Perc_surv ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.y + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT, method = "gbm", data = Larv18.train.pure, verbose = TRUE,
               tuneGrid = caret_grid.larv.hypergrid_purer.testNatComms, trControl = fitControl_rando, na.action=na.omit)

summary(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom$finalModel, plotit = TRUE)
plotres(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom)
plotmo(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom, pmethod="partdep", all1=TRUE, all2=TRUE) #testx object

###
hyper_gridjuv.LARVpure <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# total number of combinations
nrow(hyper_gridjuv.LARVpure)
## [1] 54

for(i in 1:nrow(hyper_gridjuv.LARVpure)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune.larv.pure <- gbm(
    formula = Perc_surv ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.y + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT,
    distribution = "gaussian",
    data = Larv18.train.pure,
    n.trees = 3000,
    interaction.depth = hyper_gridjuv.LARVpure$interaction.depth[i],
    shrinkage = hyper_gridjuv.LARVpure$shrinkage[i],
    n.minobsinnode = hyper_gridjuv.LARVpure$n.minobsinnode[i],
    bag.fraction = hyper_gridjuv.LARVpure$bag.fraction[i],
    train.fraction = .99,
    n.cores = NULL, # will use all cores by default
    verbose = TRUE
  )
  
  # add min training error and trees to grid
  hyper_gridjuv.LARVpure$optimal_trees[i] <- which.min(gbm.tune.larv.pure$valid.error)
  hyper_gridjuv.LARVpure$min_RMSE[i] <- sqrt(min(gbm.tune.larv.pure$valid.error))
}

#the new values will now be added to the formally empty rows of the hyper_grid

head(hyper_gridjuv.LARVpure)

hyper_gridjuv.LARVpure %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)

##Purebred larval prediction----
x2<-larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom %>% partial(pred.var = "TSA_DHWStandardDeviation") %>% autoplot(smooth = TRUE, smooth.method="auto", smooth.span= 0.5, ylab = expression(f(TSA_DHWStandardDeviation))) + theme_light() + ggtitle("ggplot2-based PDP")
x.larvpur<-ggplot_build(x2)$data[[2]]$x
y.larvpur<-ggplot_build(x2)$data[[2]]$y
TSAbuild<-data.frame(a=ggplot_build(x2)$data[[2]]$x,b=ggplot_build(x2)$data[[2]]$y)
#At this point I used write.csv to write out the data frame above and fit the polynomial in excel
#write.csv(TSAbuild, "TSAbuild_NatComms_v2final.csv")
formula.l.p <- y ~ poly(x, 3) 
larv.pure.finalTSA<-ggplot(TSAbuild, aes(a, b)) +
  geom_point() +
  stat_smooth(aes(), method = "lm", formula = formula.l.p) +theme_bw()

################
R2.1<-larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom$resample$Rsquared
boxplot(R2.1)
mean(R2.1)
std.error(R2.1)
dim(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom.NoCU$resample)
#fitControl_rando 10 x10 = 100 CV fold repeats.


#####################(2 Hybrid larvae)####-----
Raw_larvae2018_filt_temp.metrics5_hybrids<-larv6%>%
  filter(Parentage %in% c("BK x LS", "BK x CU", "BK x SB", "BK x DR", 
                          "DR x LS", "DR x CU", "DR x SB", "DR x BK",
                          "CU x LS", "CU x BK", "CU x SB", "CU x DR",
                          "LS x BK", "LS x CU", "LS x SB", "LS x DR",
                          "SB x LS", "SB x CU", "SB x BK", "SB x DR"))

set.seed(88901)
Larv.hyb_splits <- initial_split(Raw_larvae2018_filt_temp.metrics5_hybrids, .99, strata = "Perc_surv", na.rm=TRUE) 
set.seed(88901)
Larv18.train.hyb <- training(Larv.hyb_splits)
set.seed(88901)
Larv18._test.hyb <- testing(Larv.hyb_splits)

#**
caret_grid.larv.hypergrid_hybr <- data.frame(n.trees = 100, interaction.depth = 1,
                                             shrinkage = 0.3,
                                             n.minobsinnode = 5)
set.seed(25)
larv.fit_gbm_caret.cv_HYB.hyperTune <- 
  caret::train(Perc_surv ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.y + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation + DRT, method = "gbm", data = Larv18.train.hyb, verbose = TRUE,
               tuneGrid = caret_grid.larv.hypergrid_hybr, trControl = fitControl_rando)
summary(larv.fit_gbm_caret.cv_HYB.hyperTune$finalModel, plotit = TRUE)
plotres(larv.fit_gbm_caret.cv_HYB.hyperTune)
plotmo(larv.fit_gbm_caret.cv_HYB.hyperTune, pmethod="partdep", all1=TRUE, all2=TRUE) #testx object

hyper_gridjuv.LARVhyb <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# total number of combinations
nrow(hyper_gridjuv.LARVhyb)
## [1] 54

for(i in 1:nrow(hyper_gridjuv.LARVhyb)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune.larv.hyb <- gbm(
    formula = Perc_surv ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.y + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation + DRT,
    distribution = "gaussian",
    data = Larv18.train.hyb, 
    n.trees = 3000,
    interaction.depth = hyper_gridjuv.LARVhyb$interaction.depth[i],
    shrinkage = hyper_gridjuv.LARVhyb$shrinkage[i],
    n.minobsinnode = hyper_gridjuv.LARVhyb$n.minobsinnode[i],
    bag.fraction = hyper_gridjuv.LARVhyb$bag.fraction[i],
    train.fraction = .99,
    n.cores = NULL, # will use all cores by default
    verbose = TRUE
  )
  
  # add min training error and trees to grid
  hyper_gridjuv.LARVhyb$optimal_trees[i] <- which.min(gbm.tune.larv.hyb$valid.error)
  hyper_gridjuv.LARVhyb$min_RMSE[i] <- sqrt(min(gbm.tune.larv.hyb$valid.error))
}

#the new values will now be added to the formally empty rows of the hyper_grid

head(hyper_gridjuv.LARVhyb)

hyper_gridjuv.LARVhyb %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)

####Hybrid larval prediction----
larv.hybDRT.1<-larv.fit_gbm_caret.cv_HYB.hyperTune %>% partial(pred.var = "DRT") %>% autoplot(smooth = TRUE, smooth.method="auto", smooth.span= 0.5, ylab = expression(f(DRT))) + theme_light() + ggtitle("ggplot2-based PDP") #library pdp
x.larvDRT<-ggplot_build(larv.hybDRT.1)$data[[2]]$x
y.larvDRT<-ggplot_build(larv.hybDRT.1)$data[[2]]$y
DRT.larvbuild<-data.frame(a2=ggplot_build(larv.hybDRT.1)$data[[2]]$x,b2=ggplot_build(larv.hybDRT.1)$data[[2]]$y)
#DRT.larvbuild$category <- "larvhyb"

#write.csv as explained before
formula.drt.larv <- y ~ poly(x, 5, raw = TRUE)
larv.hyb.DRT_small<-ggplot(DRT.larvbuild, aes(a2, b2)) +
  geom_point() +theme_bw()+
  stat_smooth(aes(), method = "lm", formula = formula.drt.larv) 


R2.2<-larv.fit_gbm_caret.cv_HYB.hyperTune$resample$Rsquared
boxplot(R2.2)
mean(R2.2)
std.error(R2.2)
dim(larv.fit_gbm_caret.cv_HYB.hyperTune$resample)
#fitControl_rando 10 x10 = 100 CV fold repeats.



#####################(3 Purebred D1)####-----
juv6.6_subset<-read.csv("juv6.6_subset.csv")
Raw_juv2018_filt_temp.metrics5_D1<-juv6.6_subset %>% filter(Zoox %in% c("D1"))

Raw_juv2018_filt_temp.metrics5_D1_pure<-Raw_juv2018_filt_temp.metrics5_D1 %>% 
  filter(Family %in% c(" BKxBK   ", " LSxLS   "," CUxCU   "," SBxSB   "," DRxDR   "))

###Summary 
set.seed(88901)
JuvD1.pure_splits <- initial_split(Raw_juv2018_filt_temp.metrics5_D1_pure, .99, strata = "PercAlive", na.rm=TRUE) 
set.seed(88901)
Juv18.D1_train.pure <- training(JuvD1.pure_splits)
set.seed(88901)
Juv18.D1_test.pure <- testing(JuvD1.pure_splits)

caret_grid.JUV.hypergrid.D1pure <- data.frame(n.trees = 30, 
                                              interaction.depth = 1, 
                                              shrinkage = 0.3,
                                             n.minobsinnode = 10)

set.seed(25)
larv.fit_gbm_caret.cv_D1pure.hyperTune <- 
  caret::train(PercAlive ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.x + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT, method = "gbm", data = Juv18.D1_train.pure, verbose = TRUE,
               tuneGrid = caret_grid.JUV.hypergrid.D1pure, trControl = fitControl_rando, na.action=na.omit)
plotres(larv.fit_gbm_caret.cv_D1pure.hyperTune)
summary(larv.fit_gbm_caret.cv_D1pure.hyperTune$finalModel, plotit = F)
plotmo(larv.fit_gbm_caret.cv_D1pure.hyperTune, pmethod="partdep", all1=TRUE, all2=TRUE)

####
hyper_gridjuv.D1pure <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.8, 1),
  optimal_trees = 0,               
  min_RMSE = 0                     
)

# total number of combinations
nrow(hyper_gridjuv.D1pure)
## [1] 54

for(i in 1:nrow(hyper_gridjuv.D1pure)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune.juvs.D1pure <- gbm(
    formula = PercAlive ~ ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.x + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT,
    distribution = "gaussian",
    data = Juv18.D1_train.pure,
    n.trees = 3000,
    interaction.depth = hyper_gridjuv.D1pure$interaction.depth[i],
    shrinkage = hyper_gridjuv.D1pure$shrinkage[i],
    n.minobsinnode = hyper_gridjuv.D1pure$n.minobsinnode[i],
    bag.fraction = hyper_gridjuv.D1pure$bag.fraction[i],
    train.fraction = .99,
    n.cores = NULL, # will use all cores by default
    verbose = TRUE
  )
  
  # add min training error and trees to grid
  hyper_gridjuv.D1pure$optimal_trees[i] <- which.min(gbm.tune.juvs.D1pure$valid.error)
  hyper_gridjuv.D1pure$min_RMSE[i] <- sqrt(min(gbm.tune.juvs.D1pure$valid.error))
}

#the new values will now be added to the formally empty rows of the hyper_grid

head(hyper_gridjuv.D1pure)

hyper_gridjuv.D1pure %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)

##Predict D1 purebreds----
juv.pure.D1_ts_1<-larv.fit_gbm_caret.cv_D1pure.hyperTune %>% partial(pred.var = "ts_frame...2.dim.ts_frame..2...x.") %>% autoplot(smooth = TRUE, smooth.method="auto", smooth.span= 0.5, ylab = expression(f(ts_))) + theme_light() + ggtitle("ggplot2-based PDP")
x.juv.D1.ts_<-ggplot_build(juv.pure.D1_ts_1)$data[[2]]$x
y.juv.D1.ts_<-ggplot_build(juv.pure.D1_ts_1)$data[[2]]$y
ts_.JuvD1build<-data.frame(a3=x.juv.D1.ts_,b3=y.juv.D1.ts_)

#write.csv
formula_Juv_ts <- y ~ poly(x, 3, raw = TRUE)
Juv_D1.purets_small<-ggplot(ts_.JuvD1build, aes(a3, b3)) +
  geom_point() +theme_bw()+
  stat_smooth(aes(), method = "lm", formula = formula_Juv_ts)

R2.3<-larv.fit_gbm_caret.cv_D1pure.hyperTune$resample$Rsquared
boxplot(R2.3)
mean(R2.3)
std.error(R2.3)


#####################(4 Hybrid D1)####-----
Raw_juv2018_filt_temp.metrics5_D1_hyb<-Raw_juv2018_filt_temp.metrics5_D1 %>% 
  filter(Family %in% c(" BKxCU   ", " BKxDR   ", " BKxLS   ", " BKxSB   ", " CUxBK   ", " CUxDR   ", 
                       " CuxLS   ", " CUxSB   ", " DRxBK   ", " DRxCU   ", " DRxSB   ", " LSxBK   ", 
                       " LSxCU   ", " LSxSB   ", " SBxBK   ", " SBxDR   ", " SBxLS   ", " CUxLS   "))

set.seed(88901)
JuvD1.hyb_splits <- initial_split(Raw_juv2018_filt_temp.metrics5_D1_hyb, .99, strata = "PercAlive", na.rm=TRUE) 
set.seed(88901)
Juv18.D1_train.hyb <- training(JuvD1.hyb_splits)
set.seed(88901)
Juv18.D1_test.hyb <- testing(JuvD1.hyb_splits)

caret_grid.JUV.hypergrid_D1hyb <- data.frame(n.trees = 50, interaction.depth = 1, shrinkage = 0.3,
                                             n.minobsinnode = 10)
set.seed(25)
larv.fit_gbm_caret.cv_D1hyb.hyperTune <- 
  caret::train(PercAlive ~ ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.x + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation + DRT, method = "gbm", data = Juv18.D1_train.hyb, verbose = TRUE,
               tuneGrid = caret_grid.JUV.hypergrid_D1hyb, trControl = fitControl_rando, na.action=na.omit)
summary(larv.fit_gbm_caret.cv_D1hyb.hyperTune$finalModel, plotit = TRUE)
plotres(larv.fit_gbm_caret.cv_D1hyb.hyperTune) 
plotmo(larv.fit_gbm_caret.cv_D1hyb.hyperTune, pmethod="partdep", all1=TRUE, all2=TRUE)

hyper_gridjuv.D1hyb <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# total number of combinations
nrow(hyper_gridjuv.D1hyb)
## [1] 54

for(i in 1:nrow(hyper_gridjuv.D1hyb)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune.juvs.D1hyb <- gbm(
    formula = PercAlive ~ ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.x + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT,
    distribution = "gaussian",
    data = Juv18.D1_train.hyb, 
    n.trees = 3000,
    interaction.depth = hyper_gridjuv.D1hyb$interaction.depth[i],
    shrinkage = hyper_gridjuv.D1hyb$shrinkage[i],
    n.minobsinnode = hyper_gridjuv.D1hyb$n.minobsinnode[i],
    bag.fraction = hyper_gridjuv.D1hyb$bag.fraction[i],
    train.fraction = .99,
    n.cores = NULL, # will use all cores by default
    verbose = TRUE
  )
  
  # add min training error and trees to grid
  hyper_gridjuv.D1hyb$optimal_trees[i] <- which.min(gbm.tune.juvs.D1hyb$valid.error)
  hyper_gridjuv.D1hyb$min_RMSE[i] <- sqrt(min(gbm.tune.juvs.D1hyb$valid.error))
}

#the new values will now be added to the formally empty rows of the hyper_grid

head(hyper_gridjuv.D1hyb)

hyper_gridjuv.D1hyb %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)

#Predict D1 hybrids
head(larv.fit_gbm_caret.cv_D1hyb.hyperTune$trainingData$.outcome)
gbm.model.outputs.Juv.D1.hyb.TSA<-larv.fit_gbm_caret.cv_D1hyb.hyperTune$trainingData

predict.gbm.Juv.D1.hyb.TSA<-ggscatter(gbm.model.outputs.Juv.D1.hyb.TSA, x = "TSA_DHWStandardDeviation", y = ".outcome",
                                      add = "loess", conf.int = TRUE)+stat_regline_equation(label.y = 35)+labs(x="predicted gbm juv D1 hyb TSA", y=expression("Percent Survival"*~degree*C))+
  stat_cor(method = "spearman")

###D1 predict juveniles hyb----
juv.hyb.D1_ts_1<-larv.fit_gbm_caret.cv_D1hyb.hyperTune %>% partial(pred.var = "TSA_DHWStandardDeviation") %>% autoplot(smooth = TRUE, smooth.method="auto", smooth.span= 0.5, ylab = expression(f(TSA))) + theme_light() + ggtitle("ggplot2-based PDP")
x.juv.D1.hyb_TSA<-ggplot_build(juv.hyb.D1_ts_1)$data[[2]]$x
y.juv.D1.hyb_TSA<-ggplot_build(juv.hyb.D1_ts_1)$data[[2]]$y
TSA.JuvD1.hybuild<-data.frame(a4=x.juv.D1.hyb_TSA,b4=y.juv.D1.hyb_TSA)

#write.csv
formula.TSA <- y ~ poly(x, 3, raw = TRUE)
JuvD1.hyb.finalTSA<-ggplot(TSA.JuvD1.hybuild, aes(a4, b4)) +
  geom_point() +theme_bw()+
  stat_smooth(aes(), method = "lm", formula = formula.TSA)

R2.4<-larv.fit_gbm_caret.cv_D1hyb.hyperTune$resample$Rsquared
boxplot(R2.4)
mean(R2.4)
std.error(R2.4)

#####################(5 Purebred C1)####-----

Raw_juv2018_filt_temp.metrics5_C1<-juv6.6_subset %>% filter(Zoox %in% c("C1")) 

Raw_juv2018_filt_temp.metrics5_C1_pure<-Raw_juv2018_filt_temp.metrics5_C1 %>% 
  filter(Family %in% c(" BKxBK   ", " LSxLS   "," CUxCU   "," SBxSB   "," DRxDR   "))

set.seed(88901)
JuvC1.pure_splits <- initial_split(Raw_juv2018_filt_temp.metrics5_C1_pure, .99, strata = "PercAlive", na.rm=TRUE) 
set.seed(88901)
Juv18.C1_train.pure <- training(JuvC1.pure_splits)
set.seed(88901)
Juv18.C1_test.pure <- testing(JuvC1.pure_splits)

caret_grid.JUV.hypergrid <- data.frame(n.trees = 600, interaction.depth = 5, 
                                       shrinkage = 0.01,
                                       n.minobsinnode = 10) 
set.seed(25)
larv.fit_gbm_caret.cv_C1pure.hyperTune <- 
  caret::train(PercAlive ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.x + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation + DRT, method = "gbm", data = Juv18.C1_train.pure, verbose = TRUE,
               tuneGrid = caret_grid.JUV.hypergrid, trControl = fitControl_rando, na.action=na.omit)


#diagnostics
plotres(larv.fit_gbm_caret.cv_C1pure.hyperTune)
summary(larv.fit_gbm_caret.cv_C1pure.hyperTune$finalModel, plotit = F)

####
hyper_gridjuv <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# total number of combinations
nrow(hyper_gridjuv)
## [1] 54

for(i in 1:nrow(hyper_gridjuv)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune.juvs <- gbm(
    formula = PercAlive ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.x + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation + DRT,
    distribution = "gaussian",
    data = Juv18.C1_train.pure, 
    n.trees = 3000,
    interaction.depth = hyper_gridjuv$interaction.depth[i],
    shrinkage = hyper_gridjuv$shrinkage[i],
    n.minobsinnode = hyper_gridjuv$n.minobsinnode[i],
    bag.fraction = hyper_gridjuv$bag.fraction[i],
    train.fraction = .99,
    n.cores = NULL, # will use all cores by default
    verbose = TRUE
  )
  
  # add min training error and trees to grid
  hyper_gridjuv$optimal_trees[i] <- which.min(gbm.tune.juvs$valid.error)
  hyper_gridjuv$min_RMSE[i] <- sqrt(min(gbm.tune.juvs$valid.error))
}

#the new values will now be added to the formally empty rows of the hyper_grid

head(hyper_gridjuv)

hyper_gridjuv %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)

R2.5<-larv.fit_gbm_caret.cv_C1pure.hyperTune$resample$Rsquared
boxplot(R2.5)
mean(R2.5)
std.error(R2.5)

#####################(6 Hybrid C1)####-----
Raw_juv2018_filt_temp.metrics5_C1_hyb<-Raw_juv2018_filt_temp.metrics5_C1 %>% 
  filter(Family %in% c(" BKxCU   ", " BKxDR   ", " BKxLS   ", " BKxSB   ", " CUxBK   ", " CUxDR   ", 
                       " CuxLS   ", " CUxSB   ", " DRxBK   ", " DRxCU   ", " DRxSB   ", " LSxBK   ", 
                       " LSxCU   ", " LSxSB   ", " SBxBK   ", " SBxDR   ", " SBxLS   ", " CUxLS   "))

# split
set.seed(88901)
JuvC1.hyb_splits <- initial_split(Raw_juv2018_filt_temp.metrics5_C1_hyb, .99, strata = "PercAlive", na.rm=TRUE) 
set.seed(88901)
Juv18.C1_train.hyb <- training(JuvC1.hyb_splits)
set.seed(88901)
Juv18.C1_test.hyb <- testing(JuvC1.hyb_splits)

caret_grid.JUV.hypergrid_C1hyb <- data.frame(n.trees = 50, interaction.depth = 1, shrinkage = 0.3,
                                             n.minobsinnode = 10)

set.seed(25)
larv.fit_gbm_caret.cv_C1hyb.hyperTune <- 
  caret::train(PercAlive ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.x + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT, method = "gbm", data = Juv18.C1_train.hyb, verbose = TRUE,
               tuneGrid = caret_grid.JUV.hypergrid_C1hyb, trControl = fitControl_rando, na.action=na.omit)

plotres(larv.fit_gbm_caret.cv_C1hyb.hyperTune)
summary(larv.fit_gbm_caret.cv_C1hyb.hyperTune$finalModel, plotit = TRUE)


hyper_gridjuv.C1hyb <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# total number of combinations
nrow(hyper_gridjuv.C1hyb)
## [1] 54

for(i in 1:nrow(hyper_gridjuv.C1hyb)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune.juvs.C1hyb <- gbm(
    formula = PercAlive ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.x + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT,
    distribution = "gaussian",
    data = Juv18.C1_train.hyb, #Raw_juv2018_filt_temp.metrics5_C1_hyb,
    n.trees = 3000,
    interaction.depth = hyper_gridjuv.C1hyb$interaction.depth[i],
    shrinkage = hyper_gridjuv.C1hyb$shrinkage[i],
    n.minobsinnode = hyper_gridjuv.C1hyb$n.minobsinnode[i],
    bag.fraction = hyper_gridjuv.C1hyb$bag.fraction[i],
    train.fraction = .99,
    n.cores = NULL, # will use all cores by default
    verbose = TRUE
  )
  
  # add min training error and trees to grid
  hyper_gridjuv.C1hyb$optimal_trees[i] <- which.min(gbm.tune.juvs.C1hyb$valid.error)
  hyper_gridjuv.C1hyb$min_RMSE[i] <- sqrt(min(gbm.tune.juvs.C1hyb$valid.error))
}

#the new values will now be added to the formally empty rows of the hyper_grid

head(hyper_gridjuv.C1hyb)

hyper_gridjuv.C1hyb %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)

R2.6<-larv.fit_gbm_caret.cv_C1hyb.hyperTune$resample$Rsquared
boxplot(R2.6)
mean(R2.6)
std.error(R2.6)

#####################(7 Purebred SS)####-----
Raw_juv2018_filt_temp.metrics5_SS<-juv6.6_subset %>% filter(Zoox %in% c("SS"))

Raw_juv2018_filt_temp.metrics5_SS_pure<-Raw_juv2018_filt_temp.metrics5_SS %>% 
  filter(Family %in% c(" BKxBK   ", " LSxLS   "," CUxCU   "," SBxSB   "," DRxDR   "))

set.seed(88901)
JuvSS.pure_splits <- initial_split(Raw_juv2018_filt_temp.metrics5_SS_pure, .99, strata = "PercAlive", na.rm=TRUE) 
set.seed(88901)
Juv18.SS_train.pure <- training(JuvSS.pure_splits)
set.seed(88901)
Juv18.SS_test.pure <- testing(JuvSS.pure_splits)

caret_grid.JUV.hypergrid.SSpure <- data.frame(n.trees = 30, interaction.depth = 1, shrinkage = 0.3,
                                              n.minobsinnode = 15)
set.seed(25)
larv.fit_gbm_caret.cv_SSpure.hyperTune <- 
  caret::train(PercAlive ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.x + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT, method = "gbm", data = Juv18.SS_train.pure, verbose = TRUE,
               tuneGrid = caret_grid.JUV.hypergrid.SSpure, trControl = fitControl_rando, na.action=na.omit)


plotres(larv.fit_gbm_caret.cv_SSpure.hyperTune)
summary(larv.fit_gbm_caret.cv_SSpure.hyperTune$finalModel, plotit = TRUE)

hyper_gridjuv.SSpure <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# total number of combinations
nrow(hyper_gridjuv.SSpure)
## [1] 54

for(i in 1:nrow(hyper_gridjuv.SSpure)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune.juvs.SSpure <- gbm(
    formula = PercAlive ~ ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.x + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT,
    distribution = "gaussian",
    data = Juv18.SS_train.pure,  #Raw_juv2018_filt_temp.metrics5_SS_pure,
    n.trees = 3000,
    interaction.depth = hyper_gridjuv.SSpure$interaction.depth[i],
    shrinkage = hyper_gridjuv.SSpure$shrinkage[i],
    n.minobsinnode = hyper_gridjuv.SSpure$n.minobsinnode[i],
    bag.fraction = hyper_gridjuv.SSpure$bag.fraction[i],
    train.fraction = .99,
    n.cores = NULL, # will use all cores by default
    verbose = TRUE
  )
  
  # add min training error and trees to grid
  hyper_gridjuv.SSpure$optimal_trees[i] <- which.min(gbm.tune.juvs.SSpure$valid.error)
  hyper_gridjuv.SSpure$min_RMSE[i] <- sqrt(min(gbm.tune.juvs.SSpure$valid.error))
}

#the new values will now be added to the formally empty rows of the hyper_grid

head(hyper_gridjuv.SSpure)

hyper_gridjuv.SSpure %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)

R2.7<-larv.fit_gbm_caret.cv_SSpure.hyperTune$resample$Rsquared
boxplot(R2.7)
mean(R2.7)
std.error(R2.7)

#####################(8 Hybrid SS)####-----
Raw_juv2018_filt_temp.metrics5_SS_hyb<-Raw_juv2018_filt_temp.metrics5_SS %>% 
  filter(Family %in% c(" BKxCU   ", " BKxDR   ", " BKxLS   ", " BKxSB   ", " CUxBK   ", " CUxDR   ", 
                       " CuxLS   ", " CUxSB   ", " DRxBK   ", " DRxCU   ", " DRxSB   ", " LSxBK   ", 
                       " LSxCU   ", " LSxSB   ", " SBxBK   ", " SBxDR   ", " SBxLS   ", " CUxLS   "))

set.seed(88901)
JuvSS.hyb_splits <- initial_split(Raw_juv2018_filt_temp.metrics5_SS_hyb, .99, strata = "PercAlive", na.rm=TRUE) 
set.seed(88901)
Juv18.SS_train.hyb <- training(JuvSS.hyb_splits)
set.seed(88901)
Juv18.SS_test.hyb <- testing(JuvSS.hyb_splits)

caret_grid.JUV.hypergrid_SShyb <- data.frame(n.trees = 50, interaction.depth = 1, shrinkage = 0.3,
                                             n.minobsinnode = 5)
set.seed(25)
larv.fit_gbm_caret.cv_SShyb.hyperTune <- 
  caret::train(PercAlive ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.x + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT, method = "gbm", data = Juv18.SS_train.hyb, verbose = TRUE,tuneGrid = caret_grid.JUV.hypergrid_SShyb, trControl = fitControl_rando,
               distribution="gaussian", na.action=na.omit) 

plotres(larv.fit_gbm_caret.cv_SShyb.hyperTune) 
summary(larv.fit_gbm_caret.cv_SShyb.hyperTune$finalModel, plotit = TRUE)

hyper_gridjuv.SShyb <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# total number of combinations
nrow(hyper_gridjuv.SShyb)
## [1] 54

for(i in 1:nrow(hyper_gridjuv.SShyb)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune.juvs.SShyb <- gbm(
    formula = PercAlive ~ ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.x + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT,
    distribution = "gaussian",
    data = Juv18.SS_train.hyb, 
    n.trees = 3000,
    interaction.depth = hyper_gridjuv.SShyb$interaction.depth[i],
    shrinkage = hyper_gridjuv.SShyb$shrinkage[i],
    n.minobsinnode = hyper_gridjuv.SShyb$n.minobsinnode[i],
    bag.fraction = hyper_gridjuv.SShyb$bag.fraction[i],
    train.fraction = .99,
    n.cores = NULL, # will use all cores by default
    verbose = TRUE
  )
  
  # add min training error and trees to grid
  hyper_gridjuv.SShyb$optimal_trees[i] <- which.min(gbm.tune.juvs.SShyb$valid.error)
  hyper_gridjuv.SShyb$min_RMSE[i] <- sqrt(min(gbm.tune.juvs.SShyb$valid.error))
}

#the new values will now be added to the formally empty rows of the hyper_grid

head(hyper_gridjuv.SShyb)

hyper_gridjuv.SShyb %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)

R2.8<-larv.fit_gbm_caret.cv_SShyb.hyperTune$resample$Rsquared
boxplot(R2.8)
mean(R2.8)
std.error(R2.8)

#####################(9 Purebred SED)####-----
Raw_juv2018_filt_temp.metrics5_SED<-juv6.6_subset %>% filter(Zoox %in% c("SED"))

Raw_juv2018_filt_temp.metrics5_SED_pure<-Raw_juv2018_filt_temp.metrics5_SED %>% 
  filter(Family %in% c(" BKxBK   ", " LSxLS   "," CUxCU   "," SBxSB   "," DRxDR   "))

set.seed(88901)
JuvSED.pure_splits <- initial_split(Raw_juv2018_filt_temp.metrics5_SED_pure, .99, strata = "PercAlive", na.rm=TRUE) 
set.seed(88901)
Juv18.SED_train.pure <- training(JuvSED.pure_splits)
set.seed(88901)
Juv18.SED_test.pure <- testing(JuvSED.pure_splits)

caret_grid.JUV.hypergrid.SEDpure <- data.frame(n.trees = 30, interaction.depth = 1, shrinkage = 0.3,
                                               n.minobsinnode = 15)
set.seed(25)
larv.fit_gbm_caret.cv_SEDpure.hyperTune <- 
  caret::train(PercAlive ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.x + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT, method = "gbm", data = Juv18.SED_train.pure, verbose = TRUE,
               tuneGrid = caret_grid.JUV.hypergrid.SEDpure, trControl = fitControl_rando, na.action=na.omit)

plotres(larv.fit_gbm_caret.cv_SEDpure.hyperTune)
summary(larv.fit_gbm_caret.cv_SEDpure.hyperTune$finalModel, plotit = TRUE)

hyper_gridjuv.SEDpure <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# total number of combinations
nrow(hyper_gridjuv.SEDpure)
## [1] 54

for(i in 1:nrow(hyper_gridjuv.SEDpure)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune.juvs.SEDpure <- gbm(
    formula = PercAlive ~ ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.x + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT,
    distribution = "gaussian",
    data = Juv18.SED_train.pure, #Raw_juv2018_filt_temp.metrics5_SED_pure,
    n.trees = 3000,
    interaction.depth = hyper_gridjuv.SEDpure$interaction.depth[i],
    shrinkage = hyper_gridjuv.SEDpure$shrinkage[i],
    n.minobsinnode = hyper_gridjuv.SEDpure$n.minobsinnode[i],
    bag.fraction = hyper_gridjuv.SEDpure$bag.fraction[i],
    train.fraction = .99,
    n.cores = NULL, # will use all cores by default
    verbose = TRUE
  )
  
  # add min training error and trees to grid
  hyper_gridjuv.SEDpure$optimal_trees[i] <- which.min(gbm.tune.juvs.SEDpure$valid.error)
  hyper_gridjuv.SEDpure$min_RMSE[i] <- sqrt(min(gbm.tune.juvs.SEDpure$valid.error))
}

#the new values will now be added to the formally empty rows of the hyper_grid

head(hyper_gridjuv.SEDpure)

hyper_gridjuv.SEDpure %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)

R2.9<-larv.fit_gbm_caret.cv_SEDpure.hyperTune$resample$Rsquared
boxplot(R2.9)
mean(R2.9)
std.error(R2.9)


#####################(10 Hybrid SED)####-----
Raw_juv2018_filt_temp.metrics5_SED_hyb<-Raw_juv2018_filt_temp.metrics5_SED %>% 
  filter(Family %in% c(" BKxCU   ", " BKxDR   ", " BKxLS   ", " BKxSB   ", " CUxBK   ", " CUxDR   ", 
                       " CuxLS   ", " CUxSB   ", " DRxBK   ", " DRxCU   ", " DRxSB   ", " LSxBK   ", 
                       " LSxCU   ", " LSxSB   ", " SBxBK   ", " SBxDR   ", " SBxLS   ", " CUxLS   "))
set.seed(88901)
JuvSED.hyb_splits <- initial_split(Raw_juv2018_filt_temp.metrics5_SED_hyb, .99, strata = "PercAlive", na.rm=TRUE) 
set.seed(88901)
Juv18.SED_train.hyb <- training(JuvSED.hyb_splits)
set.seed(88901)
Juv18.SED_test.hyb <- testing(JuvSED.hyb_splits)

caret_grid.JUV.hypergrid_SEDhyb <- data.frame(n.trees = 50, interaction.depth = 1, shrinkage = 0.3,
                                              n.minobsinnode = 15)
set.seed(25)
larv.fit_gbm_caret.cv_SEDhyb.hyperTune <- 
  caret::train(PercAlive ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.x + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT, method = "gbm", data = Juv18.SED_train.hyb, verbose = TRUE,tuneGrid = caret_grid.JUV.hypergrid_SEDhyb, trControl = fitControl_rando,
               distribution="gaussian", na.action=na.omit) 

plotres(larv.fit_gbm_caret.cv_SEDhyb.hyperTune) #
summary(larv.fit_gbm_caret.cv_SEDhyb.hyperTune$finalModel, plotit = TRUE)

hyper_gridjuv.SEDhyb <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# total number of combinations
nrow(hyper_gridjuv.SEDhyb)
## [1] 54

for(i in 1:nrow(hyper_gridjuv.SEDhyb)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune.juvs.SEDhyb <- gbm(
    formula = PercAlive ~ ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.x + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT,
    distribution = "gaussian",
    data = Juv18.SED_train.hyb,
    n.trees = 3000,
    interaction.depth = hyper_gridjuv.SEDhyb$interaction.depth[i],
    shrinkage = hyper_gridjuv.SEDhyb$shrinkage[i],
    n.minobsinnode = hyper_gridjuv.SEDhyb$n.minobsinnode[i],
    bag.fraction = hyper_gridjuv.SEDhyb$bag.fraction[i],
    train.fraction = .99,
    n.cores = NULL, # will use all cores by default
    verbose = TRUE
  )
  
  # add min training error and trees to grid
  hyper_gridjuv.SEDhyb$optimal_trees[i] <- which.min(gbm.tune.juvs.SEDhyb$valid.error)
  hyper_gridjuv.SEDhyb$min_RMSE[i] <- sqrt(min(gbm.tune.juvs.SEDhyb$valid.error))
}

#the new values will now be added to the formally empty rows of the hyper_grid

head(hyper_gridjuv.SEDhyb)

hyper_gridjuv.SEDhyb %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)

R2.10<-larv.fit_gbm_caret.cv_SEDhyb.hyperTune$resample$Rsquared
boxplot(R2.10)
mean(R2.10)
std.error(R2.10)













#######################################################################################
#####################(1.5 test cross validation procedure)####-----
##larv pure no CU----
Raw_larvae2018_filt_temp.metrics5_pure_noCU<-larv6%>%
  filter(Parentage %in% c("BK x BK", "LS x LS","SB x SB","DR x DR"))


set.seed(88901)
Larv.pure_splits_noCU <- initial_split(Raw_larvae2018_filt_temp.metrics5_pure_noCU, .99, strata = "Perc_surv", na.rm=TRUE)
set.seed(88901)
Larv18.train.pure_noCU <- training(Larv.pure_splits_noCU)
set.seed(88901)
Larv18._test.pure_noCU <- testing(Larv.pure_splits_noCU)


# total number of combinations
nrow(hyper_gridjuv.LARVpure)
## [1] 54

for(i in 1:nrow(hyper_gridjuv.LARVpure)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune.larv.pure_noCU <- gbm(
    formula = Perc_surv ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.y + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT,
    distribution = "gaussian",
    data = Larv18.train.pure_noCU,
    n.trees = 3000,
    interaction.depth = hyper_gridjuv.LARVpure$interaction.depth[i],
    shrinkage = hyper_gridjuv.LARVpure$shrinkage[i],
    n.minobsinnode = hyper_gridjuv.LARVpure$n.minobsinnode[i],
    bag.fraction = hyper_gridjuv.LARVpure$bag.fraction[i],
    train.fraction = .99,
    n.cores = NULL, # will use all cores by default
    verbose = TRUE
  )
  
  # add min training error and trees to grid
  hyper_gridjuv.LARVpure$optimal_trees[i] <- which.min(gbm.tune.larv.pure_noCU$valid.error)
  hyper_gridjuv.LARVpure$min_RMSE[i] <- sqrt(min(gbm.tune.larv.pure_noCU$valid.error))
}

#the new values will now be added to the formally empty rows of the hyper_grid

head(hyper_gridjuv.LARVpure)

hyper_gridjuv.LARVpure %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)


caret_grid.larv.hypergrid_purer.testNatComms_noCUsame <- data.frame(n.trees = 388, interaction.depth = 3, 
                                                                    shrinkage = 0.01,n.minobsinnode = 5)

caret_grid.larv.hypergrid_purer.testNatComms_noCUmodif <- data.frame(n.trees = 38, interaction.depth = 3, 
                                                                     shrinkage = 0.3,n.minobsinnode = 5)

set.seed(25)
larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom.NoCU <- 
  caret::train(Perc_surv ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.y + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT, method = "gbm", data = Larv18.train.pure_noCU, verbose = TRUE,
               tuneGrid = caret_grid.larv.hypergrid_purer.testNatComms_noCUmodif, trControl = fitControl_rando, na.action=na.omit)

#interaction depth from recomended 1 to 3 because plot squared error loss flattens out
larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom.NoCU$results #check params

plotres(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom.NoCU)


summary(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom.NoCU$finalModel, plotit = TRUE)
plotmo(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom.NoCU, pmethod="partdep", all1=TRUE, all2=TRUE) #testx object

###################### larva pure no bk----
Raw_larvae2018_filt_temp.metrics5_pure_noBK<-larv6%>%
  filter(Parentage %in% c("CU x CU", "LS x LS","SB x SB","DR x DR"))


set.seed(88901)
Larv.pure_splits_noBK <- initial_split(Raw_larvae2018_filt_temp.metrics5_pure_noBK, .99, strata = "Perc_surv", na.rm=TRUE)
set.seed(88901)
Larv18.train.pure_noBK <- training(Larv.pure_splits_noBK)
set.seed(88901)
Larv18._test.pure_noBK <- testing(Larv.pure_splits_noBK)


# total number of combinations
nrow(hyper_gridjuv.LARVpure)
## [1] 54

for(i in 1:nrow(hyper_gridjuv.LARVpure)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune.larv.pure_noBK <- gbm(
    formula = Perc_surv ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.y + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT,
    distribution = "gaussian",
    data = Larv18.train.pure_noBK,
    n.trees = 3000,
    interaction.depth = hyper_gridjuv.LARVpure$interaction.depth[i],
    shrinkage = hyper_gridjuv.LARVpure$shrinkage[i],
    n.minobsinnode = hyper_gridjuv.LARVpure$n.minobsinnode[i],
    bag.fraction = hyper_gridjuv.LARVpure$bag.fraction[i],
    train.fraction = .99,
    n.cores = NULL, # will use all cores by default
    verbose = TRUE
  )
  
  # add min training error and trees to grid
  hyper_gridjuv.LARVpure$optimal_trees[i] <- which.min(gbm.tune.larv.pure_noBK$valid.error)
  hyper_gridjuv.LARVpure$min_RMSE[i] <- sqrt(min(gbm.tune.larv.pure_noBK$valid.error))
}

#the new values will now be added to the formally empty rows of the hyper_grid

head(hyper_gridjuv.LARVpure)

hyper_gridjuv.LARVpure %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)


caret_grid.larv.hypergrid_purer.testNatComms_noBKmodif <- data.frame(n.trees = 383, interaction.depth = 3, 
                                                                     shrinkage = 0.01,n.minobsinnode = 5)

set.seed(25)
larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_noBK <- 
  caret::train(Perc_surv ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.y + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT, method = "gbm", data = Larv18.train.pure_noBK, verbose = TRUE,
               tuneGrid = caret_grid.larv.hypergrid_purer.testNatComms_noBKmodif, trControl = fitControl_rando, na.action=na.omit)

#interaction depth from recomended 1 to 3 because plot squared error loss flattens out
larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_noBK$results #check params

plotres(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_noBK)


summary(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_noBK$finalModel, plotit = TRUE)
plotmo(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_noBK, pmethod="partdep", all1=TRUE, all2=TRUE) #testx object
print(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_noBK)
######################
##larv pure no LS----
Raw_larvae2018_filt_temp.metrics5_pure_noLS<-larv6%>%
  filter(Parentage %in% c("CU x CU", "BK x BK","SB x SB","DR x DR"))


set.seed(88901)
Larv.pure_splits_noLS <- initial_split(Raw_larvae2018_filt_temp.metrics5_pure_noLS, .99, strata = "Perc_surv", na.rm=TRUE)
set.seed(88901)
Larv18.train.pure_noLS <- training(Larv.pure_splits_noLS)
set.seed(88901)
Larv18._test.pure_noLS <- testing(Larv.pure_splits_noLS)


# total number of combinations
nrow(hyper_gridjuv.LARVpure)
## [1] 54

for(i in 1:nrow(hyper_gridjuv.LARVpure)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune.larv.pure_noLS <- gbm(
    formula = Perc_surv ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.y + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT,
    distribution = "gaussian",
    data = Larv18.train.pure_noLS,
    n.trees = 3000,
    interaction.depth = hyper_gridjuv.LARVpure$interaction.depth[i],
    shrinkage = hyper_gridjuv.LARVpure$shrinkage[i],
    n.minobsinnode = hyper_gridjuv.LARVpure$n.minobsinnode[i],
    bag.fraction = hyper_gridjuv.LARVpure$bag.fraction[i],
    train.fraction = .99,
    n.cores = NULL, # will use all cores by default
    verbose = TRUE
  )
  
  # add min training error and trees to grid
  hyper_gridjuv.LARVpure$optimal_trees[i] <- which.min(gbm.tune.larv.pure_noLS$valid.error)
  hyper_gridjuv.LARVpure$min_RMSE[i] <- sqrt(min(gbm.tune.larv.pure_noLS$valid.error))
}

#the new values will now be added to the formally empty rows of the hyper_grid

head(hyper_gridjuv.LARVpure)

hyper_gridjuv.LARVpure %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)


caret_grid.larv.hypergrid_purer.testNatComms_noLSmodif <- data.frame(n.trees = 500, interaction.depth = 3, 
                                                                     shrinkage = 0.01,n.minobsinnode = 5)

set.seed(25)
larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_noLS <- 
  caret::train(Perc_surv ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.y + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT, method = "gbm", data = Larv18.train.pure_noLS, verbose = TRUE,
               tuneGrid = caret_grid.larv.hypergrid_purer.testNatComms_noLSmodif, trControl = fitControl_rando, na.action=na.omit)

#interaction depth from recomended 1 to 3 because plot squared error loss flattens out
larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_noLS$results #check params

plotres(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_noLS)


summary(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_noLS$finalModel, plotit = TRUE)
plotmo(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_noLS, pmethod="partdep", all1=TRUE, all2=TRUE) #testx object
print(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_noLS)
######################

######################
###larv pure no SB----
Raw_larvae2018_filt_temp.metrics5_pure_noSB<-larv6%>%
  filter(Parentage %in% c("CU x CU", "BK x BK","LS x LS","DR x DR"))


set.seed(88901)
Larv.pure_splits_noSB <- initial_split(Raw_larvae2018_filt_temp.metrics5_pure_noSB, .99, strata = "Perc_surv", na.rm=TRUE)
set.seed(88901)
Larv18.train.pure_noSB <- training(Larv.pure_splits_noSB)
set.seed(88901)
Larv18._test.pure_noSB <- testing(Larv.pure_splits_noSB)


# total number of combinations
nrow(hyper_gridjuv.LARVpure)
## [1] 54

for(i in 1:nrow(hyper_gridjuv.LARVpure)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune.larv.pure_noSB <- gbm(
    formula = Perc_surv ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.y + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT,
    distribution = "gaussian",
    data = Larv18.train.pure_noSB,
    n.trees = 3000,
    interaction.depth = hyper_gridjuv.LARVpure$interaction.depth[i],
    shrinkage = hyper_gridjuv.LARVpure$shrinkage[i],
    n.minobsinnode = hyper_gridjuv.LARVpure$n.minobsinnode[i],
    bag.fraction = hyper_gridjuv.LARVpure$bag.fraction[i],
    train.fraction = .99,
    n.cores = NULL, # will use all cores by default
    verbose = TRUE
  )
  
  # add min training error and trees to grid
  hyper_gridjuv.LARVpure$optimal_trees[i] <- which.min(gbm.tune.larv.pure_noSB$valid.error)
  hyper_gridjuv.LARVpure$min_RMSE[i] <- sqrt(min(gbm.tune.larv.pure_noSB$valid.error))
}

#the new values will now be added to the formally empty rows of the hyper_grid

head(hyper_gridjuv.LARVpure)

hyper_gridjuv.LARVpure %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)


caret_grid.larv.hypergrid_purer.testNatComms_noSBmodif <- data.frame(n.trees = 1666, interaction.depth = 3, 
                                                                     shrinkage = 0.01,n.minobsinnode = 5)

set.seed(25)
larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_noSB <- 
  caret::train(Perc_surv ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.y + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT, method = "gbm", data = Larv18.train.pure_noSB, verbose = TRUE,
               tuneGrid = caret_grid.larv.hypergrid_purer.testNatComms_noSBmodif, trControl = fitControl_rando, na.action=na.omit)

#interaction depth from recomended 1 to 3 because plot squared error loss flattens out
larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_noSB$results #check params

plotres(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_noSB)

summary(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_noSB$finalModel, plotit = TRUE)
plotmo(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_noSB, pmethod="partdep", all1=TRUE, all2=TRUE) #testx object
print(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_noSB)
######################

######################
###larv pure no Davies----
Raw_larvae2018_filt_temp.metrics5_pure_noDV<-larv6%>%
  filter(Parentage %in% c("CU x CU", "BK x BK","LS x LS","SB x SB"))


set.seed(88901)
Larv.pure_splits_noDV <- initial_split(Raw_larvae2018_filt_temp.metrics5_pure_noDV, .99, strata = "Perc_surv", na.rm=TRUE)
set.seed(88901)
Larv18.train.pure_noDV <- training(Larv.pure_splits_noDV)
set.seed(88901)
Larv18._test.pure_noDV <- testing(Larv.pure_splits_noDV)


# total number of combinations
nrow(hyper_gridjuv.LARVpure)
## [1] 54

for(i in 1:nrow(hyper_gridjuv.LARVpure)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune.larv.pure_noDV <- gbm(
    formula = Perc_surv ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.y + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT,
    distribution = "gaussian",
    data = Larv18.train.pure_noDV,
    n.trees = 3000,
    interaction.depth = hyper_gridjuv.LARVpure$interaction.depth[i],
    shrinkage = hyper_gridjuv.LARVpure$shrinkage[i],
    n.minobsinnode = hyper_gridjuv.LARVpure$n.minobsinnode[i],
    bag.fraction = hyper_gridjuv.LARVpure$bag.fraction[i],
    train.fraction = .99,
    n.cores = NULL, # will use all cores by default
    verbose = TRUE
  )
  
  # add min training error and trees to grid
  hyper_gridjuv.LARVpure$optimal_trees[i] <- which.min(gbm.tune.larv.pure_noDV$valid.error)
  hyper_gridjuv.LARVpure$min_RMSE[i] <- sqrt(min(gbm.tune.larv.pure_noDV$valid.error))
}

#the new values will now be added to the formally empty rows of the hyper_grid

head(hyper_gridjuv.LARVpure)

hyper_gridjuv.LARVpure %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)


caret_grid.larv.hypergrid_purer.testNatComms_noDVmodif <- data.frame(n.trees = 535, interaction.depth = 3, 
                                                                     shrinkage = 0.01,n.minobsinnode = 5)

set.seed(25)
larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_noDV <- 
  caret::train(Perc_surv ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.y + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT, method = "gbm", data = Larv18.train.pure_noDV, verbose = TRUE,
               tuneGrid = caret_grid.larv.hypergrid_purer.testNatComms_noDVmodif, trControl = fitControl_rando, na.action=na.omit)

#interaction depth from recomended 1 to 3 because plot squared error loss flattens out
larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_noDV$results #check params

plotres(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_noDV)

summary(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_noDV$finalModel, plotit = TRUE)
plotmo(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_noDV, pmethod="partdep", all1=TRUE, all2=TRUE) #testx object
print(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_noDV)

#########################bootstrap----

fitControl_boot <- trainControl(## 10-fold CV
  method = "boot",
  number = 10, 
  repeats = 10,
  search = "random")

set.seed(25)
larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom.boot <- 
  caret::train(Perc_surv ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.y + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT, method = "gbm", data = Larv18.train.pure, verbose = TRUE,
               tuneGrid = caret_grid.larv.hypergrid_purer.testNatComms, trControl = fitControl_boot, na.action=na.omit)

summary(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom.boot$finalModel, plotit = TRUE)
plotres(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom.boot)
plotmo(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom.boot, pmethod="partdep", all1=TRUE, all2=TRUE) #testx object
print(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom.boot)
R2.boot<-larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom.boot$resample$Rsquared
boxplot(R2.boot)
mean(R2.boot)
std.error(R2.boot)

x2.boot<-larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom.boot %>% partial(pred.var = "TSA_DHWStandardDeviation") %>% autoplot(smooth = TRUE, smooth.method="auto", smooth.span= 0.5, ylab = expression(f(TSA_DHWStandardDeviation))) + theme_light() + ggtitle("ggplot2-based PDP Bootstrap")
x.larvpur.boot<-ggplot_build(x2.boot)$data[[2]]$x
y.larvpur.boot<-ggplot_build(x2.boot)$data[[2]]$y
TSAbuild.boot<-data.frame(a=ggplot_build(x2.boot)$data[[2]]$x,b=ggplot_build(x2.boot)$data[[2]]$y)

larv.pure.finalTSA.boot<-ggplot(TSAbuild.boot, aes(a, b)) +
  geom_point() +
  stat_smooth(aes(), method = "lm", formula = formula.l.p) +theme_bw()


#########################k fold cross validation----

fitControl_kfoldcv <- trainControl(## 10-fold CV
  method = "cv",
  number = 10, 
  repeats = 10,
  search = "random")

set.seed(25)
larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_kfoldcv <- 
  caret::train(Perc_surv ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.y + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT, method = "gbm", data = Larv18.train.pure, verbose = TRUE,
               tuneGrid = caret_grid.larv.hypergrid_purer.testNatComms, trControl = fitControl_kfoldcv, na.action=na.omit)

summary(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_kfoldcv$finalModel, plotit = TRUE)
plotres(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_kfoldcv)
plotmo(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_kfoldcv, pmethod="partdep", all1=TRUE, all2=TRUE) #testx object
print(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_kfoldcv)
R2._kfoldcv<-larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_kfoldcv$resample$Rsquared
boxplot(R2._kfoldcv)
mean(R2._kfoldcv)
std.error(R2._kfoldcv)

x2_kfoldcv<-larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_kfoldcv %>% partial(pred.var = "TSA_DHWStandardDeviation") %>% autoplot(smooth = TRUE, smooth.method="auto", smooth.span= 0.5, ylab = expression(f(TSA_DHWStandardDeviation))) + theme_light() + ggtitle("ggplot2-based PDP _kfoldcv")
x.larvpur_kfoldcv<-ggplot_build(x2_kfoldcv)$data[[2]]$x
y.larvpur_kfoldcv<-ggplot_build(x2_kfoldcv)$data[[2]]$y
TSAbuild_kfoldcv<-data.frame(a=ggplot_build(x2_kfoldcv)$data[[2]]$x,b=ggplot_build(x2_kfoldcv)$data[[2]]$y)

larv.pure.finalTSA_kfoldcv<-ggplot(TSAbuild_kfoldcv, aes(a, b)) +
  geom_point() +
  stat_smooth(aes(), method = "lm", formula = formula.l.p) +theme_bw()
########################k fold cross validation, leave one out----

fitControl_leaveoneout <- trainControl(## 10-fold CV
  method = "LOOCV",
  number = 10, 
  repeats = 10,
  search = "random")

set.seed(25)
larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_leaveoneout <- 
  caret::train(Perc_surv ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.y + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT, method = "gbm", data = Larv18.train.pure, verbose = TRUE,
               tuneGrid = caret_grid.larv.hypergrid_purer.testNatComms, trControl = fitControl_leaveoneout, na.action=na.omit)

summary(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_leaveoneout$finalModel, plotit = TRUE)
plotres(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_leaveoneout)
plotmo(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_leaveoneout, pmethod="partdep", all1=TRUE, all2=TRUE) #testx object
print(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_leaveoneout)
R2._leaveoneout<-larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_leaveoneout$resample$Rsquared
boxplot(R2._leaveoneout)
mean(R2._leaveoneout)
std.error(R2._leaveoneout)

x2_leaveoneout<-larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_leaveoneout %>% partial(pred.var = "TSA_DHWStandardDeviation") %>% autoplot(smooth = TRUE, smooth.method="auto", smooth.span= 0.5, ylab = expression(f(TSA_DHWStandardDeviation))) + theme_light() + ggtitle("ggplot2-based PDP _leaveoneout")
x.larvpur_leaveoneout<-ggplot_build(x2_leaveoneout)$data[[2]]$x
y.larvpur_leaveoneout<-ggplot_build(x2_leaveoneout)$data[[2]]$y
TSAbuild_leaveoneout<-data.frame(a=ggplot_build(x2_leaveoneout)$data[[2]]$x,b=ggplot_build(x2_leaveoneout)$data[[2]]$y)

larv.pure.finalTSA_leaveoneout<-ggplot(TSAbuild_leaveoneout, aes(a, b)) +
  geom_point() +
  stat_smooth(aes(), method = "lm", formula = formula.l.p) +theme_bw()

##final
factor1<-"Repeatedkfold"
TSAbuild<-cbind(TSAbuild, factor1)
TSAbuild$factor<-TSAbuild$factor1


factor2<-"boot"
TSAbuild.boot<-cbind(TSAbuild.boot, factor2)
TSAbuild.boot$factor<-TSAbuild.boot$factor2

factor3<-"kfoldcv"
TSAbuild_kfoldcv<-cbind(TSAbuild_kfoldcv, factor3)
TSAbuild_kfoldcv$factor<-TSAbuild_kfoldcv$factor3

factor4<-"leaveout"
TSAbuild_leaveoneout<-cbind(TSAbuild_leaveoneout, factor4)
TSAbuild_leaveoneout$factor<-TSAbuild_leaveoneout$factor4

TSAbuild<-TSAbuild[, c("a", "b", "factor")]
TSAbuild.boot<-TSAbuild.boot[, c("a", "b", "factor")]
TSAbuild_kfoldcv<-TSAbuild_kfoldcv[, c("a", "b", "factor")]
TSAbuild_leaveoneout<-TSAbuild_leaveoneout[, c("a", "b", "factor")]

dim(CV.testing.df<- full_join(TSAbuild, TSAbuild.boot, copy=FALSE, by=c("a", "b", "factor")))
head(CV.testing.df2<- full_join(TSAbuild_kfoldcv, TSAbuild_leaveoneout, by=c("a", "b", "factor")))
CV.testing.df3<-full_join(CV.testing.df, CV.testing.df2, by=c("a", "b", "factor"))

CV.testing.df3.fig<-ggplot(CV.testing.df3, aes(a, b, colour=factor)) +
  geom_point() +
  stat_smooth(aes(colour=factor), method = "lm", formula = formula.l.p) +theme_bw()+theme(legend.position="none")+
  scale_fill_grey(start = .5, end = 1, labels = c("Repeated k-fold CV", "Bootstrap", "k-fold CV", "Leave One Out CV"))+ylab("Predicted survival (%)")+xlab("TSA_DHW_stdev")


Rsquared<-c(0.8358, 0.8367, 0.8356, 0.83533)
factors.Rsquared<-c("Repeated k-fold CV", "Bootstrap", "k-fold CV", "Leave One Out CV")
Rsqaured.SE<-c(0.00128, 0.00208, 0.00466, NA)
Rsquared.df <- data.frame(Rsquared, factors.Rsquared, stringsAsFactors=FALSE)
Rsquared.df.t<-data.frame(Rsquared.df, Rsqaured.SE, stringsAsFactors=FALSE)
as.factor(Rsquared.df.t$factors.Rsquared)
Rsquared.CV.fig<-ggplot(Rsquared.df.t, aes(factors.Rsquared, Rsquared, colour=factors.Rsquared)) +
  geom_point() +geom_errorbar(aes(ymin=Rsquared-Rsqaured.SE, ymax=Rsquared+Rsqaured.SE), width=0.2, position=position_dodge(.2))+xlab("Cross validation methods")+ylab("R squared +/-SE")+
  stat_smooth(aes(colour=factors.Rsquared), method = "lm", formula = formula.l.p) +theme_bw()+
  theme(axis.text.x=element_blank())

Rsquared.CV.fig+CV.testing.df3.fig+plot_layout(guides = "collect", ncol = 2)

######################test and train
Raw_larvae2018_filt_temp.metrics5_pure_onlyCU<-larv6%>%
  filter(Parentage %in% c("CU x CU"))

set.seed(88901)
Larv.pure_splits._noCU <- initial_split(Raw_larvae2018_filt_temp.metrics5_pure_noCU, .99, strata = "Perc_surv", na.rm=TRUE)
set.seed(88901)
Larv18.train.pure_noCU <- training(Larv.pure_splits._noCU)


set.seed(88901)
Larv.pure_splits._onlyCU <- initial_split(Raw_larvae2018_filt_temp.metrics5_pure_onlyCU, .99, strata = "Perc_surv", na.rm=TRUE)
set.seed(88901)
Larv18._test.pure_onlyCU <- testing(Larv.pure_splits._onlyCU)
set.seed(88901)
Larv18.train.pure_onlyCU <- training(Larv.pure_splits._onlyCU)


set.seed(25)
larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_onlyCU<- 
  caret::train(Perc_surv ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.y + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT, method = "gbm", data = Larv18.train.pure_onlyCU, verbose = TRUE,
               tuneGrid = caret_grid.larv.hypergrid_purer.testNatComms_noDVmodif, trControl = fitControl_rando, na.action=na.omit)

#interaction depth from recomended 1 to 3 because plot squared error loss flattens out
#larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_noDV$results #check params

#plotres(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_noDV)

#summary(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_noDV$finalModel, plotit = TRUE)
#plotmo(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_noDV, pmethod="partdep", all1=TRUE, all2=TRUE) #testx object
#print(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_noDV)

x_test <- Larv18._test.pure_onlyCU #7 61
predictions <- predict(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom.NoCU, x_test)
# summarize results
pred<-as.table(predictions)
onlycu<-as.table(x_test$Perc_surv)
confusionMatrix(pred, onlycu)
xtab<-table(pred, onlycu)
confusionMatrix(xtab)

knn_pred <- predict(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom.NoCU, newdata = Larv18._test.pure_onlyCU)
knn_pred2 <- predict(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom.NoCU, newdata = x)
x<-Larv18._test.pure_onlyCU$Perc_surv
confusionMatrix(knn_pred, Larv18._test.pure_onlyCU$Perc_surv)
resampleresult<-resamples(Larv18._test.pure_onlyCU)

fitted<-as.factor(predict(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom.NoCU))
fitted2<-predict(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom.NoCU)
#y.predicted <- predict(lm1, newdata=test)
y.actual <- as.factor(Larv18._test.pure_onlyCU$Perc_surv)
predict(Larv18._test.pure_onlyCU$Perc_surv)
y.actualpredict<-predict(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_onlyCU)#too small data with only CU?
(subsample.fitted<-sample(fitted, size=7))
(subsample.fitted2<-sample(fitted2, size=7))
y.actual.num<-as.numeric(Larv18._test.pure_onlyCU$Perc_surv)

#errors <- (y.actual - y.predicted)
errors <- (y.actual - knn_pred)
1 - sum(errors^2)/sum(y.actual^2) #R2
#Also, generally speaking, you should not be using R-squared for your test data but something like RMSE or MSE instead.
plot(varImp(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom.NoCU))

#23 vs 4 levels
#y.actual
#[1] 65 80 85 75 75 75 75
#Levels: 65 75 80 85
#subsample 4 values?
sy.actual2<-cut(y.actual, 3)
subsample.fitted2<-cut(subsample.fitted, 3, labels = 1:3)
boxplot(y.actual.num)
boxplot(fitted2)
mean(fitted2)
caret::confusionMatrix(reference = y.actual.num, data = subsample.fitted2, mode='everything', positive='MM')

fitted<-as.factor(predict(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom.NoCU))
y.actualpredict.f<-as.factor(y.actualpredict) #onlyCU

fitted.n<-as.numeric(predict(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom.NoCU))
y.actualpredict.n<-as.numeric(y.actualpredict) #onlyCU

boxplot(fitted.n)
boxplot(y.actualpredict.n)

levels(fitted)
levels(y.actualpredict.f)
str(fitted) #23 levels
str(y.actualpredict.f) #6 levels
levels(fitted) <- list("0" = "1", "1" = "2")

caret::confusionMatrix(reference = y.actualpredict.f, data = fitted, mode='everything', positive='MM') #needs to feactor of same levels

##Predict boxplots without Curd----
predictions <- predict(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom.NoCU, Raw_larvae2018_filt_temp.metrics5_pure_onlyCU)
pred<-as.numeric(predictions)
onlycu<-as.table(Raw_larvae2018_filt_temp.metrics5_pure_onlyCU$Perc_surv)
confusionMatrix(pred, onlycu)
xtab<-table(pred, onlycu)
confusionMatrix(xtab)

y.actual <- as.numeric(Larv18._test.pure_onlyCU$Perc_surv)
errors <- (y.actual - predictions)

set.seed(100)

# Step 1: Get row numbers for the training data
trainRowNumbers <- createDataPartition(Raw_larvae2018_filt_temp.metrics5_pure_onlyCU$Perc_surv, p=0.8, list=FALSE)

# Step 2: Create the training  dataset
trainData <- Raw_larvae2018_filt_temp.metrics5_pure_onlyCU[trainRowNumbers,]

# Step 3: Create the test dataset
testData <- Raw_larvae2018_filt_temp.metrics5_pure_onlyCU[-trainRowNumbers,]

# Store X and Y for later use.
x = trainData[, 2:18]
y = trainData$Perc_surv

testData2 <- predict(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom.NoCU, testData) 
confusionMatrix(reference = testData$Perc_surv, data = testData2, mode='everything', positive='MM')

fitted.n<-as.numeric(predict(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom.NoCU)) #**
y.actualpredict.n<-as.numeric(y.actualpredict) #onlyCU

boxplot(fitted.n) #**
boxplot(y.actualpredict.n)

knn_pred <- predict(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom.NoCU, newdata = Larv18._test.pure_onlyCU)

knn_pred.withcu <- predict(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom, newdata = Raw_larvae2018_filt_temp.metrics5_pure_onlyCU)
knn_pred.withcu.n<-as.numeric(knn_pred.withcu)
boxplot(knn_pred.withcu.n)

knn_pred <- predict(larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom.NoCU, newdata =  Larv18.train.pure_onlyCU)
knn_pred.n<-as.numeric(knn_pred)
boxplot(knn_pred.n)

boxplot(Raw_larvae2018_filt_temp.metrics5_pure_onlyCU$Perc_surv) #75 %

###########################################################
###########################################################
#hybrid larvae drop one----
#######################################################################################
#####################(1.5 test cross validation procedure)####-----
##larv hybrid no CU----
Raw_larvae2018_filt_temp.metrics5_hybrid_noCU<-larv6%>%
  filter(Parentage %in% c("BK x LS", "BK x SB", "BK x DR", 
                          "DR x LS", "DR x SB", "DR x BK",
                          "LS x BK", "LS x SB", "LS x DR",
                          "SB x LS", "SB x BK", "SB x DR"))

set.seed(88901)
Larv.hybrid_splits_noCU <- initial_split(Raw_larvae2018_filt_temp.metrics5_hybrid_noCU, .99, strata = "Perc_surv", na.rm=TRUE)
set.seed(88901)
Larv18.train.hybrid_noCU <- training(Larv.hybrid_splits_noCU)
set.seed(88901)
Larv18._test.hybrid_noCU <- testing(Larv.hybrid_splits_noCU)


# total number of combinations
nrow(hyper_gridjuv.LARVpure)
## [1] 54

for(i in 1:nrow(hyper_gridjuv.LARVpure)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune.larv.hybrid_noCU <- gbm(
    formula = Perc_surv ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.y + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT,
    distribution = "gaussian",
    data = Larv18.train.hybrid_noCU,
    n.trees = 3000,
    interaction.depth = hyper_gridjuv.LARVpure$interaction.depth[i],
    shrinkage = hyper_gridjuv.LARVpure$shrinkage[i],
    n.minobsinnode = hyper_gridjuv.LARVpure$n.minobsinnode[i],
    bag.fraction = hyper_gridjuv.LARVpure$bag.fraction[i],
    train.fraction = .99,
    n.cores = NULL, # will use all cores by default
    verbose = TRUE
  )
  
  # add min training error and trees to grid
  hyper_gridjuv.LARVpure$optimal_trees[i] <- which.min(gbm.tune.larv.hybrid_noCU$valid.error)
  hyper_gridjuv.LARVpure$min_RMSE[i] <- sqrt(min(gbm.tune.larv.hybrid_noCU$valid.error))
}

#the new values will now be added to the formally empty rows of the hyper_grid

head(hyper_gridjuv.LARVpure)

hyper_gridjuv.LARVpure %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)


caret_grid.larv.hypergrid_hybr.noCU <- data.frame(n.trees = 92, interaction.depth = 1,
                                             shrinkage = 0.3,
                                             n.minobsinnode = 5)

caret_grid.larv.hypergrid_hybr <- data.frame(n.trees = 100, interaction.depth = 1,
                                             shrinkage = 0.3,
                                             n.minobsinnode = 5)
set.seed(25)
larv.fit_gbm_caret.cv_HYB.hyperTune.noCU <- 
  caret::train(Perc_surv ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.y + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation + DRT, method = "gbm", 
               data = Larv18.train.hybrid_noCU, verbose = TRUE,
               tuneGrid = caret_grid.larv.hypergrid_hybr.noCU, trControl = fitControl_rando)

summary(larv.fit_gbm_caret.cv_HYB.hyperTune.noCU$finalModel, plotit = TRUE)
plotres(larv.fit_gbm_caret.cv_HYB.hyperTune.noCU)
plotmo(larv.fit_gbm_caret.cv_HYB.hyperTune.noCU, pmethod="partdep", all1=TRUE, all2=TRUE) #testx object

###################### larva hyb no bk----
Raw_larvae2018_filt_temp.metrics5_hybrid_noBK<-larv6%>%
  filter(Parentage %in% c("DR x LS", "DR x CU", "DR x SB",
                          "CU x LS", "CU x SB", "CU x DR",
                          "LS x CU", "LS x SB", "LS x DR",
                          "SB x LS", "SB x CU", "SB x DR"))

set.seed(88901)
Larv.hybrid_splits_noBK <- initial_split(Raw_larvae2018_filt_temp.metrics5_hybrid_noBK, .99, strata = "Perc_surv", na.rm=TRUE)
set.seed(88901)
Larv18.train.hybrid_noBK <- training(Larv.hybrid_splits_noBK)
set.seed(88901)
Larv18._test.hybrid_noBK <- testing(Larv.hybrid_splits_noBK)


# total number of combinations
nrow(hyper_gridjuv.LARVpure)
## [1] 54

for(i in 1:nrow(hyper_gridjuv.LARVpure)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune.larv.hybrid_noBK <- gbm(
    formula = Perc_surv ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.y + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT,
    distribution = "gaussian",
    data = Larv18.train.hybrid_noBK,
    n.trees = 3000,
    interaction.depth = hyper_gridjuv.LARVpure$interaction.depth[i],
    shrinkage = hyper_gridjuv.LARVpure$shrinkage[i],
    n.minobsinnode = hyper_gridjuv.LARVpure$n.minobsinnode[i],
    bag.fraction = hyper_gridjuv.LARVpure$bag.fraction[i],
    train.fraction = .99,
    n.cores = NULL, # will use all cores by default
    verbose = TRUE
  )
  
  # add min training error and trees to grid
  hyper_gridjuv.LARVpure$optimal_trees[i] <- which.min(gbm.tune.larv.hybrid_noBK$valid.error)
  hyper_gridjuv.LARVpure$min_RMSE[i] <- sqrt(min(gbm.tune.larv.hybrid_noBK$valid.error))
}

#the new values will now be added to the formally empty rows of the hyper_grid

head(hyper_gridjuv.LARVpure)

hyper_gridjuv.LARVpure %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)


caret_grid.larv.hypergrid_hybr.noBK <- data.frame(n.trees = 30, interaction.depth = 1,
                                                  shrinkage = 0.3,
                                                  n.minobsinnode = 5)

caret_grid.larv.hypergrid_hybr <- data.frame(n.trees = 100, interaction.depth = 1,
                                             shrinkage = 0.3,
                                             n.minobsinnode = 5)
set.seed(25)
larv.fit_gbm_caret.cv_HYB.hyperTune.noBK <- 
  caret::train(Perc_surv ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.y + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation + DRT, method = "gbm", 
               data = Larv18.train.hybrid_noBK, verbose = TRUE,
               tuneGrid = caret_grid.larv.hypergrid_hybr.noBK, trControl = fitControl_rando)

summary(larv.fit_gbm_caret.cv_HYB.hyperTune.noBK$finalModel, plotit = TRUE)
plotres(larv.fit_gbm_caret.cv_HYB.hyperTune.noBK)
plotmo(larv.fit_gbm_caret.cv_HYB.hyperTune.noBK, pmethod="partdep", all1=TRUE, all2=TRUE) #testx object


######################
##larv hyb no LS----
Raw_larvae2018_filt_temp.metrics5_hybrid_noLS<-larv6%>%
  filter(Parentage %in% c("BK x CU", "BK x SB", "BK x DR", 
                          "DR x CU", "DR x SB", "DR x BK",
                          "CU x BK", "CU x SB", "CU x DR",
                          "SB x CU", "SB x BK", "SB x DR"))

set.seed(88901)
Larv.hybrid_splits_noLS <- initial_split(Raw_larvae2018_filt_temp.metrics5_hybrid_noLS, .99, strata = "Perc_surv", na.rm=TRUE)
set.seed(88901)
Larv18.train.hybrid_noLS <- training(Larv.hybrid_splits_noLS)
set.seed(88901)
Larv18._test.hybrid_noLS <- testing(Larv.hybrid_splits_noLS)


# total number of combinations
nrow(hyper_gridjuv.LARVpure)
## [1] 54

for(i in 1:nrow(hyper_gridjuv.LARVpure)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune.larv.hybrid_noLS <- gbm(
    formula = Perc_surv ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.y + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT,
    distribution = "gaussian",
    data = Larv18.train.hybrid_noLS,
    n.trees = 3000,
    interaction.depth = hyper_gridjuv.LARVpure$interaction.depth[i],
    shrinkage = hyper_gridjuv.LARVpure$shrinkage[i],
    n.minobsinnode = hyper_gridjuv.LARVpure$n.minobsinnode[i],
    bag.fraction = hyper_gridjuv.LARVpure$bag.fraction[i],
    train.fraction = .99,
    n.cores = NULL, # will use all cores by default
    verbose = TRUE
  )
  
  # add min training error and trees to grid
  hyper_gridjuv.LARVpure$optimal_trees[i] <- which.min(gbm.tune.larv.hybrid_noLS$valid.error)
  hyper_gridjuv.LARVpure$min_RMSE[i] <- sqrt(min(gbm.tune.larv.hybrid_noLS$valid.error))
}

#the new values will now be added to the formally empty rows of the hyper_grid

head(hyper_gridjuv.LARVpure)

hyper_gridjuv.LARVpure %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)

caret_grid.larv.hypergrid_hybr.noLS <- data.frame(n.trees = 1, interaction.depth = 1,
                                                  shrinkage = 0.01,
                                                  n.minobsinnode = 5)

caret_grid.larv.hypergrid_hybr <- data.frame(n.trees = 100, interaction.depth = 1,
                                             shrinkage = 0.3,
                                             n.minobsinnode = 5)
set.seed(25)
larv.fit_gbm_caret.cv_HYB.hyperTune.noLS <- 
  caret::train(Perc_surv ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.y + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation + DRT, method = "gbm", 
               data = Larv18.train.hybrid_noLS, verbose = TRUE,
               tuneGrid = caret_grid.larv.hypergrid_hybr.noLS, trControl = fitControl_rando)

summary(larv.fit_gbm_caret.cv_HYB.hyperTune.noLS$finalModel, plotit = TRUE)
plotres(larv.fit_gbm_caret.cv_HYB.hyperTune.noLS)
plotmo(larv.fit_gbm_caret.cv_HYB.hyperTune.noLS, pmethod="partdep", all1=TRUE, all2=TRUE) #testx object


######################

######################
###larv hyb no SB----
Raw_larvae2018_filt_temp.metrics5_hybrid_noSB<-larv6%>%
  filter(Parentage %in% c("BK x LS", "BK x CU", "BK x DR", 
                          "DR x LS", "DR x CU", "DR x BK",
                          "CU x LS", "CU x BK", "CU x DR",
                          "LS x BK", "LS x CU", "LS x DR"))


set.seed(88901)
Larv.hybrid_splits_noSB <- initial_split(Raw_larvae2018_filt_temp.metrics5_hybrid_noSB, .99, strata = "Perc_surv", na.rm=TRUE)
set.seed(88901)
Larv18.train.hybrid_noSB <- training(Larv.hybrid_splits_noSB)
set.seed(88901)
Larv18._test.hybrid_noSB <- testing(Larv.hybrid_splits_noSB)


# total number of combinations
nrow(hyper_gridjuv.LARVpure)
## [1] 54

for(i in 1:nrow(hyper_gridjuv.LARVpure)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune.larv.hybrid_noSB <- gbm(
    formula = Perc_surv ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.y + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT,
    distribution = "gaussian",
    data = Larv18.train.hybrid_noSB,
    n.trees = 3000,
    interaction.depth = hyper_gridjuv.LARVpure$interaction.depth[i],
    shrinkage = hyper_gridjuv.LARVpure$shrinkage[i],
    n.minobsinnode = hyper_gridjuv.LARVpure$n.minobsinnode[i],
    bag.fraction = hyper_gridjuv.LARVpure$bag.fraction[i],
    train.fraction = .99,
    n.cores = NULL, # will use all cores by default
    verbose = TRUE
  )
  
  # add min training error and trees to grid
  hyper_gridjuv.LARVpure$optimal_trees[i] <- which.min(gbm.tune.larv.hybrid_noSB$valid.error)
  hyper_gridjuv.LARVpure$min_RMSE[i] <- sqrt(min(gbm.tune.larv.hybrid_noSB$valid.error))
}

#the new values will now be added to the formally empty rows of the hyper_grid

head(hyper_gridjuv.LARVpure)

hyper_gridjuv.LARVpure %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)


caret_grid.larv.hypergrid_hybr.nosB <- data.frame(n.trees = 2465, interaction.depth = 5,
                                                  shrinkage = 0.3,
                                                  n.minobsinnode = 5)

caret_grid.larv.hypergrid_hybr <- data.frame(n.trees = 100, interaction.depth = 1,
                                             shrinkage = 0.3,
                                             n.minobsinnode = 5)
set.seed(25)
larv.fit_gbm_caret.cv_HYB.hyperTune.noSB <- 
  caret::train(Perc_surv ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.y + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation + DRT, method = "gbm", 
               data = Larv18.train.hybrid_noSB, verbose = TRUE,
               tuneGrid = caret_grid.larv.hypergrid_hybr.nosB, trControl = fitControl_rando)

summary(larv.fit_gbm_caret.cv_HYB.hyperTune.noSB$finalModel, plotit = TRUE)
plotres(larv.fit_gbm_caret.cv_HYB.hyperTune.noSB)
plotmo(larv.fit_gbm_caret.cv_HYB.hyperTune.noSB, pmethod="partdep", all1=TRUE, all2=TRUE) #testx object######################

######################
###larv hyb no Davies----
Raw_larvae2018_filt_temp.metrics5_hybrid_noDV<-larv6%>%
  filter(Parentage %in% c("BK x LS", "BK x CU", "BK x SB",
                          "CU x LS", "CU x BK", "CU x SB", 
                          "LS x BK", "LS x CU", "LS x SB", 
                          "SB x LS", "SB x CU", "SB x BK"))


set.seed(88901)
Larv.hybrid_splits_noDV <- initial_split(Raw_larvae2018_filt_temp.metrics5_hybrid_noDV, .99, strata = "Perc_surv", na.rm=TRUE)
set.seed(88901)
Larv18.train.hybrid_noDV <- training(Larv.hybrid_splits_noDV)
set.seed(88901)
Larv18._test.hybrid_noDV <- testing(Larv.hybrid_splits_noDV)


# total number of combinations
nrow(hyper_gridjuv.LARVpure)
## [1] 54

for(i in 1:nrow(hyper_gridjuv.LARVpure)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune.larv.hybrid_noDV <- gbm(
    formula = Perc_surv ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.y + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT,
    distribution = "gaussian",
    data = Larv18.train.hybrid_noDV,
    n.trees = 3000,
    interaction.depth = hyper_gridjuv.LARVpure$interaction.depth[i],
    shrinkage = hyper_gridjuv.LARVpure$shrinkage[i],
    n.minobsinnode = hyper_gridjuv.LARVpure$n.minobsinnode[i],
    bag.fraction = hyper_gridjuv.LARVpure$bag.fraction[i],
    train.fraction = .99,
    n.cores = NULL, # will use all cores by default
    verbose = TRUE
  )
  
  # add min training error and trees to grid
  hyper_gridjuv.LARVpure$optimal_trees[i] <- which.min(gbm.tune.larv.hybrid_noDV$valid.error)
  hyper_gridjuv.LARVpure$min_RMSE[i] <- sqrt(min(gbm.tune.larv.hybrid_noDV$valid.error))
}

#the new values will now be added to the formally empty rows of the hyper_grid

head(hyper_gridjuv.LARVpure)

hyper_gridjuv.LARVpure %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)

caret_grid.larv.hypergrid_hybr.noDV <- data.frame(n.trees = 2407, interaction.depth = 1,
                                                  shrinkage = 0.3,
                                                  n.minobsinnode = 5)

caret_grid.larv.hypergrid_hybr <- data.frame(n.trees = 100, interaction.depth = 1,
                                             shrinkage = 0.3,
                                             n.minobsinnode = 5)
set.seed(25)
larv.fit_gbm_caret.cv_HYB.hyperTune.noDV <- 
  caret::train(Perc_surv ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.y + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation + DRT, method = "gbm", 
               data = Larv18.train.hybrid_noDV, verbose = TRUE,
               tuneGrid = caret_grid.larv.hypergrid_hybr.noDV, trControl = fitControl_rando)

summary(larv.fit_gbm_caret.cv_HYB.hyperTune.noDV$finalModel, plotit = TRUE)
plotres(larv.fit_gbm_caret.cv_HYB.hyperTune.noDV)
plotmo(larv.fit_gbm_caret.cv_HYB.hyperTune.noDV, pmethod="partdep", all1=TRUE, all2=TRUE) #testx object######################


###########################################################
###########################################################
#####################(3 Purebred D1)####-----
juv6.6_subset<-read.csv("juv6.6_subset.csv")
Raw_juv2018_filt_temp.metrics5_D1<-juv6.6_subset %>% filter(Zoox %in% c("D1"))

Raw_juv2018_filt_temp.metrics5_D1_pure_NoBk<-Raw_juv2018_filt_temp.metrics5_D1 %>% 
  filter(Family %in% c(" LSxLS   "," CUxCU   "," SBxSB   "," DRxDR   "))

###Summary 
set.seed(88901)
JuvD1.pure_splits_NoBk <- initial_split(Raw_juv2018_filt_temp.metrics5_D1_pure_NoBk, .99, strata = "PercAlive", na.rm=TRUE) 
set.seed(88901)
Juv18.D1_train.pure_NoBk <- training(JuvD1.pure_splits_NoBk)
set.seed(88901)
Juv18.D1_test.pure_NoBk <- testing(JuvD1.pure_splits_NoBk)

####
hyper_gridjuv.D1pure <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.8, 1),
  optimal_trees = 0,               
  min_RMSE = 0                     
)

# total number of combinations
nrow(hyper_gridjuv.D1pure)
## [1] 54

for(i in 1:nrow(hyper_gridjuv.D1pure)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune.juvs.D1pure_NoBk <- gbm(
    formula = PercAlive ~ ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.x + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT,
    distribution = "gaussian",
    data = Juv18.D1_train.pure_NoBk,
    n.trees = 3000,
    interaction.depth = hyper_gridjuv.D1pure$interaction.depth[i],
    shrinkage = hyper_gridjuv.D1pure$shrinkage[i],
    n.minobsinnode = hyper_gridjuv.D1pure$n.minobsinnode[i],
    bag.fraction = hyper_gridjuv.D1pure$bag.fraction[i],
    train.fraction = .99,
    n.cores = NULL, # will use all cores by default
    verbose = TRUE
  )
  
  # add min training error and trees to grid
  hyper_gridjuv.D1pure$optimal_trees[i] <- which.min(gbm.tune.juvs.D1pure_NoBk$valid.error)
  hyper_gridjuv.D1pure$min_RMSE[i] <- sqrt(min(gbm.tune.juvs.D1pure_NoBk$valid.error))
}

#the new values will now be added to the formally empty rows of the hyper_grid

head(hyper_gridjuv.D1pure)

hyper_gridjuv.D1pure %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)

caret_grid.JUV.hypergrid.D1pure <- data.frame(n.trees = 30, 
                                                   interaction.depth = 1, 
                                                   shrinkage = 0.3,
                                                   n.minobsinnode = 10)

caret_grid.JUV.hypergrid.D1pure_NoBk <- data.frame(n.trees = 2136, 
                                                   interaction.depth = 1, 
                                                   shrinkage = 0.3,
                                                   n.minobsinnode = 10)
set.seed(25)
larv.fit_gbm_caret.cv_D1pure.hyperTune_NoBk <- 
  caret::train(PercAlive ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.x + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT, method = "gbm", 
               data = Juv18.D1_train.pure_NoBk, verbose = TRUE,
               tuneGrid = caret_grid.JUV.hypergrid.D1pure_NoBk, trControl = fitControl_rando, na.action=na.omit)
plotres(larv.fit_gbm_caret.cv_D1pure.hyperTune_NoBk)
summary(larv.fit_gbm_caret.cv_D1pure.hyperTune_NoBk$finalModel, plotit = F)
plotmo(larv.fit_gbm_caret.cv_D1pure.hyperTune_NoBk, pmethod="partdep", all1=TRUE, all2=TRUE)

###########################################################
#No LS D1 purebreds
Raw_juv2018_filt_temp.metrics5_D1_pure_NoLS<-Raw_juv2018_filt_temp.metrics5_D1 %>% 
  filter(Family %in% c(" BKxBK   ", " CUxCU   "," SBxSB   "," DRxDR   "))

###Summary 
set.seed(88901)
JuvD1.pure_splits_NoLS <- initial_split(Raw_juv2018_filt_temp.metrics5_D1_pure_NoLS, .99, strata = "PercAlive", na.rm=TRUE) 
set.seed(88901)
Juv18.D1_train.pure_NoLS <- training(JuvD1.pure_splits_NoLS)
set.seed(88901)
Juv18.D1_test.pure_NoLS <- testing(JuvD1.pure_splits_NoLS)

####
hyper_gridjuv.D1pure <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.8, 1),
  optimal_trees = 0,               
  min_RMSE = 0                     
)

# total number of combinations
nrow(hyper_gridjuv.D1pure)
## [1] 54

for(i in 1:nrow(hyper_gridjuv.D1pure)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune.juvs.D1pure_NoLS <- gbm(
    formula = PercAlive ~ ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.x + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT,
    distribution = "gaussian",
    data = Juv18.D1_train.pure_NoLS,
    n.trees = 3000,
    interaction.depth = hyper_gridjuv.D1pure$interaction.depth[i],
    shrinkage = hyper_gridjuv.D1pure$shrinkage[i],
    n.minobsinnode = hyper_gridjuv.D1pure$n.minobsinnode[i],
    bag.fraction = hyper_gridjuv.D1pure$bag.fraction[i],
    train.fraction = .99,
    n.cores = NULL, # will use all cores by default
    verbose = TRUE
  )
  
  # add min training error and trees to grid
  hyper_gridjuv.D1pure$optimal_trees[i] <- which.min(gbm.tune.juvs.D1pure_NoLS$valid.error)
  hyper_gridjuv.D1pure$min_RMSE[i] <- sqrt(min(gbm.tune.juvs.D1pure_NoLS$valid.error))
}

#the new values will now be added to the formally empty rows of the hyper_grid

head(hyper_gridjuv.D1pure)

hyper_gridjuv.D1pure %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)

caret_grid.JUV.hypergrid.D1pure <- data.frame(n.trees = 30, 
                                              interaction.depth = 1, 
                                              shrinkage = 0.3,
                                              n.minobsinnode = 10)

caret_grid.JUV.hypergrid.D1pure_NoLS <- data.frame(n.trees = 1055, 
                                                   interaction.depth = 1, 
                                                   shrinkage = 0.3,
                                                   n.minobsinnode = 15)
set.seed(25)
larv.fit_gbm_caret.cv_D1pure.hyperTune_NoLS <- 
  caret::train(PercAlive ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.x + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT, method = "gbm", 
               data = Juv18.D1_train.pure_NoLS, verbose = TRUE,
               tuneGrid = caret_grid.JUV.hypergrid.D1pure_NoLS, trControl = fitControl_rando, na.action=na.omit)
plotres(larv.fit_gbm_caret.cv_D1pure.hyperTune_NoLS)
summary(larv.fit_gbm_caret.cv_D1pure.hyperTune_NoLS$finalModel, plotit = F)
plotmo(larv.fit_gbm_caret.cv_D1pure.hyperTune_NoLS, pmethod="partdep", all1=TRUE, all2=TRUE)

###########################################################
#No CU D1 purebreds
Raw_juv2018_filt_temp.metrics5_D1_pure_NoCU<-Raw_juv2018_filt_temp.metrics5_D1 %>% 
  filter(Family %in% c(" BKxBK   ", " LSxLS   "," SBxSB   "," DRxDR   "))

###Summary 
set.seed(88901)
JuvD1.pure_splits_NoCU <- initial_split(Raw_juv2018_filt_temp.metrics5_D1_pure_NoCU, .99, strata = "PercAlive", na.rm=TRUE) 
set.seed(88901)
Juv18.D1_train.pure_NoCU <- training(JuvD1.pure_splits_NoCU)
set.seed(88901)
Juv18.D1_test.pure_NoCU <- testing(JuvD1.pure_splits_NoCU)

####
hyper_gridjuv.D1pure <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.8, 1),
  optimal_trees = 0,               
  min_RMSE = 0                     
)

# total number of combinations
nrow(hyper_gridjuv.D1pure)
## [1] 54

for(i in 1:nrow(hyper_gridjuv.D1pure)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune.juvs.D1pure_NoCU <- gbm(
    formula = PercAlive ~ ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.x + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT,
    distribution = "gaussian",
    data = Juv18.D1_train.pure_NoCU,
    n.trees = 3000,
    interaction.depth = hyper_gridjuv.D1pure$interaction.depth[i],
    shrinkage = hyper_gridjuv.D1pure$shrinkage[i],
    n.minobsinnode = hyper_gridjuv.D1pure$n.minobsinnode[i],
    bag.fraction = hyper_gridjuv.D1pure$bag.fraction[i],
    train.fraction = .99,
    n.cores = NULL, # will use all cores by default
    verbose = TRUE
  )
  
  # add min training error and trees to grid
  hyper_gridjuv.D1pure$optimal_trees[i] <- which.min(gbm.tune.juvs.D1pure_NoCU$valid.error)
  hyper_gridjuv.D1pure$min_RMSE[i] <- sqrt(min(gbm.tune.juvs.D1pure_NoCU$valid.error))
}

#the new values will now be added to the formally empty rows of the hyper_grid

head(hyper_gridjuv.D1pure)

hyper_gridjuv.D1pure %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)

caret_grid.JUV.hypergrid.D1pure <- data.frame(n.trees = 30, 
                                              interaction.depth = 1, 
                                              shrinkage = 0.3,
                                              n.minobsinnode = 10)

caret_grid.JUV.hypergrid.D1pure_NoCU <- data.frame(n.trees = 251, 
                                                   interaction.depth = 3, 
                                                   shrinkage = 0.1,
                                                   n.minobsinnode = 15)
set.seed(25)
larv.fit_gbm_caret.cv_D1pure.hyperTune_NoCU <- 
  caret::train(PercAlive ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.x + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT, method = "gbm", 
               data = Juv18.D1_train.pure_NoCU, verbose = TRUE,
               tuneGrid = caret_grid.JUV.hypergrid.D1pure_NoCU, trControl = fitControl_rando, na.action=na.omit)
plotres(larv.fit_gbm_caret.cv_D1pure.hyperTune_NoCU)
summary(larv.fit_gbm_caret.cv_D1pure.hyperTune_NoCU$finalModel, plotit = F)
plotmo(larv.fit_gbm_caret.cv_D1pure.hyperTune_NoCU, pmethod="partdep", all1=TRUE, all2=TRUE)

###########################################################
#No SB D1 purebreds
Raw_juv2018_filt_temp.metrics5_D1_pure_NoSB<-Raw_juv2018_filt_temp.metrics5_D1 %>% 
  filter(Family %in% c(" BKxBK   ", " LSxLS   "," CUxCU   "," DRxDR   "))

###Summary 
set.seed(88901)
JuvD1.pure_splits_NoSB <- initial_split(Raw_juv2018_filt_temp.metrics5_D1_pure_NoSB, .99, strata = "PercAlive", na.rm=TRUE) 
set.seed(88901)
Juv18.D1_train.pure_NoSB <- training(JuvD1.pure_splits_NoSB)
set.seed(88901)
Juv18.D1_test.pure_NoSB <- testing(JuvD1.pure_splits_NoSB)

####
hyper_gridjuv.D1pure <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.8, 1),
  optimal_trees = 0,               
  min_RMSE = 0                     
)

# total number of combinations
nrow(hyper_gridjuv.D1pure)
## [1] 54

for(i in 1:nrow(hyper_gridjuv.D1pure)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune.juvs.D1pure_NoSB <- gbm(
    formula = PercAlive ~ ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.x + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT,
    distribution = "gaussian",
    data = Juv18.D1_train.pure_NoSB,
    n.trees = 3000,
    interaction.depth = hyper_gridjuv.D1pure$interaction.depth[i],
    shrinkage = hyper_gridjuv.D1pure$shrinkage[i],
    n.minobsinnode = hyper_gridjuv.D1pure$n.minobsinnode[i],
    bag.fraction = hyper_gridjuv.D1pure$bag.fraction[i],
    train.fraction = .99,
    n.cores = NULL, # will use all cores by default
    verbose = TRUE
  )
  
  # add min training error and trees to grid
  hyper_gridjuv.D1pure$optimal_trees[i] <- which.min(gbm.tune.juvs.D1pure_NoSB$valid.error)
  hyper_gridjuv.D1pure$min_RMSE[i] <- sqrt(min(gbm.tune.juvs.D1pure_NoSB$valid.error))
}

#the new values will now be added to the formally empty rows of the hyper_grid

head(hyper_gridjuv.D1pure)

hyper_gridjuv.D1pure %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)

caret_grid.JUV.hypergrid.D1pure <- data.frame(n.trees = 30, 
                                              interaction.depth = 1, 
                                              shrinkage = 0.3,
                                              n.minobsinnode = 10)

caret_grid.JUV.hypergrid.D1pure_NoSB <- data.frame(n.trees = 118, 
                                                   interaction.depth = 1, 
                                                   shrinkage = 0.3,
                                                   n.minobsinnode = 15)
set.seed(25)
larv.fit_gbm_caret.cv_D1pure.hyperTune_NoSB <- 
  caret::train(PercAlive ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.x + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT, method = "gbm", 
               data = Juv18.D1_train.pure_NoSB, verbose = TRUE,
               tuneGrid = caret_grid.JUV.hypergrid.D1pure_NoSB, trControl = fitControl_rando, na.action=na.omit)
plotres(larv.fit_gbm_caret.cv_D1pure.hyperTune_NoSB)
summary(larv.fit_gbm_caret.cv_D1pure.hyperTune_NoSB$finalModel, plotit = F)
plotmo(larv.fit_gbm_caret.cv_D1pure.hyperTune_NoSB, pmethod="partdep", all1=TRUE, all2=TRUE)

###########################################################
#No DR D1 purebreds
Raw_juv2018_filt_temp.metrics5_D1_pure_NoDR<-Raw_juv2018_filt_temp.metrics5_D1 %>% 
  filter(Family %in% c(" BKxBK   ", " LSxLS   "," CUxCU   "," SBxSB   "))


###Summary 
set.seed(88901)
JuvD1.pure_splits_NoDR <- initial_split(Raw_juv2018_filt_temp.metrics5_D1_pure_NoDR, .99, strata = "PercAlive", na.rm=TRUE) 
set.seed(88901)
Juv18.D1_train.pure_NoDR <- training(JuvD1.pure_splits_NoDR)
set.seed(88901)
Juv18.D1_test.pure_NoDR <- testing(JuvD1.pure_splits_NoDR)

####
hyper_gridjuv.D1pure <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.8, 1),
  optimal_trees = 0,               
  min_RMSE = 0                     
)

# total number of combinations
nrow(hyper_gridjuv.D1pure)
## [1] 54

for(i in 1:nrow(hyper_gridjuv.D1pure)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune.juvs.D1pure_NoDR <- gbm(
    formula = PercAlive ~ ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.x + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT,
    distribution = "gaussian",
    data = Juv18.D1_train.pure_NoDR,
    n.trees = 3000,
    interaction.depth = hyper_gridjuv.D1pure$interaction.depth[i],
    shrinkage = hyper_gridjuv.D1pure$shrinkage[i],
    n.minobsinnode = hyper_gridjuv.D1pure$n.minobsinnode[i],
    bag.fraction = hyper_gridjuv.D1pure$bag.fraction[i],
    train.fraction = .99,
    n.cores = NULL, # will use all cores by default
    verbose = TRUE
  )
  
  # add min training error and trees to grid
  hyper_gridjuv.D1pure$optimal_trees[i] <- which.min(gbm.tune.juvs.D1pure_NoDR$valid.error)
  hyper_gridjuv.D1pure$min_RMSE[i] <- sqrt(min(gbm.tune.juvs.D1pure_NoDR$valid.error))
}

#the new values will now be added to the formally empty rows of the hyper_grid

head(hyper_gridjuv.D1pure)

hyper_gridjuv.D1pure %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)

caret_grid.JUV.hypergrid.D1pure <- data.frame(n.trees = 30, 
                                              interaction.depth = 1, 
                                              shrinkage = 0.3,
                                              n.minobsinnode = 10)

caret_grid.JUV.hypergrid.D1pure_NoDR <- data.frame(n.trees = 2807, 
                                                   interaction.depth = 1, 
                                                   shrinkage = 0.3,
                                                   n.minobsinnode = 15)
set.seed(25)
larv.fit_gbm_caret.cv_D1pure.hyperTune_NoDR <- 
  caret::train(PercAlive ~ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.x + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT, method = "gbm", 
               data = Juv18.D1_train.pure_NoDR, verbose = TRUE,
               tuneGrid = caret_grid.JUV.hypergrid.D1pure_NoDR, trControl = fitControl_rando, na.action=na.omit)
plotres(larv.fit_gbm_caret.cv_D1pure.hyperTune_NoDR)
summary(larv.fit_gbm_caret.cv_D1pure.hyperTune_NoDR$finalModel, plotit = F)
plotmo(larv.fit_gbm_caret.cv_D1pure.hyperTune_NoDR, pmethod="partdep", all1=TRUE, all2=TRUE)


###########################################
############################################
#####################(4 Hybrid D1 drop one)####-----
##no BK D1 hyb
Raw_juv2018_filt_temp.metrics5_D1_hyb_noBK<-Raw_juv2018_filt_temp.metrics5_D1 %>% 
  filter(Family %in% c(" CUxDR   ", 
                       " CuxLS   ", " CUxSB   ", " DRxCU   ", " DRxSB   ", 
                       " LSxCU   ", " LSxSB   ", " SBxDR   ", " SBxLS   ", " CUxLS   "))

set.seed(88901)
JuvD1.hyb_splits_noBK <- initial_split(Raw_juv2018_filt_temp.metrics5_D1_hyb_noBK, .99, strata = "PercAlive", na.rm=TRUE) 
set.seed(88901)
Juv18.D1_train.hyb_noBK <- training(JuvD1.hyb_splits_noBK)
set.seed(88901)
Juv18.D1_test.hyb_noBK <- testing(JuvD1.hyb_splits_noBK)

hyper_gridjuv.D1hyb <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# total number of combinations
nrow(hyper_gridjuv.D1hyb)
## [1] 54

for(i in 1:nrow(hyper_gridjuv.D1hyb)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune.juvs.D1hyb_noBK <- gbm(
    formula = PercAlive ~ ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.x + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT,
    distribution = "gaussian",
    data = Juv18.D1_train.hyb_noBK, 
    n.trees = 3000,
    interaction.depth = hyper_gridjuv.D1hyb$interaction.depth[i],
    shrinkage = hyper_gridjuv.D1hyb$shrinkage[i],
    n.minobsinnode = hyper_gridjuv.D1hyb$n.minobsinnode[i],
    bag.fraction = hyper_gridjuv.D1hyb$bag.fraction[i],
    train.fraction = .99,
    n.cores = NULL, # will use all cores by default
    verbose = TRUE
  )
  
  # add min training error and trees to grid
  hyper_gridjuv.D1hyb$optimal_trees[i] <- which.min(gbm.tune.juvs.D1hyb_noBK$valid.error)
  hyper_gridjuv.D1hyb$min_RMSE[i] <- sqrt(min(gbm.tune.juvs.D1hyb_noBK$valid.error))
}

#the new values will now be added to the formally empty rows of the hyper_grid

head(hyper_gridjuv.D1hyb)

hyper_gridjuv.D1hyb %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)


caret_grid.JUV.hypergrid_D1hyb <- data.frame(n.trees = 50, interaction.depth = 1, shrinkage = 0.3,
                                             n.minobsinnode = 10)

caret_grid.JUV.hypergrid_D1hyb_noBK <- data.frame(n.trees = 95, interaction.depth = 1, shrinkage = 0.3,
                                             n.minobsinnode = 15)

set.seed(25)
larv.fit_gbm_caret.cv_D1hyb.hyperTune_noBK <- 
  caret::train(PercAlive ~ ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.x + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation + DRT, method = "gbm", 
               data = Juv18.D1_train.hyb_noBK, verbose = TRUE,
               tuneGrid = caret_grid.JUV.hypergrid_D1hyb_noBK, trControl = fitControl_rando, na.action=na.omit)
summary(larv.fit_gbm_caret.cv_D1hyb.hyperTune_noBK$finalModel, plotit = TRUE)
plotres(larv.fit_gbm_caret.cv_D1hyb.hyperTune_noBK) 
plotmo(larv.fit_gbm_caret.cv_D1hyb.hyperTune_noBK, pmethod="partdep", all1=TRUE, all2=TRUE)

#####
##No CU D1 hybrids
Raw_juv2018_filt_temp.metrics5_D1_hyb<-Raw_juv2018_filt_temp.metrics5_D1 %>% 
  filter(Family %in% c(" BKxCU   ", " BKxDR   ", " BKxLS   ", " BKxSB   ", " CUxBK   ", " CUxDR   ", 
                       " CuxLS   ", " CUxSB   ", " DRxBK   ", " DRxCU   ", " DRxSB   ", " LSxBK   ", 
                       " LSxCU   ", " LSxSB   ", " SBxBK   ", " SBxDR   ", " SBxLS   ", " CUxLS   "))

Raw_juv2018_filt_temp.metrics5_D1_hyb_noCU<-Raw_juv2018_filt_temp.metrics5_D1 %>% 
  filter(Family %in% c(" BKxDR   ", " BKxLS   ", " BKxSB   ", 
                       " DRxBK   ", " DRxSB   ", " LSxBK   ", 
                       " LSxSB   ", " SBxBK   ", " SBxDR   ", " SBxLS   "))

set.seed(88901)
JuvD1.hyb_splits_noCU <- initial_split(Raw_juv2018_filt_temp.metrics5_D1_hyb_noCU, .99, strata = "PercAlive", na.rm=TRUE) 
set.seed(88901)
Juv18.D1_train.hyb_noCU <- training(JuvD1.hyb_splits_noCU)
set.seed(88901)
Juv18.D1_test.hyb_noCU <- testing(JuvD1.hyb_splits_noCU)

hyper_gridjuv.D1hyb <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# total number of combinations
nrow(hyper_gridjuv.D1hyb)
## [1] 54

for(i in 1:nrow(hyper_gridjuv.D1hyb)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune.juvs.D1hyb_noCU <- gbm(
    formula = PercAlive ~ ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.x + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT,
    distribution = "gaussian",
    data = Juv18.D1_train.hyb_noCU, 
    n.trees = 3000,
    interaction.depth = hyper_gridjuv.D1hyb$interaction.depth[i],
    shrinkage = hyper_gridjuv.D1hyb$shrinkage[i],
    n.minobsinnode = hyper_gridjuv.D1hyb$n.minobsinnode[i],
    bag.fraction = hyper_gridjuv.D1hyb$bag.fraction[i],
    train.fraction = .99,
    n.cores = NULL, # will use all cores by default
    verbose = TRUE
  )
  
  # add min training error and trees to grid
  hyper_gridjuv.D1hyb$optimal_trees[i] <- which.min(gbm.tune.juvs.D1hyb_noCU$valid.error)
  hyper_gridjuv.D1hyb$min_RMSE[i] <- sqrt(min(gbm.tune.juvs.D1hyb_noCU$valid.error))
}

#the new values will now be added to the formally empty rows of the hyper_grid

head(hyper_gridjuv.D1hyb)

hyper_gridjuv.D1hyb %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)


caret_grid.JUV.hypergrid_D1hyb <- data.frame(n.trees = 50, interaction.depth = 1, shrinkage = 0.3,
                                             n.minobsinnode = 10)

caret_grid.JUV.hypergrid_D1hyb_noCU <- data.frame(n.trees = 1328, interaction.depth = 1, shrinkage = 0.3,
                                                  n.minobsinnode = 5)

set.seed(25)
larv.fit_gbm_caret.cv_D1hyb.hyperTune_noCU <- 
  caret::train(PercAlive ~ ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.x + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation + DRT, method = "gbm", 
               data = Juv18.D1_train.hyb_noCU, verbose = TRUE,
               tuneGrid = caret_grid.JUV.hypergrid_D1hyb_noCU, trControl = fitControl_rando, na.action=na.omit)
summary(larv.fit_gbm_caret.cv_D1hyb.hyperTune_noCU$finalModel, plotit = TRUE)
plotres(larv.fit_gbm_caret.cv_D1hyb.hyperTune_noCU) 
plotmo(larv.fit_gbm_caret.cv_D1hyb.hyperTune_noCU, pmethod="partdep", all1=TRUE, all2=TRUE)

########################
##No SB D1 hybrids
Raw_juv2018_filt_temp.metrics5_D1_hyb<-Raw_juv2018_filt_temp.metrics5_D1 %>% 
  filter(Family %in% c(" BKxCU   ", " BKxDR   ", " BKxLS   ", " BKxSB   ", " CUxBK   ", " CUxDR   ", 
                       " CuxLS   ", " CUxSB   ", " DRxBK   ", " DRxCU   ", " DRxSB   ", " LSxBK   ", 
                       " LSxCU   ", " LSxSB   ", " SBxBK   ", " SBxDR   ", " SBxLS   ", " CUxLS   "))

Raw_juv2018_filt_temp.metrics5_D1_hyb_SB<-Raw_juv2018_filt_temp.metrics5_D1 %>% 
  filter(Family %in% c(" BKxCU   ", " BKxDR   ", " BKxLS   ", " CUxBK   ", " CUxDR   ", 
                       " CuxLS   ", " DRxBK   ", " DRxCU   ", " LSxBK   ", 
                       " LSxCU   ", " CUxLS   "))

set.seed(88901)
JuvD1.hyb_splits_SB <- initial_split(Raw_juv2018_filt_temp.metrics5_D1_hyb_SB, .99, strata = "PercAlive", na.rm=TRUE) 
set.seed(88901)
Juv18.D1_train.hyb_SB <- training(JuvD1.hyb_splits_SB)
set.seed(88901)
Juv18.D1_test.hyb_SB <- testing(JuvD1.hyb_splits_SB)

hyper_gridjuv.D1hyb <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# total number of combinations
nrow(hyper_gridjuv.D1hyb)
## [1] 54

for(i in 1:nrow(hyper_gridjuv.D1hyb)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune.juvs.D1hyb_SB <- gbm(
    formula = PercAlive ~ ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.x + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT,
    distribution = "gaussian",
    data = Juv18.D1_train.hyb_SB, 
    n.trees = 3000,
    interaction.depth = hyper_gridjuv.D1hyb$interaction.depth[i],
    shrinkage = hyper_gridjuv.D1hyb$shrinkage[i],
    n.minobsinnode = hyper_gridjuv.D1hyb$n.minobsinnode[i],
    bag.fraction = hyper_gridjuv.D1hyb$bag.fraction[i],
    train.fraction = .99,
    n.cores = NULL, # will use all cores by default
    verbose = TRUE
  )
  
  # add min training error and trees to grid
  hyper_gridjuv.D1hyb$optimal_trees[i] <- which.min(gbm.tune.juvs.D1hyb_SB$valid.error)
  hyper_gridjuv.D1hyb$min_RMSE[i] <- sqrt(min(gbm.tune.juvs.D1hyb_SB$valid.error))
}

#the new values will now be added to the formally empty rows of the hyper_grid

head(hyper_gridjuv.D1hyb)

hyper_gridjuv.D1hyb %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)


caret_grid.JUV.hypergrid_D1hyb <- data.frame(n.trees = 50, interaction.depth = 1, shrinkage = 0.3,
                                             n.minobsinnode = 10)

caret_grid.JUV.hypergrid_D1hyb_SB <- data.frame(n.trees = 2190, interaction.depth = 1, shrinkage = 0.3,
                                                  n.minobsinnode = 5)

set.seed(25)
larv.fit_gbm_caret.cv_D1hyb.hyperTune_SB <- 
  caret::train(PercAlive ~ ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.x + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation + DRT, method = "gbm", 
               data = Juv18.D1_train.hyb_SB, verbose = TRUE,
               tuneGrid = caret_grid.JUV.hypergrid_D1hyb_SB, trControl = fitControl_rando, na.action=na.omit)
summary(larv.fit_gbm_caret.cv_D1hyb.hyperTune_SB$finalModel, plotit = TRUE)
plotres(larv.fit_gbm_caret.cv_D1hyb.hyperTune_SB) 
plotmo(larv.fit_gbm_caret.cv_D1hyb.hyperTune_SB, pmethod="partdep", all1=TRUE, all2=TRUE)

########################
##No LS D1 hybrids
Raw_juv2018_filt_temp.metrics5_D1_hyb<-Raw_juv2018_filt_temp.metrics5_D1 %>% 
  filter(Family %in% c(" BKxCU   ", " BKxDR   ", " BKxLS   ", " BKxSB   ", " CUxBK   ", " CUxDR   ", 
                       " CuxLS   ", " CUxSB   ", " DRxBK   ", " DRxCU   ", " DRxSB   ", " LSxBK   ", 
                       " LSxCU   ", " LSxSB   ", " SBxBK   ", " SBxDR   ", " SBxLS   ", " CUxLS   "))

Raw_juv2018_filt_temp.metrics5_D1_hyb_noLS<-Raw_juv2018_filt_temp.metrics5_D1 %>% 
  filter(Family %in% c(" BKxCU   ", " BKxDR   ", " BKxSB   ", " CUxBK   ", " CUxDR   ", 
                        " CUxSB   ", " DRxBK   ", " DRxCU   ", " DRxSB   ", " SBxBK   ", " SBxDR   "))

set.seed(88901)
JuvD1.hyb_splits_noLS <- initial_split(Raw_juv2018_filt_temp.metrics5_D1_hyb_noLS, .99, strata = "PercAlive", na.rm=TRUE) 
set.seed(88901)
Juv18.D1_train.hyb_noLS <- training(JuvD1.hyb_splits_noLS)
set.seed(88901)
Juv18.D1_test.hyb_noLS <- testing(JuvD1.hyb_splits_noLS)

hyper_gridjuv.D1hyb <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# total number of combinations
nrow(hyper_gridjuv.D1hyb)
## [1] 54

for(i in 1:nrow(hyper_gridjuv.D1hyb)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune.juvs.D1hyb_noLS <- gbm(
    formula = PercAlive ~ ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.x + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT,
    distribution = "gaussian",
    data = Juv18.D1_train.hyb_noLS, 
    n.trees = 3000,
    interaction.depth = hyper_gridjuv.D1hyb$interaction.depth[i],
    shrinkage = hyper_gridjuv.D1hyb$shrinkage[i],
    n.minobsinnode = hyper_gridjuv.D1hyb$n.minobsinnode[i],
    bag.fraction = hyper_gridjuv.D1hyb$bag.fraction[i],
    train.fraction = .99,
    n.cores = NULL, # will use all cores by default
    verbose = TRUE
  )
  
  # add min training error and trees to grid
  hyper_gridjuv.D1hyb$optimal_trees[i] <- which.min(gbm.tune.juvs.D1hyb_noLS$valid.error)
  hyper_gridjuv.D1hyb$min_RMSE[i] <- sqrt(min(gbm.tune.juvs.D1hyb_noLS$valid.error))
}

#the new values will now be added to the formally empty rows of the hyper_grid

head(hyper_gridjuv.D1hyb)

hyper_gridjuv.D1hyb %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)


caret_grid.JUV.hypergrid_D1hyb <- data.frame(n.trees = 50, interaction.depth = 1, shrinkage = 0.3,
                                             n.minobsinnode = 10)

caret_grid.JUV.hypergrid_D1hyb_noLS <- data.frame(n.trees = 1, interaction.depth = 1, shrinkage = 0.01,
                                                n.minobsinnode = 5)

set.seed(25)
larv.fit_gbm_caret.cv_D1hyb.hyperTune_noLS <- 
  caret::train(PercAlive ~ ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.x + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation + DRT, method = "gbm", 
               data = Juv18.D1_train.hyb_noLS, verbose = TRUE,
               tuneGrid = caret_grid.JUV.hypergrid_D1hyb_noLS, trControl = fitControl_rando, na.action=na.omit)
summary(larv.fit_gbm_caret.cv_D1hyb.hyperTune_noLS$finalModel, plotit = TRUE)
plotres(larv.fit_gbm_caret.cv_D1hyb.hyperTune_noLS) 
plotmo(larv.fit_gbm_caret.cv_D1hyb.hyperTune_noLS, pmethod="partdep", all1=TRUE, all2=TRUE)

########################
##No LS D1 hybrids
Raw_juv2018_filt_temp.metrics5_D1_hyb<-Raw_juv2018_filt_temp.metrics5_D1 %>% 
  filter(Family %in% c(" BKxCU   ", " BKxDR   ", " BKxLS   ", " BKxSB   ", " CUxBK   ", " CUxDR   ", 
                       " CuxLS   ", " CUxSB   ", " DRxBK   ", " DRxCU   ", " DRxSB   ", " LSxBK   ", 
                       " LSxCU   ", " LSxSB   ", " SBxBK   ", " SBxDR   ", " SBxLS   ", " CUxLS   "))

Raw_juv2018_filt_temp.metrics5_D1_hyb_noDR<-Raw_juv2018_filt_temp.metrics5_D1 %>% 
  filter(Family %in% c(" BKxCU   ", " BKxLS   ", " BKxSB   ", " CUxBK   ", 
                       " CuxLS   ", " CUxSB   ", " LSxBK   ", 
                       " LSxCU   ", " LSxSB   ", " SBxBK   ", " SBxLS   ", " CUxLS   "))

set.seed(88901)
JuvD1.hyb_splits_noDR <- initial_split(Raw_juv2018_filt_temp.metrics5_D1_hyb_noDR, .99, strata = "PercAlive", na.rm=TRUE) 
set.seed(88901)
Juv18.D1_train.hyb_noDR <- training(JuvD1.hyb_splits_noDR)
set.seed(88901)
Juv18.D1_test.hyb_noDR <- testing(JuvD1.hyb_splits_noDR)

hyper_gridjuv.D1hyb <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# total number of combinations
nrow(hyper_gridjuv.D1hyb)
## [1] 54

for(i in 1:nrow(hyper_gridjuv.D1hyb)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune.juvs.D1hyb_noDR <- gbm(
    formula = PercAlive ~ ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.x + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation +DRT,
    distribution = "gaussian",
    data = Juv18.D1_train.hyb_noDR, 
    n.trees = 3000,
    interaction.depth = hyper_gridjuv.D1hyb$interaction.depth[i],
    shrinkage = hyper_gridjuv.D1hyb$shrinkage[i],
    n.minobsinnode = hyper_gridjuv.D1hyb$n.minobsinnode[i],
    bag.fraction = hyper_gridjuv.D1hyb$bag.fraction[i],
    train.fraction = .99,
    n.cores = NULL, # will use all cores by default
    verbose = TRUE
  )
  
  # add min training error and trees to grid
  hyper_gridjuv.D1hyb$optimal_trees[i] <- which.min(gbm.tune.juvs.D1hyb_noDR$valid.error)
  hyper_gridjuv.D1hyb$min_RMSE[i] <- sqrt(min(gbm.tune.juvs.D1hyb_noDR$valid.error))
}

#the new values will now be added to the formally empty rows of the hyper_grid

head(hyper_gridjuv.D1hyb)

hyper_gridjuv.D1hyb %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)


caret_grid.JUV.hypergrid_D1hyb <- data.frame(n.trees = 50, interaction.depth = 1, shrinkage = 0.3,
                                             n.minobsinnode = 10)

caret_grid.JUV.hypergrid_D1hyb_noDR <- data.frame(n.trees = 178, interaction.depth = 3, shrinkage = 0.10,
                                                  n.minobsinnode = 5)

set.seed(25)
larv.fit_gbm_caret.cv_D1hyb.hyperTune_noDR <- 
  caret::train(PercAlive ~ ts_frame...2.dim.ts_frame..2...x. + Temp + Lat + sd.x + TSA_DHWStandardDeviation + SSTA_FrequencyStandardDeviation + DRT, method = "gbm", 
               data = Juv18.D1_train.hyb_noDR, verbose = TRUE,
               tuneGrid = caret_grid.JUV.hypergrid_D1hyb_noDR, trControl = fitControl_rando, na.action=na.omit)
summary(larv.fit_gbm_caret.cv_D1hyb.hyperTune_noDR$finalModel, plotit = TRUE)
plotres(larv.fit_gbm_caret.cv_D1hyb.hyperTune_noDR) 
plotmo(larv.fit_gbm_caret.cv_D1hyb.hyperTune_noDR, pmethod="partdep", all1=TRUE, all2=TRUE)
#############################################
#############################################
#############################################
#############################################

##larv purebreds plot----
larv.pureTSA.1_noBK<- larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_noBK %>% partial(pred.var = "TSA_DHWStandardDeviation") %>% autoplot(smooth = TRUE, smooth.method="auto", ylab = expression(f(TSA))) + theme_light() + ggtitle("ggplot2-based PDP") #library pdp
x.larv.pureTSA.1_noBK<-ggplot_build(larv.pureTSA.1_noBK)$data[[2]]$x
y.larv.pureTSA.1_noBK<-ggplot_build(larv.pureTSA.1_noBK)$data[[2]]$y
TSA.larv.pureTSA.1_noBK<-data.frame(a4=ggplot_build(larv.pureTSA.1_noBK)$data[[2]]$x,b4=ggplot_build(larv.pureTSA.1_noBK)$data[[2]]$y)
TSA.larv.pureTSA.1_noBK$category <- "larvpure"
TSA.larv.pureTSA.1_noBK$category_drop <- "NoBacknumbers"

larv.pureTSA.1_noCU<- larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom.NoCU %>% partial(pred.var = "TSA_DHWStandardDeviation") %>% autoplot(smooth = TRUE, smooth.method="auto", ylab = expression(f(TSA))) + theme_light() + ggtitle("ggplot2-based PDP") #library pdp
x.larv.pureTSA.1_noCU<-ggplot_build(larv.pureTSA.1_noCU)$data[[2]]$x
y.larv.pureTSA.1_noCU<-ggplot_build(larv.pureTSA.1_noCU)$data[[2]]$y
TSA.larv.pureTSA.1_noCU<-data.frame(a4=ggplot_build(larv.pureTSA.1_noCU)$data[[2]]$x,b4=ggplot_build(larv.pureTSA.1_noCU)$data[[2]]$y)
TSA.larv.pureTSA.1_noCU$category <- "larvpure"
TSA.larv.pureTSA.1_noCU$category_drop <- "NoCurd"

larv.pureTSA.1_noSB<- larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_noSB %>% partial(pred.var = "TSA_DHWStandardDeviation") %>% autoplot(smooth = TRUE, smooth.method="auto", 
                                                                                                         ylab = expression(f(TSA))) + theme_light() + ggtitle("ggplot2-based PDP") #library pdp
x.larv.pureTSA.1_noSB<-ggplot_build(larv.pureTSA.1_noSB)$data[[2]]$x
y.larv.pureTSA.1_noSB<-ggplot_build(larv.pureTSA.1_noSB)$data[[2]]$y
TSA.larv.pureTSA.1_noSB<-data.frame(a4=ggplot_build(larv.pureTSA.1_noSB)$data[[2]]$x,b4=ggplot_build(larv.pureTSA.1_noSB)$data[[2]]$y)
TSA.larv.pureTSA.1_noSB$category <- "larvpure"
TSA.larv.pureTSA.1_noSB$category_drop <- "NoSandbank"

larv.pureTSA.1_noLS<- larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_noLS %>% partial(pred.var = "TSA_DHWStandardDeviation") %>% autoplot(smooth = TRUE, smooth.method="auto", 
                                                                                                         ylab = expression(f(TSA))) + theme_light() + ggtitle("ggplot2-based PDP") #library pdp
x.larv.pureTSA.1_noLS<-ggplot_build(larv.pureTSA.1_noLS)$data[[2]]$x
y.larv.pureTSA.1_noLS<-ggplot_build(larv.pureTSA.1_noLS)$data[[2]]$y
TSA.larv.pureTSA.1_noLS<-data.frame(a4=ggplot_build(larv.pureTSA.1_noLS)$data[[2]]$x,b4=ggplot_build(larv.pureTSA.1_noLS)$data[[2]]$y)
TSA.larv.pureTSA.1_noLS$category <- "larvpure"
TSA.larv.pureTSA.1_noLS$category_drop <- "NoLongSandy"

larv.pureTSA.1_noDR<-  larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom_noDV%>% partial(pred.var = "TSA_DHWStandardDeviation") %>% autoplot(smooth = TRUE, smooth.method="auto", 
                                                                                                         ylab = expression(f(TSA))) + theme_light() + ggtitle("ggplot2-based PDP") #library pdp
x.larv.pureTSA.1_noDR<-ggplot_build(larv.pureTSA.1_noDR)$data[[2]]$x
y.larv.pureTSA.1_noDR<-ggplot_build(larv.pureTSA.1_noDR)$data[[2]]$y
larv.pureTSA.1_noDR.df<-data.frame(a4=ggplot_build(larv.pureTSA.1_noDR)$data[[2]]$x,b4=ggplot_build(larv.pureTSA.1_noDR)$data[[2]]$y)
larv.pureTSA.1_noDR.df$category <- "larvpure"
larv.pureTSA.1_noDR.df$category_drop <- "NoDavies"
head(larv.pureTSA.1_noDR.df)

x2<-larv.fit_gbm_caret.cv_pure.hyperTuneRAW.NatCom %>% partial(pred.var = "TSA_DHWStandardDeviation") %>% autoplot(smooth = TRUE, smooth.method="auto", smooth.span= 0.5, ylab = expression(f(TSA_DHWStandardDeviation))) + theme_light() + ggtitle("ggplot2-based PDP")
x.larvpur<-ggplot_build(x2)$data[[2]]$x
y.larvpur<-ggplot_build(x2)$data[[2]]$y
TSA.larvbuild.Allreefs<-data.frame(a4=ggplot_build(x2)$data[[2]]$x,b4=ggplot_build(x2)$data[[2]]$y)
TSA.larvbuild.Allreefs$category <- "larvpure"
TSA.larvbuild.Allreefs$category_drop <- "Allreefs"

larv.pure.TSA.DropReefs.df<-rbind(TSA.larv.pureTSA.1_noBK, 
                                  TSA.larv.pureTSA.1_noCU, 
                                  TSA.larv.pureTSA.1_noSB, 
                                  TSA.larv.pureTSA.1_noLS, 
                                  larv.pureTSA.1_noDR.df,
                                  TSA.larvbuild.Allreefs)
formula.l.p <- y ~ poly(x, 3) 
(larv.pure.TSA.DropReefs<-
    ggplot(larv.pure.TSA.DropReefs.df, aes(a4, b4)) +
    geom_point(aes(colour=category_drop)) + facet_wrap(.~category_drop, scales = "free_y")+
    scale_colour_manual(values=c("NoLongSandy"="#F6CCA0","NoCurd"="#BB8996", 
                                 "NoSandbank"="#616594", "NoBacknumbers"="#0E0A0E", "NoDavies"="#55C1E2", "Allreefs"= "#A1C7D3"))+
    theme_bw()+stat_smooth(aes(), 
                           method = "lm", formula = formula.l.p , color="black")+
    xlab("TSA")+ylab("Predicted survival (%)"))#+geom_vline(xintercept = 1.08)+


#############################################
#############################################

##larv hybrids plot----
larv.hybDRT.1_noBK<- larv.fit_gbm_caret.cv_HYB.hyperTune.noBK %>% partial(pred.var = "DRT") %>% autoplot(smooth = TRUE, smooth.method="auto", ylab = expression(f(TSA))) + theme_light() + ggtitle("ggplot2-based PDP") #library pdp
x.larv.hybDRT.1_noBK<-ggplot_build(larv.hybDRT.1_noBK)$data[[2]]$x
y.larv.hybDRT.1_noBK<-ggplot_build(larv.hybDRT.1_noBK)$data[[2]]$y
DRT.larv.hybDRT.1_noBK<-data.frame(a4=ggplot_build(larv.hybDRT.1_noBK)$data[[2]]$x,b4=ggplot_build(larv.hybDRT.1_noBK)$data[[2]]$y)
DRT.larv.hybDRT.1_noBK$category <- "larvhyb"
DRT.larv.hybDRT.1_noBK$category_drop <- "NoBacknumbers"

larv.hybDRT.1_noCU<- larv.fit_gbm_caret.cv_HYB.hyperTune.noCU %>% partial(pred.var = "DRT") %>% autoplot(smooth = TRUE, smooth.method="auto", ylab = expression(f(TSA))) + theme_light() + ggtitle("ggplot2-based PDP") #library pdp
x.larv.hybDRT.1_noCU<-ggplot_build(larv.hybDRT.1_noCU)$data[[2]]$x
y.larv.hybDRT.1_noCU<-ggplot_build(larv.hybDRT.1_noCU)$data[[2]]$y
DRT.larv.hybDRT.1_noCU<-data.frame(a4=ggplot_build(larv.hybDRT.1_noCU)$data[[2]]$x,b4=ggplot_build(larv.hybDRT.1_noCU)$data[[2]]$y)
DRT.larv.hybDRT.1_noCU$category <- "larvhyb"
DRT.larv.hybDRT.1_noCU$category_drop <- "NoCurd"

larv.hybDRT.1_noSB<- larv.fit_gbm_caret.cv_HYB.hyperTune.noSB %>% partial(pred.var = "DRT") %>% autoplot(smooth = TRUE, smooth.method="auto", 
                                                                                                                            ylab = expression(f(TSA))) + theme_light() + ggtitle("ggplot2-based PDP") #library pdp
x.larv.hybDRT.1_noSB<-ggplot_build(larv.hybDRT.1_noSB)$data[[2]]$x
y.larv.hybDRT.1_noSB<-ggplot_build(larv.hybDRT.1_noSB)$data[[2]]$y
DRT.larv.hybDRT.1_noSB<-data.frame(a4=ggplot_build(larv.hybDRT.1_noSB)$data[[2]]$x,b4=ggplot_build(larv.hybDRT.1_noSB)$data[[2]]$y)
DRT.larv.hybDRT.1_noSB$category <- "larvhyb"
DRT.larv.hybDRT.1_noSB$category_drop <- "NoSandbank"

larv.hybDRT.1_noLS<- larv.fit_gbm_caret.cv_HYB.hyperTune.noLS %>% partial(pred.var = "DRT") %>% autoplot(smooth = TRUE, smooth.method="auto", 
                                                                                                                              ylab = expression(f(TSA))) + theme_light() + ggtitle("ggplot2-based PDP") #library pdp
x.larv.hybDRT.1_noLS<-ggplot_build(larv.hybDRT.1_noLS)$data[[2]]$x
y.larv.hybDRT.1_noLS<-ggplot_build(larv.hybDRT.1_noLS)$data[[2]]$y
DRT.larv.hybDRT.1_noLS<-data.frame(a4=ggplot_build(larv.hybDRT.1_noLS)$data[[2]]$x,b4=ggplot_build(larv.hybDRT.1_noLS)$data[[2]]$y)
DRT.larv.hybDRT.1_noLS$category <- "larvhyb"
DRT.larv.hybDRT.1_noLS$category_drop <- "NoLongSandy"

larv.hybDRT.1_noDR<- larv.fit_gbm_caret.cv_HYB.hyperTune.noDV %>% partial(pred.var = "DRT") %>% autoplot(smooth = TRUE, smooth.method="auto", 
                                                                                                                              ylab = expression(f(TSA))) + theme_light() + ggtitle("ggplot2-based PDP") #library pdp
x.larv.hybDRT.1_noDR<-ggplot_build(larv.hybDRT.1_noDR)$data[[2]]$x
y.larv.hybDRT.1_noDR<-ggplot_build(larv.hybDRT.1_noDR)$data[[2]]$y
larv.hybDRT.1_noDR.df<-data.frame(a4=ggplot_build(larv.hybDRT.1_noDR)$data[[2]]$x,b4=ggplot_build(larv.hybDRT.1_noDR)$data[[2]]$y)
larv.hybDRT.1_noDR.df$category <- "larvhyb"
larv.hybDRT.1_noDR.df$category_drop <- "NoDavies"
head(larv.hybDRT.1_noDR.df)

larv.hybDRT.1<-larv.fit_gbm_caret.cv_HYB.hyperTune %>% partial(pred.var = "DRT") %>% autoplot(smooth = TRUE, smooth.method="auto", smooth.span= 0.5, ylab = expression(f(DRT))) + theme_light() + ggtitle("ggplot2-based PDP") #library pdp
x.larvDRT<-ggplot_build(larv.hybDRT.1)$data[[2]]$x
y.larvDRT<-ggplot_build(larv.hybDRT.1)$data[[2]]$y
DRT.larvbuild.Allreefs<-data.frame(a4=ggplot_build(larv.hybDRT.1)$data[[2]]$x,b4=ggplot_build(larv.hybDRT.1)$data[[2]]$y)
DRT.larvbuild.Allreefs$category <- "larvhyb"
DRT.larvbuild.Allreefs$category_drop <- "Allreefs"

larv.hyb.DRT.DropReefs.df<-rbind(DRT.larv.hybDRT.1_noBK, 
                    DRT.larv.hybDRT.1_noCU, 
                    DRT.larv.hybDRT.1_noSB, 
                    DRT.larv.hybDRT.1_noLS, 
                    larv.hybDRT.1_noDR.df,
                    DRT.larvbuild.Allreefs)

formula.drt.larv <- y ~ poly(x, 5, raw = TRUE)
(larv.hyb.DRT.DropReefs<-
    ggplot(larv.hyb.DRT.DropReefs.df, aes(a4, b4)) +
    geom_point(aes(colour=category_drop)) + facet_wrap(.~category_drop, scales = "free_y")+
    scale_colour_manual(values=c("NoLongSandy"="#F6CCA0","NoCurd"="#BB8996", 
                                 "NoSandbank"="#616594", "NoBacknumbers"="#0E0A0E", "NoDavies"="#55C1E2", "Allreefs"= "#A1C7D3"))+
    theme_bw()+stat_smooth(aes(), 
                           method = "lm", formula = formula.drt.larv, color="black")+
    xlab("DRT")+ylab("Predicted survival (%)"))



#####################################3
#####################################3
##D1 purebred juvenile plot----
juv.purets_.1_noBK<-larv.fit_gbm_caret.cv_D1pure.hyperTune_NoBk %>% partial(pred.var = "ts_frame...2.dim.ts_frame..2...x.") %>% autoplot(smooth = TRUE, smooth.method="auto", ylab = expression(f(TSA))) + theme_light() + ggtitle("ggplot2-based PDP") #library pdp
x.juv.purets_.1_noBK<-ggplot_build(juv.purets_.1_noBK)$data[[2]]$x
y.juv.purets_.1_noBK<-ggplot_build(juv.purets_.1_noBK)$data[[2]]$y
ts_.juv.purets_.1_noBK<-data.frame(a4=ggplot_build(juv.purets_.1_noBK)$data[[2]]$x,b4=ggplot_build(juv.purets_.1_noBK)$data[[2]]$y)
ts_.juv.purets_.1_noBK$category <- "juvpure"
ts_.juv.purets_.1_noBK$category_drop <- "NoBacknumbers"

juv.purets_.1_noCU<-larv.fit_gbm_caret.cv_D1pure.hyperTune_NoCU %>% partial(pred.var = "ts_frame...2.dim.ts_frame..2...x.") %>% autoplot(smooth = TRUE, smooth.method="auto", ylab = expression(f(TSA))) + theme_light() + ggtitle("ggplot2-based PDP") #library pdp
x.juv.purets_.1_noCU<-ggplot_build(juv.purets_.1_noCU)$data[[2]]$x
y.juv.purets_.1_noCU<-ggplot_build(juv.purets_.1_noCU)$data[[2]]$y
ts_.juv.purets_.1_noCU<-data.frame(a4=ggplot_build(juv.purets_.1_noCU)$data[[2]]$x,b4=ggplot_build(juv.purets_.1_noCU)$data[[2]]$y)
ts_.juv.purets_.1_noCU$category <- "juvpure"
ts_.juv.purets_.1_noCU$category_drop <- "NoCurd"

juv.purets_.1_noSB<-larv.fit_gbm_caret.cv_D1pure.hyperTune_NoSB %>% partial(pred.var = "ts_frame...2.dim.ts_frame..2...x.") %>% autoplot(smooth = TRUE, smooth.method="auto",                                                                                                        ylab = expression(f(TSA))) + theme_light() + ggtitle("ggplot2-based PDP") #library pdp
x.juv.purets_.1_noSB<-ggplot_build(juv.purets_.1_noSB)$data[[2]]$x
y.juv.purets_.1_noSB<-ggplot_build(juv.purets_.1_noSB)$data[[2]]$y
ts_.juv.purets_.1_noSB<-data.frame(a4=ggplot_build(juv.purets_.1_noSB)$data[[2]]$x,b4=ggplot_build(juv.purets_.1_noSB)$data[[2]]$y)
ts_.juv.purets_.1_noSB$category <- "juvpure"
ts_.juv.purets_.1_noSB$category_drop <- "NoSandbank"

juv.hybts_.1_noLS<-larv.fit_gbm_caret.cv_D1pure.hyperTune_NoLS %>% partial(pred.var = "ts_frame...2.dim.ts_frame..2...x.") %>% autoplot(smooth = TRUE, smooth.method="auto",                                                                                                                         ylab = expression(f(TSA))) + theme_light() + ggtitle("ggplot2-based PDP") #library pdp
x.juv.purets_.1_noLS<-ggplot_build(juv.hybts_.1_noLS)$data[[2]]$x
y.juv.purets_.1_noLS<-ggplot_build(juv.hybts_.1_noLS)$data[[2]]$y
ts_.juv.purets_.1_noLS<-data.frame(a4=ggplot_build(juv.hybts_.1_noLS)$data[[2]]$x,b4=ggplot_build(juv.hybts_.1_noLS)$data[[2]]$y)
ts_.juv.purets_.1_noLS$category <- "juvpure"
ts_.juv.purets_.1_noLS$category_drop <- "NoLongSandy"

juv.purets_.1_noDR<-larv.fit_gbm_caret.cv_D1pure.hyperTune_NoDR %>% partial(pred.var = "ts_frame...2.dim.ts_frame..2...x.") %>% autoplot(smooth = TRUE, smooth.method="auto", 
                                                                                                                              ylab = expression(f(TSA))) + theme_light() + ggtitle("ggplot2-based PDP") #library pdp
x.juv.purets_.1_noDR<-ggplot_build(juv.purets_.1_noDR)$data[[2]]$x
y.juv.purets_.1_noDR<-ggplot_build(juv.purets_.1_noDR)$data[[2]]$y
juv.hybts_.1_noDR.df<-data.frame(a4=ggplot_build(juv.purets_.1_noDR)$data[[2]]$x,b4=ggplot_build(juv.purets_.1_noDR)$data[[2]]$y)
juv.hybts_.1_noDR.df$category <- "juvpure"
juv.hybts_.1_noDR.df$category_drop <- "NoDavies"
head(juv.hybts_.1_noDR.df)

juv.pure.D1_ts_1_all<-larv.fit_gbm_caret.cv_D1pure.hyperTune %>% partial(pred.var = "ts_frame...2.dim.ts_frame..2...x.") %>% autoplot(smooth = TRUE, smooth.method="auto", ylab = expression(f(ts_))) + theme_light() + ggtitle("ggplot2-based PDP")
x.juv.D1.ts__all<-ggplot_build(juv.pure.D1_ts_1_all)$data[[2]]$x
y.juv.D1.ts__all<-ggplot_build(juv.pure.D1_ts_1_all)$data[[2]]$y
ts_.JuvD1build_all<-data.frame(a4=x.juv.D1.ts_,b4=y.juv.D1.ts_)
ts_.JuvD1build_all$category <- "juvpure"
ts_.JuvD1build_all$category_drop <- "Allreefs"

juvD1.pure.df<-rbind(ts_.juv.purets_.1_noBK, 
                    ts_.juv.purets_.1_noCU, 
                    ts_.juv.purets_.1_noSB, 
                    ts_.juv.purets_.1_noLS, 
                    juv.hybts_.1_noDR.df,
                    ts_.JuvD1build_all)

formula_Juv_ts <- y ~ poly(x, 3, raw = TRUE)
(juv.D1pure.ts__DropReefs<-
    ggplot(juvD1.pure.df, aes(a4, b4)) +
    geom_point(aes(colour=category_drop)) + facet_wrap(.~category_drop, scales = "free_y")+
    scale_colour_manual(values=c("NoLongSandy"="#F6CCA0","NoCurd"="#BB8996", 
                                 "NoSandbank"="#616594", "NoBacknumbers"="#0E0A0E", "NoDavies"="#55C1E2", "Allreefs"= "#A1C7D3"))+
    theme_bw()+stat_smooth(aes(), 
                           method = "lm", formula = formula_Juv_ts, color="black")+
    xlab("SST_av.pre")+ylab("Predicted survival (%)"))


###################################

##D1 hybrid juvenile plot----
juv.hybDRT.1_noBK<-larv.fit_gbm_caret.cv_D1hyb.hyperTune_noBK %>% partial(pred.var = "TSA_DHWStandardDeviation") %>% autoplot(smooth = TRUE, smooth.method="auto", ylab = expression(f(TSA))) + theme_light() + ggtitle("ggplot2-based PDP") #library pdp
x.juv.hybDRT.1_noBK<-ggplot_build(juv.hybDRT.1_noBK)$data[[2]]$x
y.juv.hybDRT.1_noBK<-ggplot_build(juv.hybDRT.1_noBK)$data[[2]]$y
DRT.juv.hybDRT.1_noBK<-data.frame(a4=ggplot_build(juv.hybDRT.1_noBK)$data[[2]]$x,b4=ggplot_build(juv.hybDRT.1_noBK)$data[[2]]$y)
DRT.juv.hybDRT.1_noBK$category <- "juvhyb"
DRT.juv.hybDRT.1_noBK$category_drop <- "NoBacknumbers"

juv.hybDRT.1_noCU<-larv.fit_gbm_caret.cv_D1hyb.hyperTune_noCU %>% partial(pred.var = "TSA_DHWStandardDeviation") %>% autoplot(smooth = TRUE, smooth.method="auto", ylab = expression(f(TSA))) + theme_light() + ggtitle("ggplot2-based PDP") #library pdp
x.juv.hybDRT.1_noCU<-ggplot_build(juv.hybDRT.1_noCU)$data[[2]]$x
y.juv.hybDRT.1_noCU<-ggplot_build(juv.hybDRT.1_noCU)$data[[2]]$y
DRT.juv.hybDRT.1_noCU<-data.frame(a4=ggplot_build(juv.hybDRT.1_noCU)$data[[2]]$x,b4=ggplot_build(juv.hybDRT.1_noCU)$data[[2]]$y)
DRT.juv.hybDRT.1_noCU$category <- "juvhyb"
DRT.juv.hybDRT.1_noCU$category_drop <- "NoCurd"

juv.hybDRT.1_noSB<-larv.fit_gbm_caret.cv_D1hyb.hyperTune_SB %>% partial(pred.var = "TSA_DHWStandardDeviation") %>% autoplot(smooth = TRUE, smooth.method="auto", 
                                                                                                                            ylab = expression(f(TSA))) + theme_light() + ggtitle("ggplot2-based PDP") #library pdp
x.juv.hybDRT.1_noSB<-ggplot_build(juv.hybDRT.1_noSB)$data[[2]]$x
y.juv.hybDRT.1_noSB<-ggplot_build(juv.hybDRT.1_noSB)$data[[2]]$y
DRT.juv.hybDRT.1_noSB<-data.frame(a4=ggplot_build(juv.hybDRT.1_noSB)$data[[2]]$x,b4=ggplot_build(juv.hybDRT.1_noSB)$data[[2]]$y)
DRT.juv.hybDRT.1_noSB$category <- "juvhyb"
DRT.juv.hybDRT.1_noSB$category_drop <- "NoSandbank"

juv.hybDRT.1_noLS<-larv.fit_gbm_caret.cv_D1hyb.hyperTune_noLS %>% partial(pred.var = "TSA_DHWStandardDeviation") %>% autoplot(smooth = TRUE, smooth.method="auto", 
                                                                                                                              ylab = expression(f(TSA))) + theme_light() + ggtitle("ggplot2-based PDP") #library pdp
x.juv.hybDRT.1_noLS<-ggplot_build(juv.hybDRT.1_noLS)$data[[2]]$x
y.juv.hybDRT.1_noLS<-ggplot_build(juv.hybDRT.1_noLS)$data[[2]]$y
DRT.juv.hybDRT.1_noLS<-data.frame(a4=ggplot_build(juv.hybDRT.1_noLS)$data[[2]]$x,b4=ggplot_build(juv.hybDRT.1_noLS)$data[[2]]$y)
DRT.juv.hybDRT.1_noLS$category <- "juvhyb"
DRT.juv.hybDRT.1_noLS$category_drop <- "NoLongSandy"

juv.hybDRT.1_noDR<-larv.fit_gbm_caret.cv_D1hyb.hyperTune_noDR %>% partial(pred.var = "TSA_DHWStandardDeviation") %>% autoplot(smooth = TRUE, smooth.method="auto", 
                                                                                                                              ylab = expression(f(TSA))) + theme_light() + ggtitle("ggplot2-based PDP") #library pdp
x.juv.hybDRT.1_noDR<-ggplot_build(juv.hybDRT.1_noDR)$data[[2]]$x
y.juv.hybDRT.1_noDR<-ggplot_build(juv.hybDRT.1_noDR)$data[[2]]$y
juv.hybDRT.1_noDR.df<-data.frame(a4=ggplot_build(juv.hybDRT.1_noDR)$data[[2]]$x,b4=ggplot_build(juv.hybDRT.1_noDR)$data[[2]]$y)
juv.hybDRT.1_noDR.df$category <- "juvhyb"
juv.hybDRT.1_noDR.df$category_drop <- "NoDavies"
head(juv.hybDRT.1_noDR.df)

TSA.JuvD1.hybuild$category <- "juvhyb"
TSA.JuvD1.hybuild$category_drop <- "Allreefs"

juvD1.hyb.df<-rbind(DRT.juv.hybDRT.1_noBK, 
                   DRT.juv.hybDRT.1_noCU, 
                   DRT.juv.hybDRT.1_noSB, 
                   DRT.juv.hybDRT.1_noLS, 
                   juv.hybDRT.1_noDR.df,
                   TSA.JuvD1.hybuild)

formula.TSA <- y ~ poly(x, 3, raw = TRUE)
(juv.D1hyb.DRT_DropReefs<-
  ggplot(juvD1.hyb.df, aes(a4, b4)) +
  geom_point(aes(colour=category_drop)) + facet_wrap(.~category_drop, scales = "free_y")+
  scale_colour_manual(values=c("NoLongSandy"="#F6CCA0","NoCurd"="#BB8996", 
                             "NoSandbank"="#616594", "NoBacknumbers"="#0E0A0E", "NoDavies"="#55C1E2", "Allreefs"= "#A1C7D3"))+
                                                 theme_bw()+stat_smooth(aes(), 
                                                                        method = "lm", formula = formula.TSA, color="black")+
    xlab("TSA_DHW_stdev")+ylab("Predicted survival (%)"))

###
larv.pure.TSA.DropReefs + larv.hyb.DRT.DropReefs + juv.D1pure.ts__DropReefs+ juv.D1hyb.DRT_DropReefs + plot_layout(guides = "NA", ncol = 2)
Rsquared.CV.fig+CV.testing.df3.fig+plot_layout(guides = "collect", ncol = 2)+larv.pure.TSA.DropReefs + larv.hyb.DRT.DropReefs + juv.D1pure.ts__DropReefs+ juv.D1hyb.DRT_DropReefs + plot_layout(guides = "collect", ncol = 2)
