
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
RNGkind("Rounding")
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
#write.csv as explained before
formula.drt.larv <- y ~ poly(x, 5, raw = TRUE)
larv.hyb.DRT_small<-ggplot(DRT.larvbuild, aes(a2, b2)) +
  geom_point() +theme_bw()+
  stat_smooth(aes(), method = "lm", formula = formula.drt.larv) 


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

#save.image(file="Prediction_AGF18_githubtidy_NatComms.RUN.RData") 


