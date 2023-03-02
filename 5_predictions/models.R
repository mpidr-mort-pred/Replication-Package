pacman::p_load(flexsurv,eha,survival,LTRCforests,glmnet,#reticulate,
               MASS,gamm4,nlme,reshape2,Hmisc,mgcv,
               gbm,lme4,ggplot2,gridExtra,knitr,
               lattice,dplyr,xtable,corrplot,memisc, #plyr,
               foreign,missForest,randomForestSRC,
               ggRandomForests,partykit,C50,reporttools,
               stargazer,glmnet,selectiveInference,grid,
               gridGraphics,cowplot,gridBase,party,readr,
               glmnetUtils,plotmo,dplyr,ggsci,extrafont,
               doParallel,parallel,snow,caret,rms)

#########################################################################
####################          Time-Varying Data
#########################################################################

# load time varying data
# fit the over-sampled datasets?
# datasets <- c('all','educl','gender','race')
dataset <- 'all'
if(dataset == 'all'){
  # df_train_all <- readRDS("4_modelling_data/all_train_imputed_scaled.rds")
  df_train_all <- readRDS("4_modelling_data/minus_backimpd/all_train_imputed_scaled_2.rds")
}else{
  # df_train_all <- readRDS(paste0('5_predictions/oversampled/train_balanced_',dataset,'.rds'))
  df_train_all <- readRDS(paste0('5_predictions/oversampled_minus_backimpd/train_balanced_',dataset,'.rds'))
}

# df_test_all <- readRDS("4_modelling_data/all_test_imputed_scaled.rds") 
df_test_all <- readRDS("4_modelling_data/minus_backimpd/all_test_imputed_scaled_2.rds") 

sum(df_train_all$start_outcome > df_train_all$stop_outcome)
sum(df_train_all$start_outcome < 0)

sum(df_test_all$start_outcome > df_test_all$stop_outcome)
sum(df_test_all$start_outcome < 0)

df_train <- df_train_all %>%
  rename(age = age_now) %>%
  group_by(hhidpn) %>%
  mutate(age = min(age_now),
         start = start_outcome - age,
         stop = stop_outcome - age) %>%
  ungroup() %>%
  filter(start < stop)
# df_train$age
# df_train$age_now
# identical(df_train$age,df_train$age_now)
# identical(df_train$start,df_train$start2)

df_test <- df_test_all %>%
  rename(age = age_now) %>%
  group_by(hhidpn) %>%
  mutate(age = min(age_now),
         start = start_outcome - age,
         stop = stop_outcome - age) %>%
  ungroup() %>%
  filter(start < stop)

# create time-varying data
# dropping spouse variables?
drop_spouse <- FALSE
drop_pwarm <- FALSE

if(drop_spouse){
  x_train <- df_train %>%
    dplyr::select(!contains(c("start","stop","outcome","birth_date","age_now")) &
                    !starts_with('s_'))

  x_test <- df_test %>%
    dplyr::select(!contains(c("start","stop","outcome","birth_date","age_now")) &
                    !starts_with('s_'))
}else{
  x_train <- df_train %>%
    dplyr::select(!contains(c("start","stop","outcome","birth_date","age_now")))
  
  x_test <- df_test %>%
    dplyr::select(!contains(c("start","stop","outcome","birth_date","age_now")))   
}
if(drop_pwarm){
  x_train <- x_train %>%
    dplyr::select(!contains(c('r_pwarm')))
  
  x_test <- x_test %>%
    dplyr::select(!contains(c('r_pwarm')))
}

y_train <- df_train %>%
  dplyr::select(start, stop, event = death_status_outcome)

y_test <- df_test %>%
  dplyr::select(start, stop, event = death_status_outcome)

train <- cbind(y_train,x_train)
test <- cbind(y_test,x_test)

# train <- train %>% filter(stop > start)
# test <- test %>% filter(stop > start)

# df_test %>%
#   dplyr::select(contains(c("start","stop","outcome","birth_date","age_now"))) %>%
#   names()

#########################################################################
####################          Time-invariant Data
#########################################################################

#df_train %>% filter(wave_outcome == first_wave_outcome) %>% nrow()
#length(unique(df_train_all$hhidpn))

df_train_ti <- df_train_all %>%
  group_by(hhidpn) %>%
  filter(wave_outcome == min(wave_outcome)) %>%
  ungroup()

df_test_ti <- df_test_all %>%
  group_by(hhidpn) %>%
  filter(wave_outcome == min(wave_outcome)) %>%
  ungroup()

# create time-invariant data

if(drop_spouse){
  x_train_ti <- df_train_ti %>%
    dplyr::select(!contains(c("start","stop","outcome","birth_date","age_now","s_")))
  
  x_test_ti <- df_test_ti %>%
    dplyr::select(!contains(c("start","stop","outcome","birth_date","age_now","s_")))
}else{
  x_train_ti <- df_train_ti %>%
    dplyr::select(!contains(c("start","stop","outcome","birth_date","age_now")))
  
  x_test_ti <- df_test_ti %>%
    dplyr::select(!contains(c("start","stop","outcome","birth_date","age_now")))
}
if(drop_pwarm){
  x_train_ti <- x_train_ti %>%
    dplyr::select(!contains(c('r_pwarm')))
  
  x_test_ti <- x_test_ti %>%
    dplyr::select(!contains(c('r_pwarm')))
}

y_train_ti <- df_train_ti %>%
  dplyr::select(time = exit_time_outcome, event = event_type_outcome)

y_test_ti <- df_test_ti %>%
  dplyr::select(time = exit_time_outcome, event = event_type_outcome)

train_ti <- cbind(y_train_ti,x_train_ti)
test_ti <- cbind(y_test_ti,x_test_ti)

# save data
write_csv(y_train_ti,paste0('5_predictions/oversampled_minus_backimpd/y_train_',dataset,'.csv'))
write_csv(x_train_ti,paste0('5_predictions/oversampled_minus_backimpd/x_train_',dataset,'.csv'))
write_csv(y_test_ti,paste0('5_predictions/oversampled_minus_backimpd/y_test_',dataset,'.csv'))
write_csv(x_test_ti,paste0('5_predictions/oversampled_minus_backimpd/x_test_',dataset,'.csv'))

# write_csv(y_train_ti,paste0('4_modelling_data/minus_backimpd/y_train_ti.csv'))
# write_csv(x_train_ti,paste0('4_modelling_data/minus_backimpd/x_train_ti.csv'))
# write_csv(y_test_ti,paste0('4_modelling_data/minus_backimpd/y_test_ti.csv'))
# write_csv(x_test_ti,paste0('4_modelling_data/minus_backimpd/x_test_ti.csv'))


# test how weights affect KM estimator, if at all
# weights_train <- runif(n = nrow(train_ti), min = 1, max = 5)
# weights_test <- runif(n = nrow(train_ti), min = 1, max = 5)
# 
# km1 <- survfit(Surv(time,event) ~ 1, data = y_train_ti)
# km_weights <- coxph(Surv(y_train_ti$time,y_train_ti$event) ~ 1, weights = weights_train)
# km2 <- survfit(km_weights)
# plot(km1)
# plot(km2)
# 
# plot(km1$surv, col = 'red',type = 'l')
# lines(km2$surv, col = 'blue')
# 
# mean(abs(km1$surv-km2$surv))
#########################################################################
####################          Fit models
#########################################################################

min(y_train$stop); min(y_test$stop)
max(y_train$stop); max(y_test$stop)
# times = seq(0,max(y_train$stop),1); times
times = seq(0,27,1); times

# fm = Surv(start,stop,event) ~ .
fm <- as.formula(paste("Surv(start,stop,event)", 
                       paste(setdiff(names(x_train),"hhidpn"),collapse="+"),sep="~"))

fm_ti <- as.formula(paste("Surv(time,event)", 
                       paste(setdiff(names(x_train_ti),"hhidpn"),collapse="+"),sep="~"))

# for time-varying models: get extended survival curves (carry forward last observed covariates)
max_time <- max(test$stop)
test_ext <- test
for(id in unique(test_ext$hhidpn)){
  id_ind <- test_ext$hhidpn == id
  max_ind <- test_ext$stop[id_ind] == max(test_ext$stop[id_ind])
  test_ext$stop[id_ind][max_ind] <- max_time
}

#########################################################################
####################          Reduced Cox (time-invariant)
#########################################################################

# fit model

fit_coxred <- coxph(
  Surv(time,event) ~ age + r_gender_cat + r_race_cat + r_educl_cat,
  data = train_ti,
  id = hhidpn
)

summary(fit_coxred)

# save model object
# saveRDS(fit_coxred, "5_predictions/model_objects/cphreduced_model.rds")

# get survival curves
cphreduced_pred <- survival::survfit(fit_coxred, newdata = test_ti)

# saveRDS(cphreduced_pred, "5_predictions/nospouse/curves/cph_tv_out.rds")

# save curves
# cph_pred <- readRDS("5_predictions/curves/cph_tv_out.rds")

dim(cphreduced_pred$surv)
unique(cphreduced_pred$strata)
unique(cphreduced_pred$time)
setdiff(times,unique(cphreduced_pred$time))
#View(cphreduced_pred$surv[,1:30])

cphreduced_out_mat <- matrix(0,length(times),length(unique(test_ti$hhidpn)))
cphreduced_out_mat[1,] <- 1 # time 0, all probs = 1
for(j in 2:length(times)){
  cphreduced_out_mat[j,] <- cphreduced_pred$surv[cphreduced_pred$time == times[j]]
}
cphreduced_out_mat <- cbind(times,cphreduced_out_mat)
# View(cphreduced_out_mat[,1:30])

cphreduced_out <- data.frame(cphreduced_out_mat)
names(cphreduced_out) <- c("time",unique(test_ti$hhidpn)); View(cphreduced_out[,1:30])
# write_csv(cphreduced_out, "5_predictions/curves/cphreduced_out.csv")
# write_csv(cphreduced_out, "5_predictions/nospouse/curves/cphreduced_out.csv")
write_csv(cphreduced_out, paste0("5_predictions/oversampled_minus_backimpd/curves/",dataset,"/cphreduced_out.csv"))
# write_csv(cphreduced_out, paste0("5_predictions/no_pwarm/curves/cphreduced_out.csv"))
# write_csv(cphreduced_out, paste0("5_predictions/minus_backimpd/curves/cphreduced_out.csv"))


#########################################################################
####################          Cox TV (w/ ridge regression)
#########################################################################

# fit time-varying cox model

# format data for ridge regression
train_ridge <- train
train_ridge$many <- model.matrix( ~.-1 - hhidpn, x_train) #as.matrix(x_train[,2:ncol(x_train)])

fm_ridge <- "Surv(start,stop,event) ~ ridge(many, theta = 0.000000001, scale = TRUE)"

# without ridge
# ptm <- proc.time()
# cph <- coxph(
#   formula = fm,
#   data = train,
#   id = hhidpn
# )
# proc.time() - ptm

# with ridge
ptm <- proc.time()
fit_cph5 <- coxph(
  # Surv(start,stop,event) ~ ridge(many,theta=0.0000000001,scale = TRUE), 
  Surv(start,stop,event) ~ ridge(many,theta=1,scale = TRUE), 
  id = hhidpn, data = train_ridge
)
proc.time() - ptm

fit_cph_final <- fit_cph4
fit_cph <- fit_cph4

# save model object
saveRDS(fit_cph_final, "5_predictions/minus_backimpd/model_objects/cph_tv_model.rds")

# get survival curves - full trajectories
fit_cph_final <- readRDS("5_predictions/minus_backimpd/model_objects/cph_tv_model.rds")
plot(anova(fit_cph_final), what = 'proportion chisq')
plot(anova(fit_cph_final))
anova(fit_cph_final)

test_ridge <- test_ext
test_ridge$many <- model.matrix( ~.-1 - hhidpn, x_test)
cph_pred <- survival::survfit(fit_cph_final, newdata = test_ridge, id = hhidpn)

# saveRDS(cph_pred, "5_predictions/nospouse/curves/cph_tv_out.rds")

# save curves
# cph_pred <- readRDS("5_predictions/curves/cph_tv_out.rds")

unique(cph_pred$strata)
unique(cph_pred$time)
setdiff(times,unique(cph_pred$time))

cph_out_mat <- matrix(0,length(times),length(unique(test$hhidpn)))
cph_out_mat[1,] <- 1 # time 0, all probs = 1
for(j in 2:length(times)){
  cph_out_mat[j,] <- cph_pred$surv[cph_pred$time == times[j]]
}
cph_out_mat <- cbind(times,cph_out_mat)
View(cph_out_mat[,1:30])

cph_out <- data.frame(cph_out_mat)
names(cph_out) <- c("time",unique(test$hhidpn)); 
write_csv(cph_out, "5_predictions/minus_backimpd/curves/cph_tv_out.csv")



# get largest coefficients == most important variables?
#fit_cph <- readRDS("5_predictions/model_objects/cph_tv_model.rds")
fit_cph <- readRDS("5_predictions/minus_backimpd/model_objects/cph_tv_model.rds")

top_vars <- order(abs(fit_cph$coefficients),decreasing=TRUE)[1:15]; top_vars
attributes(train_ridge$many)$dimnames[[2]][top_vars]
  
length(attributes(train_ridge$many)$dimnames[[2]])
length(fit_cph$coefficients)
fit_cph$coefficients[top_vars]
fit_cph$coefficients[attributes(train_ridge$many)$dimnames[[2]] == 'age']

#########################################################################
####################          CoxNet TV 
#########################################################################

# fit time-varying coxnet model using glmnet

x_mat_train <- model.matrix( ~.-1 - hhidpn, x_train)
surv_train <- Surv(y_train$start,
                   y_train$stop,
                   y_train$event,
                   type="counting")

ptm <- proc.time()
s <- 10
cphnet <- cv.glmnet(x = x_mat_train, y = surv_train,
              family = "cox")#,alpha = 0)#,lambda = s)
proc.time() - ptm

# get coefficients from lambda=1se fit
betas <- coef.glmnet(cphnet,s = cphnet$lambda.1se)

# top vars
attributes(betas)$Dimnames[[1]][order(abs(betas),decreasing = TRUE)[1:15]]

# format data for coxph
train_ridge <- train
train_ridge$many <- model.matrix( ~.-1 - hhidpn, x_train) 

# create coxph model object with same parameters
cphnet_obj <- coxph(
    Surv(start,stop,event) ~ many,
    id = hhidpn,
    data = train_ridge,
    init = betas,
    iter.max = 0
  )

# check coefficients
sum(abs(betas - cphnet_obj$coefficients))

# save model object
saveRDS(cphnet_obj, "5_predictions/nospouse/model_objects/cphlasso_tv_model.rds")
fit_cph_final <- cphnet_obj

# get survival curves - full trajectories
# fit_cph_final <- readRDS("5_predictions/model_objects/cphlasso_tv_model.rds")

test_ridge <- test_ext
test_ridge$many <- model.matrix( ~.-1 - hhidpn, x_test)
cph_pred <- survival::survfit(fit_cph_final, newdata = test_ridge, id = hhidpn)

# saveRDS(cph_pred, "5_predictions/nospouse/curves/cphlasso_tv_out.rds")

# save curves
# cph_pred <- readRDS("5_predictions/curves/cphlasso_tv_out.rds")

unique(cph_pred$strata)
unique(cph_pred$time)
setdiff(times,unique(cph_pred$time))

cph_out_mat <- matrix(0,length(times),length(unique(test$hhidpn)))
cph_out_mat[1,] <- 1 # time 0, all probs = 1
for(j in 2:length(times)){
  cph_out_mat[j,] <- cph_pred$surv[cph_pred$time == times[j]]
}
cph_out_mat <- cbind(times,cph_out_mat)
View(cph_out_mat[,1:30])

cph_out <- data.frame(cph_out_mat)
names(cph_out) <- c("time",unique(test$hhidpn)); 
write_csv(cph_out, "5_predictions/nospouse/curves/cphlasso_tv_out.csv")

#########################################################################
####################          time-invariant Gompertz: won't converge 
#########################################################################

# format data for ridge regression
train_ti_ridge <- train_ti
train_ti_ridge$many <- model.matrix( ~.-1 - hhidpn, x_train_ti) #as.matrix(x_train[,2:ncol(x_train)])

fm_ridge <- "Surv(start,stop,event) ~ ridge(many, theta = 0.000000001, scale = TRUE)"

# Gompertz with ridge
ptm <- proc.time()
fit_gomp <- flexsurvreg(
  formula = Surv(time,event) ~ ridge(many,theta=100000,scale = TRUE),
  data = train_ti_ridge,
  dist = 'gompertz'
)
proc.time() - ptm

ptm <- proc.time()
fit_gomp <- phreg(
  formula = Surv(time,event) ~ ridge(many,theta=100000,scale = TRUE),
  data = train_ti_ridge,
  dist = 'gompertz'
)
proc.time() - ptm

  
# fit (time-varying?) gompertz model (flexsurv or eha (phreg)?)

# ptm <- proc.time()
# fit_gomp <- phreg(formula = Surv(start,stop,event) ~ r_edyrs,#fm,
#                   data = train[1:9999,],
#                   dist = "gompertz")
# proc.time() - ptm
# 
# fit_gomp$coefficients
# 
# predict(fit_gomp, newdata = test[1:9999,])
# 
# 
# fit_cr <- coxregp(Surv(start,sto,event) ~ r_edyrs, data = train[1:99999,])
# check.dist(fit_cr, fit_gomp)
# 
# fit_gomp

# time-invariant gompertz using flexsurvreg
ptm <- proc.time()
fit_gomp <- flexsurvreg(formula = fm_ti,
                        data = train_ti,
                        dist = "gompertz")
ptm <- proc.time()

# saveRDS(fit_gomp, "5_predictions/model_objects/gomp_model.rds")

pred_gomp <- predict(object = fit_gomp, 
                     newdata = test_ti,
                     times = times,
                     type = 'survival')

saveRDS(pred_gomp, "5_predictions/curves/gomp_out.rds")

# fit (time-varying?) PCH model (flexsurv or eha?)

#########################################################################
####################          RRF - works
#########################################################################

# fit rrf (LTRCforests)

ptm <- proc.time()
fit_rrf <- ltrcrrf(formula = fm, data = train, id = hhidpn, ntime = times,mtry = ncol(x_train)-1)#,
                   #stepFactor = 1.5, ntree = 1, mtry = 1, nodedepth = 2,nsplit=2)
proc.time() - ptm

# save model object
saveRDS(fit_rrf, "5_predictions/no_pwarm/model_objects/rrf_model.rds")

# get predicted curves - full trajectory
# fit_rrf <- readRDS("5_predictions/model_objects/rrf_model.rds")

pred_rrf_test <- predictProb(fit_rrf, time.eval = times,
                             newdata = test, newdata.id = hhidpn)

# saveRDS(pred_rrf_test, "5_predictions/no_pwarm/curves/rrf_out.rds")

# save curves

rrf_out_mat <- cbind(pred_rrf_test$survival.times, pred_rrf_test$survival.probs)
rrf_out <- data.frame(rrf_out_mat)
names(rrf_out) <- c("time",unique(test$hhidpn)); #names(rrf_out)
write_csv(rrf_out, "5_predictions/no_pwarm/curves/rrf_out.csv")



# get stop times for each individual
tau <- test %>%
  group_by(hhidpn) %>%
  summarize(tau = max(stop))
View(tau)

tau <- tau[match(test$hhidpn,tau$hhidpn),]
tau <- tau[!duplicated(tau),]
# tau <- tau[match(tau$hhidpn,test$hhidpn),]
View(tau)
View(test %>% select(hhidpn))

# get predicted curves, observed/partial trajectory
pred_rrf_test_partial <- predictProb(fit_rrf, time.eval = times,
                             newdata = test, newdata.id = hhidpn,
                             time.tau = tau %>% pull(tau))

saveRDS(pred_rrf_test_partial, "5_predictions/curves/rrf_out_partial.rds")

# save curves
rrf_obs_mat <- matrix(0,
                      length(pred_rrf_test_partial$survival.times),
                      length(pred_rrf_test_partial$survival.tau)+1)
rrf_obs_mat[,1] <- times
for(j in 2:ncol(rrf_obs_mat)){
  probs <- pred_rrf_test_partial$survival.probs[[j-1]]
  n_obs <- length(probs)
  rrf_obs_mat[,j] <- c(probs, rep(NA,length(times) - n_obs))   
}
View(rrf_obs_mat[,1:30])

rrf_obs_out <- data.frame(rrf_obs_mat)
names(rrf_obs_out) <- c("time",unique(test$hhidpn)); 
write_csv(rrf_obs_out, "5_predictions/curves/rrf_out_partial.csv")



# # get predicted curves, extended trajectory (carry last observed covariates forward to end)
# pred_rrf_test_ext <- predictProb(fit_rrf, time.eval = times,
#                                  newdata = test_ext[1:10,], newdata.id = hhidpn)
# 
# # get stop times for each individual
# tau_ext <- test_ext %>%
#   group_by(hhidpn) %>%
#   summarize(tau = max(stop))
# View(tau_ext)
# 
# tau_ext <- tau_ext[match(test_ext$hhidpn,tau_ext$hhidpn),]
# tau_ext <- tau_ext[!duplicated(tau_ext),]
# View(tau_ext)
# View(test_ext %>% select(hhidpn))
# 
# pred_rrf_test_ext2 <- predictProb(fit_rrf, time.eval = times,
#                                  newdata = test_ext, newdata.id = hhidpn,
#                                  time.tau = tau_ext %>% pull(tau))
# 
# View(pred_rrf_test_ext2$survival.probs[,1:10])
# pred_rrf_test <- readRDS("5_predictions/curves/rrf_out.rds")
# View(pred_rrf_test$survival.probs[,1:10])
# identical(pred_rrf_test_ext2$survival.probs,pred_rrf_test$survival.probs)



# variable importance? -- nope
# randomForestSRC::vimp.rfsrc(fit_rrf)

#########################################################################
####################          CIF
#########################################################################

# fit cif (LTRCforests)

ptm <- proc.time()
fit_cif <- ltrccif(formula = fm, data = train, id = hhidpn, mtry = ncol(x_train)-1)
#stepFactor = 1.5, ntree = 1, mtry = 1, nodedepth = 2,nsplit=2)
proc.time() - ptm

# save model object
saveRDS(fit_cif, "5_predictions/model_objects/cif_model_ns.rds")

# get predicted curves - full trajectory
fit_cif <- readRDS("5_predictions/model_objects/cif_model.rds")

# calculate curves in chunks due to memory issues
ids <- unique(test$hhidpn)
n_folds <- 100
folds <- round(seq(1,length(ids),length.out = n_folds+1)); folds
pred_cif_test <- NULL
for(j in 1:n_folds){
  print(j/n_folds *100)
  test_fold <- test %>% filter(hhidpn %in% ids[folds[j]:(folds[j+1]-1)])
  pred_cif_test[[j]] <- predictProb(fit_cif, time.eval = times,
                                newdata = test_fold, newdata.id = hhidpn)  
}

saveRDS(pred_cif_test, "5_predictions/curves/cif_out.rds")

# save curves

cif_out_mat <- matrix(0,length(times),length(ids)+1)
cif_out_mat[,1] <- times
for(j in 1:n_folds){
  cif_out_mat[,(folds[j]+1):(folds[j+1])] <- pred_cif_test[[j]]$survival.probs
}
View(cif_out_mat[,1:30])
 
# cif_out_mat <- cbind(pred_cif_test$survival.times, pred_cif_test$survival.probs)
cif_out <- data.frame(cif_out_mat)
names(cif_out) <- c("time",unique(test$hhidpn)); 
View(cif_out[,1:30])

write_csv(cif_out, "5_predictions/curves/cif_out.csv")




















































#########################################################################
####################          RF-SLAM - broken
#########################################################################

# load RF-SLAM
if(!require(rfSLAM)){
  pacman::p_load(devtools)
  install_github("mattrosen/rfSLAM")  
}
library(rfSLAM)

# source functions
# source('../functions/rfslam_functions.R')

# fit RF-SLAM

ntree <- 2
nodesize <- 2
nodedepth <- 2

train_rfslam <- train
train_rfslam$event <- as.factor(train_rfslam$event)
test_rfslam <- test
test_rfslam$event <- as.factor(test_rfslam$event)

fm_rfslam <- as.formula(paste("event", 
                       paste(setdiff(names(x_train),"hhidpn"),collapse="+"),sep="~"))

fit_rfslam <- rfSLAM::rfsrc(formula = fm_rfslam,
                            data = train_rfslam[1:999,],
                            ntree = ntree,
                            nodesize = nodesize,
                            nodedepth = nodedepth,
                            splitrule = 'poisson.split1',
                            risk.time = y_train[1:999,]$stop - y_train[1:999,]$start,
                            stratifier = df_train[1:999,]$wave_outcome)
                            #mtry = ncol(x_train))

pred_rfslam <- rfSLAM::predict.rfsrc(object = fit_rfslam)
pred_rfslam$predicted

pred_rfslam <- rfSLAM::predict.rfsrc(object = fit_rfslam,
                                     newdata = test_rfslam[1:999,])

pred_rfslam <- rfSLAM::predict.rfsrc(object = fit_rfslam,
                                     newdata = train_rfslam[1:999,])




























# check
df_train %>% filter(exit_time_outcome <= 0) %>% nrow()
View(df_train %>% filter(stop_outcome <= start_outcome) %>% group_by(hhidpn))
df_test %>% filter(exit_time_outcome <= 0) %>% nrow()
View(y_train %>% filter(stop <= start))

x_train$age[df_train$hhidpn == df_train$hhidpn[1]]

df_train <- df_train %>%
  group_by(hhidpn) %>%
  mutate(age_entry = age[1]) %>%
  ungroup()

df_train %>% filter(hhidpn == "10404010") %>%
  select(start_outcome, stop_outcome, age, exit_age_outcome)

y_train %>% filter(hhidpn == unique(y_train$hhidpn)[1])
         