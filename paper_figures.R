# MVC Analyses and Figures ----

setwd('~/Desktop/MVC')
set.seed(40)
# Load some libraries ----
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rms)
library(boot)
library(pROC)
library(survey)

# Load the dataset ----
df <- read_csv("NASS_filtered_06092019.csv")
df <- df %>% select(dv_mph, age, female, modelyr, beltuse, body_van, body_suv, body_pickup, body_car, multicoll, pdof_front, pdof_nearside, pdof_farside, pdof_rear, iss90_16_plus, ratwgt, stratif, psu, psustrat, age_15_19, age_20_24, age_25_29, age_30_34, age_35_39, age_40_44, age_45_49, age_50_54, age_55_59, age_60_64, age_65_69, age_70_74, age_75_plus)
df <- na.omit(df)

# Assign PDOF and vehicle body type labels
df$pdof <- ifelse(df$pdof_front,"front",ifelse(df$pdof_nearside,"nearside", ifelse(df$pdof_farside, "farside", "rear")))
df$body <- ifelse(df$body_car,"car",ifelse(df$body_pickup,"pickup", ifelse(df$body_van, "van", "suv")))

df_15_54   <- df %>% filter(age >= 15 & age < 55)
df_45_plus <- df %>% filter(age >= 45)
df_55_plus <- df %>% filter(age >= 55)
df_65_plus <- df %>% filter(age >= 65)

# Table 1 Demographics ----

# Total cases

n_15_54      <- nrow(df_15_54); p_15_54 <- n_15_54/nrow(df)
n_15_54_wt   <- sum(df_15_54$ratwgt); p_15_54_wt <- n_15_54_wt/sum(df$ratwgt)
n_45_plus    <- nrow(df_45_plus); p_45_plus <- n_45_plus/nrow(df)
n_45_plus_wt <- sum(df_45_plus$ratwgt); p_45_plus_wt <- n_45_plus_wt/sum(df$ratwgt)
n_55_plus    <- nrow(df_55_plus); p_55_plus <- n_55_plus/nrow(df)
n_55_plus_wt <- sum(df_55_plus$ratwgt); p_55_plus_wt <- n_55_plus_wt/sum(df$ratwgt)
n_65_plus    <- nrow(df_65_plus); p_65_plus <- n_65_plus/nrow(df)
n_65_plus_wt <- sum(df_65_plus$ratwgt); p_65_plus_wt <- n_65_plus_wt/sum(df$ratwgt)

# ISS 16+
nISS_15_54 <- summarise(group_by(df_15_54,iss90_16_plus),cases=n(), freq=n()/nrow(df_15_54), weighted=sum(ratwgt), freq_wt=weighted/sum(df_15_54$ratwgt))

nISS_45_plus <- summarise(group_by(df_45_plus,iss90_16_plus),cases=n(), freq=n()/nrow(df_45_plus), weighted=sum(ratwgt), freq_wt=weighted/sum(df_45_plus$ratwgt))

nISS_55_plus <- summarise(group_by(df_55_plus,iss90_16_plus),cases=n(), freq=n()/nrow(df_55_plus), weighted=sum(ratwgt), freq_wt=weighted/sum(df_55_plus$ratwgt))

nISS_65_plus <- summarise(group_by(df_65_plus,iss90_16_plus),cases=n(), freq=n()/nrow(df_65_plus), weighted=sum(ratwgt), freq_wt=weighted/sum(df_65_plus$ratwgt))


# Gender
nGender_15_54 <- summarise(group_by(df_15_54,female),cases=n(), freq=n()/nrow(df_15_54), weighted=sum(ratwgt), freq_wt=weighted/sum(df_15_54$ratwgt))

nGender_45_plus <- summarise(group_by(df_45_plus,female),cases=n(), freq=n()/nrow(df_45_plus), weighted=sum(ratwgt), freq_wt=weighted/sum(df_45_plus$ratwgt))

nGender_55_plus <- summarise(group_by(df_55_plus,female),cases=n(), freq=n()/nrow(df_55_plus), weighted=sum(ratwgt), freq_wt=weighted/sum(df_55_plus$ratwgt))

nGender_65_plus <- summarise(group_by(df_65_plus,female),cases=n(), freq=n()/nrow(df_65_plus), weighted=sum(ratwgt), freq_wt=weighted/sum(df_65_plus$ratwgt))

# Single Collision
nColl_15_54 <- summarise(group_by(df_15_54,multicoll),cases=n(), freq=n()/nrow(df_15_54), weighted=sum(ratwgt), freq_wt=weighted/sum(df_15_54$ratwgt))

nColl_45_plus <- summarise(group_by(df_45_plus,multicoll),cases=n(), freq=n()/nrow(df_45_plus), weighted=sum(ratwgt), freq_wt=weighted/sum(df_45_plus$ratwgt))

nColl_55_plus <- summarise(group_by(df_55_plus,multicoll),cases=n(), freq=n()/nrow(df_55_plus), weighted=sum(ratwgt), freq_wt=weighted/sum(df_55_plus$ratwgt))

nColl_65_plus <- summarise(group_by(df_65_plus,multicoll),cases=n(), freq=n()/nrow(df_65_plus), weighted=sum(ratwgt), freq_wt=weighted/sum(df_65_plus$ratwgt))

# PDOF

nPDOF_15_54 <- summarise(group_by(df_15_54,pdof),cases=n(), freq=n()/nrow(df_15_54), weighted=sum(ratwgt), freq_wt=weighted/sum(df_15_54$ratwgt))

nPDOF_45_plus <- summarise(group_by(df_45_plus,pdof),cases=n(), freq=n()/nrow(df_45_plus), weighted=sum(ratwgt), freq_wt=weighted/sum(df_45_plus$ratwgt))

nPDOF_55_plus <- summarise(group_by(df_55_plus,pdof),cases=n(), freq=n()/nrow(df_55_plus), weighted=sum(ratwgt), freq_wt=weighted/sum(df_55_plus$ratwgt))

nPDOF_65_plus <- summarise(group_by(df_65_plus,pdof),cases=n(), freq=n()/nrow(df_65_plus), weighted=sum(ratwgt), freq_wt=weighted/sum(df_65_plus$ratwgt))

# Vehicle Body Type

nBody_15_54 <- summarise(group_by(df_15_54,body),cases=n(), freq=n()/nrow(df_15_54), weighted=sum(ratwgt), freq_wt=weighted/sum(df_15_54$ratwgt))

nBody_45_plus <- summarise(group_by(df_45_plus,body),cases=n(), freq=n()/nrow(df_45_plus), weighted=sum(ratwgt), freq_wt=weighted/sum(df_45_plus$ratwgt))

nBody_55_plus <- summarise(group_by(df_55_plus,body),cases=n(), freq=n()/nrow(df_55_plus), weighted=sum(ratwgt), freq_wt=weighted/sum(df_55_plus$ratwgt))

nBody_65_plus <- summarise(group_by(df_65_plus,body),cases=n(), freq=n()/nrow(df_65_plus), weighted=sum(ratwgt), freq_wt=weighted/sum(df_65_plus$ratwgt))

# Belt Use

nBeltuse_15_54 <- summarise(group_by(df_15_54,beltuse),cases=n(), freq=n()/nrow(df_15_54), weighted=sum(ratwgt), freq_wt=weighted/sum(df_15_54$ratwgt))

nBeltuse_45_plus <- summarise(group_by(df_45_plus,beltuse),cases=n(), freq=n()/nrow(df_45_plus), weighted=sum(ratwgt), freq_wt=weighted/sum(df_45_plus$ratwgt))

nBeltuse_55_plus <- summarise(group_by(df_55_plus,beltuse),cases=n(), freq=n()/nrow(df_55_plus), weighted=sum(ratwgt), freq_wt=weighted/sum(df_55_plus$ratwgt))

nBeltuse_65_plus <- summarise(group_by(df_65_plus,beltuse),cases=n(), freq=n()/nrow(df_65_plus), weighted=sum(ratwgt), freq_wt=weighted/sum(df_65_plus$ratwgt))

# Table 2 Performance of Models over 1000 iterations ----

train_idx_15_54 <- vector("list",1000)
fit_glm_15_54   <- vector("list",1000)
df_train_15_54  <- vector("list",1000)
df_test_15_54   <- vector("list",1000)
auc_15_54_self_list  <- vector(length=1000)

for (i in 1:1000){
  print(i)
  # Split into train and test
  train_idx_15_54[[i]] <- createDataPartition(df_15_54$age, p = 0.8, list=FALSE)
  df_15_54_train <- df_15_54[train_idx_15_54[[i]],]
  df_15_54_test  <- df_15_54[-train_idx_15_54[[i]],]
  
  fit_glm_15_54[[i]] <- glm(iss90_16_plus ~ log(dv_mph)
                            + age
                            + female
                            + beltuse
                            + body_van
                            + body_suv
                            + body_pickup
                            + multicoll
                            + pdof_front
                            + pdof_nearside
                            + pdof_farside,
                            data = df_15_54_train, family = binomial(link=logit))
  
  pred_15_54 <- predict(fit_glm_15_54[[i]], newdata=df_15_54_test, type="response")
  roc_15_54_self <- roc(df_15_54_test$iss90_16_plus, pred_15_54)
  auc_15_54_self_list[[i]] <- as.numeric(auc(roc_15_54_self))
}

x_15_54_self <- mean(auc_15_54_self_list)
sd_15_54_self <- sd(auc_15_54_self_list)
low_15_54_self <- quantile(auc_15_54_self_list, 0.025)
hi_15_54_self <- quantile(auc_15_54_self_list, 0.975)


# 45+ in 45+
# # 55+ in 45+
train_idx_45_plus <- vector("list",1000)
fit_glm_45_plus   <- vector("list",1000)
df_train_45_plus  <- vector("list",1000)
df_test_45_plus   <- vector("list",1000)
auc_45_plus_self_list  <- vector(length=1000)

for (i in 1:1000){
  print(i)
  # Split into train and test
  train_idx_45_plus[[i]] <- createDataPartition(df_45_plus$age, p = 0.8, list=FALSE)
  df_45_plus_train <- df_45_plus[train_idx_45_plus[[i]],]
  df_45_plus_test  <- df_45_plus[-train_idx_45_plus[[i]],]
  
  fit_glm_45_plus[[i]] <- glm(iss90_16_plus ~ log(dv_mph)
                              + age
                              + female
                              + beltuse
                              + body_van
                              + body_suv
                              + body_pickup
                              + multicoll
                              + pdof_front
                              + pdof_nearside
                              + pdof_farside,
                              data = df_45_plus_train, family = binomial(link=logit))
  
  pred_45_plus <- predict(fit_glm_45_plus[[i]], newdata=df_45_plus_test, type="response")
  roc_45_plus_self <- roc(df_45_plus_test$iss90_16_plus, pred_45_plus)
  auc_45_plus_self_list[[i]] <- as.numeric(auc(roc_45_plus_self))
}

x_45_plus_self <- mean(auc_45_plus_self_list)
sd_45_plus_self <- sd(auc_45_plus_self_list)
low_45_plus_self <- quantile(auc_45_plus_self_list, 0.025)
hi_45_plus_self <- quantile(auc_45_plus_self_list, 0.975)

# # 55+ in 55+
train_idx_55_plus <- vector("list",1000)
fit_glm_55_plus   <- vector("list",1000)
df_train_55_plus  <- vector("list",1000)
df_test_55_plus   <- vector("list",1000)
auc_55_plus_self_list  <- vector(length=1000)

for (i in 1:1000){
  print(i)
  # Split into train and test
  train_idx_55_plus[[i]] <- createDataPartition(df_55_plus$age, p = 0.8, list=FALSE)
  df_55_plus_train <- df_55_plus[train_idx_55_plus[[i]],]
  df_55_plus_test  <- df_55_plus[-train_idx_55_plus[[i]],]
  
  fit_glm_55_plus[[i]] <- glm(iss90_16_plus ~ log(dv_mph)
                              + age
                              + female
                              + beltuse
                              + body_van
                              + body_suv
                              + body_pickup
                              + multicoll
                              + pdof_front
                              + pdof_nearside
                              + pdof_farside,
                              data = df_55_plus_train, family = binomial(link=logit))
  
  pred_55_plus <- predict(fit_glm_55_plus[[i]], newdata=df_55_plus_test, type="response")
  roc_55_plus_self <- roc(df_55_plus_test$iss90_16_plus, pred_55_plus)
  auc_55_plus_self_list[[i]] <- as.numeric(auc(roc_55_plus_self))
}

x_55_plus_self <- mean(auc_55_plus_self_list)
sd_55_plus_self <- sd(auc_55_plus_self_list)
low_55_plus_self <- quantile(auc_55_plus_self_list, 0.025)
hi_55_plus_self <- quantile(auc_55_plus_self_list, 0.975)
#
# # 65+ in 65+
train_idx_65_plus <- vector("list",1000)
fit_glm_65_plus   <- vector("list",1000)
df_train_65_plus  <- vector("list",1000)
df_test_65_plus   <- vector("list",1000)
auc_65_plus_self_list  <- vector(length=1000)


for (i in 1:1000){
  print(i)
  # Split into train and test
  train_idx_65_plus[[i]] <- createDataPartition(df_65_plus$age, p = 0.8, list=FALSE)
  df_65_plus_train <- df_65_plus[train_idx_65_plus[[i]],]
  df_65_plus_test  <- df_65_plus[-train_idx_65_plus[[i]],]
  
  fit_glm_65_plus[[i]] <- glm(iss90_16_plus ~ log(dv_mph)
                              + age
                              + female
                              + beltuse
                              + body_van
                              + body_suv
                              + body_pickup
                              + multicoll
                              + pdof_front
                              + pdof_nearside
                              + pdof_farside,
                              data = df_65_plus_train, family = binomial(link=logit))
  
  pred_65_plus <- predict(fit_glm_65_plus[[i]], newdata=df_65_plus_test, type="response")
  roc_65_plus_self <- roc(df_65_plus_test$iss90_16_plus, pred_65_plus)
  auc_65_plus_self_list[[i]] <- as.numeric(auc(roc_65_plus_self))
}

x_65_plus_self <- mean(auc_65_plus_self_list)
sd_65_plus_self <- sd(auc_65_plus_self_list)
low_65_plus_self <- quantile(auc_65_plus_self_list, 0.025)
hi_65_plus_self <- quantile(auc_65_plus_self_list, 0.975)

# Table 2 Performance for all data model ----

train_idx <- vector("list",1000)
fit_glm   <- vector("list",1000)
df_train  <- vector("list",1000)
df_test   <- vector("list",1000)
auc_15_54_list <- vector(length=1000)
auc_45_plus_list <- vector(length=1000)
auc_55_plus_list  <- vector(length=1000)
auc_65_plus_list  <- vector(length=1000)
spec_15_54 <- vector(length=1000)
spec_45_plus <- vector(length=1000)
spec_55_plus <- vector(length=1000)
spec_65_plus <- vector(length=1000)
sens_15_54 <- vector("list",1000)
sens_45_plus <- vector("list",1000)
sens_55_plus <- vector("list",1000)
sens_65_plus <- vector("list",1000)

for (i in 1:1000){ #chose 37
  print(i)
  # Split into train and test
  train_idx[[i]] <- createDataPartition(df$age, p = 0.8, list=FALSE)
  df_train <- df[train_idx[[i]],]
  df_test  <- df[-train_idx[[i]],]

  fit_glm[[i]] <- glm(iss90_16_plus ~ log(dv_mph)
                      + age
                      + female
                      + beltuse
                      + body_van
                      + body_suv
                      + body_pickup
                      + multicoll
                      + pdof_front
                      + pdof_nearside
                      + pdof_farside,
                      data = df_train, family = binomial(link=logit))

  df_15_54_test <- df_test %>% filter(age < 55)
  pred_15_54 <- predict(fit_glm[[i]], newdata=df_15_54_test, type="response")
  roc_15_54 <- roc(df_15_54_test$iss90_16_plus, pred_15_54)
  auc_15_54_list[[i]] <- as.numeric(auc(roc_15_54))
  
  df_45_plus_test <- df_test %>% filter(age >=45)
  pred_45_plus <- predict(fit_glm[[i]], newdata=df_45_plus_test, type="response")
  roc_45_plus <- roc(df_45_plus_test$iss90_16_plus, pred_45_plus)
  auc_45_plus_list[[i]] <- as.numeric(auc(roc_45_plus))

  df_55_plus_test <- df_test %>% filter(age >= 55)
  pred_55_plus <- predict(fit_glm[[i]], newdata=df_55_plus_test, type="response")
  roc_55_plus <- roc(df_55_plus_test$iss90_16_plus, pred_55_plus)
  auc_55_plus_list[[i]] <- as.numeric(auc(roc_55_plus))

  df_65_plus_test <- df_test %>% filter(age >= 65)
  pred_65_plus <- predict(fit_glm[[i]], newdata=df_65_plus_test, type="response")
  roc_65_plus <- roc(df_65_plus_test$iss90_16_plus, pred_65_plus)
  auc_65_plus_list[[i]] <- as.numeric(auc(roc_65_plus))


  sens_15_54[[i]] <- roc_15_54$sensitivities
  sens_55_plus[[i]] <- roc_55_plus$sensitivities
  sens_65_plus[[i]] <- roc_65_plus$sensitivities

  spec_15_54[[i]] <- roc_15_54$specificities[[tail(which(sens_15_54[[i]] >= 0.95), n=1)]]
  spec_55_plus[[i]] <- roc_55_plus$specificities[[tail(which(sens_55_plus[[i]] >= 0.95), n=1)]]
  spec_65_plus[[i]] <- roc_65_plus$specificities[[tail(which(sens_65_plus[[i]] >= 0.95), n=1)]]
}

x_15_54 <- mean(auc_15_54_list)
sd_15_54 <- sd(auc_15_54_list)
low_15_54 <- quantile(auc_15_54_list, 0.025)
hi_15_54 <- quantile(auc_15_54_list, 0.975)

x_45_plus <- mean(auc_45_plus_list)
sd_45_plus <- sd(auc_45_plus_list)
low_45_plus <- quantile(auc_45_plus_list, 0.025)
hi_45_plus <- quantile(auc_45_plus_list, 0.975)

x_55_plus <- mean(auc_55_plus_list)
sd_55_plus <- sd(auc_55_plus_list)
low_55_plus <- quantile(auc_55_plus_list, 0.025)
hi_55_plus <- quantile(auc_55_plus_list, 0.975)

x_65_plus <- mean(auc_65_plus_list)
sd_65_plus <- sd(auc_65_plus_list)
low_65_plus <- quantile(auc_65_plus_list, 0.025)
hi_65_plus <- quantile(auc_65_plus_list, 0.975)

# Table 3 Specificities Unweighted ----

fit_glm_all <- glm(iss90_16_plus ~ log(dv_mph)
                   + age
                   + female
                   + beltuse
                   + body_van
                   + body_suv
                   + body_pickup
                   + multicoll
                   + pdof_front
                   + pdof_nearside
                   + pdof_farside,
                   data = df, family = binomial(link=logit))
summary(fit_glm_all)
pred_all <- predict(fit_glm_all, type="response")
roc_all  <- roc(df$iss90_16_plus, pred_all)

fit_glm_15_54 <- glm(iss90_16_plus ~ log(dv_mph)
                     + age
                     + female
                     + beltuse
                     + body_van
                     + body_suv
                     + body_pickup
                     + multicoll
                     + pdof_front
                     + pdof_nearside
                     + pdof_farside,
                     data = df_15_54, family = binomial(link=logit))
summary(fit_glm_15_54)
pred_15_54 <- predict(fit_glm_15_54, type="response")
roc_15_54  <- roc(df_15_54$iss90_16_plus, pred_15_54)

fit_glm_45_plus <- glm(iss90_16_plus ~ log(dv_mph)
                       + age
                       + female
                       + beltuse
                       + body_van
                       + body_suv
                       + body_pickup
                       + multicoll
                       + pdof_front
                       + pdof_nearside
                       + pdof_farside,
                       data = df_45_plus, family = binomial(link=logit))
summary(fit_glm_45_plus)
pred_45_plus <- predict(fit_glm_45_plus, type="response")
roc_45_plus  <- roc(df_45_plus$iss90_16_plus, pred_45_plus)

fit_glm_55_plus <- glm(iss90_16_plus ~ log(dv_mph)
                       + age
                       + female
                       + beltuse
                       + body_van
                       + body_suv
                       + body_pickup
                       + multicoll
                       + pdof_front
                       + pdof_nearside
                       + pdof_farside,
                       data = df_55_plus, family = binomial(link=logit))
summary(fit_glm_55_plus)
pred_55_plus <- predict(fit_glm_55_plus, type="response")
roc_55_plus  <- roc(df_55_plus$iss90_16_plus, pred_55_plus)

fit_glm_65_plus <- glm(iss90_16_plus ~ log(dv_mph)
                       + age
                       + female
                       + beltuse
                       + body_van
                       + body_suv
                       + body_pickup
                       + multicoll
                       + pdof_front
                       + pdof_nearside
                       + pdof_farside,
                       data = df_65_plus, family = binomial(link=logit))
summary(fit_glm_65_plus)
pred_65_plus <- predict(fit_glm_65_plus, type="response")
roc_65_plus  <- roc(df_65_plus$iss90_16_plus, pred_65_plus)

plot_roc_all <- data.frame(Specificity=vector(,length(roc_all$specificities)), Sensitivity = vector(,length(roc_all$specificities)), Group = vector(,length(roc_all$specificities)))
plot_roc_all$Specificity <- roc_all$specificities
plot_roc_all$Sensitivity <- roc_all$sensitivities

plot_roc_15_54 <- data.frame(Specificity=vector(,length(roc_15_54$specificities)), Sensitivity = vector(,length(roc_15_54$specificities)), Group = vector(,length(roc_15_54$specificities)))
plot_roc_15_54$Specificity <- roc_15_54$specificities
plot_roc_15_54$Sensitivity <- roc_15_54$sensitivities
plot_roc_15_54$Group <- rep(paste0("15-54: AUC = ",round(as.numeric(auc(roc_15_54)),digits=3)),length(roc_15_54$specificities))

plot_roc_45_plus <- data.frame(Specificity=vector(,length(roc_45_plus$specificities)), Sensitivity = vector(,length(roc_45_plus$specificities)), Group = vector(,length(roc_45_plus$specificities)))
plot_roc_45_plus$Specificity <- roc_45_plus$specificities
plot_roc_45_plus$Sensitivity <- roc_45_plus$sensitivities
plot_roc_45_plus$Group <- rep(paste0("45+: AUC = ",round(as.numeric(auc(roc_45_plus)),digits=3)),length(roc_45_plus$specificities))

plot_roc_55_plus <- data.frame(Specificity=vector(,length(roc_55_plus$specificities)), Sensitivity = vector(,length(roc_55_plus$specificities)), Group = vector(,length(roc_55_plus$specificities)))
plot_roc_55_plus$Specificity <- roc_55_plus$specificities
plot_roc_55_plus$Sensitivity <- roc_55_plus$sensitivities
plot_roc_55_plus$Group <- rep(paste0("55+: AUC = ",round(as.numeric(auc(roc_55_plus)),digits=3)),length(roc_55_plus$specificities))

plot_roc_65_plus <- data.frame(Specificity=vector(,length(roc_65_plus$specificities)), Sensitivity = vector(,length(roc_65_plus$specificities)), Group = vector(,length(roc_65_plus$specificities)))
plot_roc_65_plus$Specificity <- roc_65_plus$specificities
plot_roc_65_plus$Sensitivity <- roc_65_plus$sensitivities
plot_roc_65_plus$Group <- rep(paste0("65+: AUC = ",round(as.numeric(auc(roc_65_plus)),digits=3)),length(roc_65_plus$specificities))

# Table 3 Specificities Weighted ----

design_svy <- svydesign(id=~psu, strata=~stratif, weights=~ratwgt, data=df, nest=TRUE)
fit_svy    <- svyglm(iss90_16_plus ~ log(dv_mph)
                     + age
                     + female
                     + beltuse
                     + body_van
                     + body_suv
                     + body_pickup
                     + multicoll
                     + pdof_front
                     + pdof_nearside
                     + pdof_farside,
                     design = design_svy)

pred_svy <- predict(fit_svy, type="response")
roc_svy <- roc(df$iss90_16_plus, pred_svy)
AUC_svy <- as.numeric(auc(roc_svy))

plot_roc_all_wt <- data.frame(Specificity=vector(,length(roc_svy$specificities)), Sensitivity = vector(,length(roc_svy$specificities)), Group = vector(,length(roc_svy$specificities)))
plot_roc_all_wt$Specificity <- roc_svy$specificities
plot_roc_all_wt$Sensitivity <- roc_svy$sensitivities

design_svy_15_54 <- svydesign(id=~psu, strata=~stratif, weights=~ratwgt, data=df_15_54, nest=TRUE)
fit_svy_15_54    <- svyglm(iss90_16_plus ~ log(dv_mph)
                           + age
                           + female
                           + beltuse
                           + body_van
                           + body_suv
                           + body_pickup
                           + multicoll
                           + pdof_front
                           + pdof_nearside
                           + pdof_farside,
                           design = design_svy_15_54)

pred_svy_15_54 <- predict(fit_svy_15_54, type="response")
roc_svy_15_54 <- roc(df_15_54$iss90_16_plus, pred_svy_15_54)
AUC_svy_15_54 <- as.numeric(auc(roc_svy_15_54))

plot_roc_15_54_wt <- data.frame(Specificity=vector(,length(roc_svy_15_54$specificities)), Sensitivity = vector(,length(roc_svy_15_54$specificities)), Group = vector(,length(roc_svy_15_54$specificities)))
plot_roc_15_54_wt$Specificity <- roc_svy_15_54$specificities
plot_roc_15_54_wt$Sensitivity <- roc_svy_15_54$sensitivities

# 45 +

design_svy_45_plus <- svydesign(id=~psu, strata=~stratif, weights=~ratwgt, data=df_45_plus, nest=TRUE)
fit_svy_45_plus    <- svyglm(iss90_16_plus ~ log(dv_mph)
                             + age
                             + female
                             + beltuse
                             + body_van
                             + body_suv
                             + body_pickup
                             + multicoll
                             + pdof_front
                             + pdof_nearside
                             + pdof_farside,
                             design = design_svy_45_plus)

pred_svy_45_plus <- predict(fit_svy_45_plus, type="response")
roc_svy_45_plus <- roc(df_45_plus$iss90_16_plus, pred_svy_45_plus)
AUC_svy_45_plus <- as.numeric(auc(roc_svy_45_plus))

plot_roc_45_plus_wt <- data.frame(Specificity=vector(,length(roc_svy_45_plus$specificities)), Sensitivity = vector(,length(roc_svy_45_plus$specificities)), Group = vector(,length(roc_svy_45_plus$specificities)))
plot_roc_45_plus_wt$Specificity <- roc_svy_45_plus$specificities
plot_roc_45_plus_wt$Sensitivity <- roc_svy_45_plus$sensitivities

# 55 +
design_svy_55_plus <- svydesign(id=~psu, strata=~stratif, weights=~ratwgt, data=df_55_plus, nest=TRUE)
fit_svy_55_plus    <- svyglm(iss90_16_plus ~ log(dv_mph)
                             + age
                             + female
                             + beltuse
                             + body_van
                             + body_suv
                             + body_pickup
                             + multicoll
                             + pdof_front
                             + pdof_nearside
                             + pdof_farside,
                             design = design_svy_55_plus)

pred_svy_55_plus <- predict(fit_svy_55_plus, type="response")
roc_svy_55_plus <- roc(df_55_plus$iss90_16_plus, pred_svy_55_plus)
AUC_svy_55_plus <- as.numeric(auc(roc_svy_55_plus))

plot_roc_55_plus_wt <- data.frame(Specificity=vector(,length(roc_svy_55_plus$specificities)), Sensitivity = vector(,length(roc_svy_55_plus$specificities)), Group = vector(,length(roc_svy_55_plus$specificities)))
plot_roc_55_plus_wt$Specificity <- roc_svy_55_plus$specificities
plot_roc_55_plus_wt$Sensitivity <- roc_svy_55_plus$sensitivities

design_svy_65_plus <- svydesign(id=~psu, strata=~stratif, weights=~ratwgt, data=df_65_plus, nest=TRUE)
fit_svy_65_plus    <- svyglm(iss90_16_plus ~ log(dv_mph)
                             + age
                             + female
                             + beltuse
                             + body_van
                             + body_suv
                             + body_pickup
                             + multicoll
                             + pdof_front
                             + pdof_nearside
                             + pdof_farside,
                             design = design_svy_65_plus)

pred_svy_65_plus <- predict(fit_svy_65_plus, type="response")
roc_svy_65_plus <- roc(df_65_plus$iss90_16_plus, pred_svy_65_plus)
AUC_svy_65_plus <- as.numeric(auc(roc_svy_65_plus))

plot_roc_65_plus_wt <- data.frame(Specificity=vector(,length(roc_svy_65_plus$specificities)), Sensitivity = vector(,length(roc_svy_65_plus$specificities)), Group = vector(,length(roc_svy_65_plus$specificities)))
plot_roc_65_plus_wt$Specificity <- roc_svy_65_plus$specificities
plot_roc_65_plus_wt$Sensitivity <- roc_svy_65_plus$sensitivities

# Figure 2 ROC curves ----

plot_roc <- rbind(plot_roc_15_54, plot_roc_45_plus, plot_roc_55_plus, plot_roc_65_plus)


roc_plot <- ggplot(plot_roc, aes(x = Specificity, y = Sensitivity, linetype = Group, color = Group)) +
  geom_line() +
  scale_x_reverse(name = "Specificity", limits = c(1,0)) +
  scale_y_continuous(name = "Sensitivity", limits = c(0,1)) +
  scale_linetype_manual(values = c("solid", "dotted", "dashed", "solid")) +
  scale_color_manual(values = c("#000000", "#A6ACAF", "#000000", "#717B7D")) +
  theme_bw() +
  theme(legend.position = c(0.7,0.2)) +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=14)) +
  theme(text = element_text(size=16))

# Figure 3 AUC sweeps ----

median_age<- matrix(, nrow = 1000, ncol=71)
auc_sweeps <- matrix(, nrow = 1000, ncol=71)
num_cases  <- matrix(, nrow = 1000, ncol=71)

train_idx<- vector("list",1000)
fit_glm   <- vector("list",1000)
df_train  <- vector("list",1000)
df_test   <- vector("list",1000)

for (i in 1:1000){
  print(i)
  # Split into train and test
  train_idx[[i]] <- createDataPartition(df$age, p = 0.8, list=FALSE)
  df_train <- df[train_idx[[i]],]
  df_test  <- df[-train_idx[[i]],]

  fit_glm[[i]] <- glm(iss90_16_plus ~ log(dv_mph)
                      + age
                      + female
                      + beltuse
                      + body_van
                      + body_suv
                      + body_pickup
                      + multicoll
                      + pdof_front
                      + pdof_nearside
                      + pdof_farside,
                      data = df_train, family = binomial(link=logit))
  counter <- 1

  for (j in 15:85){
    if (j < 15) {
      min_age <- 0
    } else {
      min_age <- j - 10
    }
    
    if (j > 75) {
      max_age <- 85
    } else {
      max_age <- j + 10
    }

    cases <- subset(df_test, age >= min_age & age <= max_age)
    pred_cases <- predict(fit_glm[[i]], type="response", newdata = cases)
    roc_cases  <- roc(cases$iss90_16_plus, pred_cases)
    auc_cases  <- as.numeric(auc(roc_cases))

    median_age[i,counter] <- j
    auc_sweeps[i,counter] <- auc_cases
    num_cases[i,counter] <- nrow(cases)

    counter <- counter + 1
  }
}

plot(x = median_age[1,], y=colMeans(auc_sweeps), ylim = c(0.7,0.95), xlab = 'Age', ylab='Average AUC', type='l')
lRange <- apply(auc_sweeps, 2, quantile, probs = c(0.025))
uRange <- apply(auc_sweeps, 2, quantile, probs = c(0.975))

polygon(c(median_age[1,],rev(median_age[1,])),c(lRange,rev(uRange)),col = "grey75", border = FALSE)
lines(median_age[1,], y=colMeans(auc_sweeps), lwd = 2)

lines(median_age[1,], uRange, col="grey50",lty=2)
lines(median_age[1,], lRange, col="grey50",lty=2)