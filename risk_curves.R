# MVC tables and figures

# Load some libraries ----
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rms)
library(boot)
library(pROC)
library(survey)
library(caret)
library(gganimate)

# Load the dataset ----
df <- read_csv("NASS_filtered_06092019.csv")
df <- df %>% select(dv_mph, age, female, modelyr, beltuse, body_van, body_suv, body_pickup, body_car, multicoll, pdof_front, pdof_nearside, pdof_farside, pdof_rear, iss90_16_plus, ratwgt, stratif, psu, psustrat, age_15_19, age_20_24, age_25_29, age_30_34, age_35_39, age_40_44, age_45_49, age_50_54, age_55_59, age_60_64, age_65_69, age_70_74, age_75_plus)
df <- na.omit(df)
df_15_54 <- df %>% filter(age <= 54)
df_55_plus <- df %>% filter(age >= 55)
df_65_plus <- df %>% filter(age >= 65)

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

# Gender ----
male <- data.frame()

for (i in 15:95) {
    curr_df                     <- data.frame(age = rep(i, 51), 
                                           dv_mph = seq(10, 60, by=1),
                                           female = rep(0,51),
                                           beltuse = rep(1,51),
                                           body_van = rep(0,51),
                                           body_suv = rep(0,51),
                                           body_pickup = rep(0,51),
                                           body_car = rep(1,51),
                                           multicoll = rep(0,51),
                                           pdof_front = rep(1,51),
                                           pdof_nearside = rep(0,51),
                                           pdof_farside = rep(0,51),
                                           pdof_rear = rep(0,51),
                                           type = rep('Male',51))
    
    male <- rbind(male, curr_df)
    
}

male$iss90_16_plus <- predict(fit_glm_all, newdata = male, type='response')

female <- data.frame()

for (i in 15:95) {
  curr_df                     <- data.frame(age = rep(i, 51), 
                                            dv_mph = seq(10, 60, by=1),
                                            female = rep(1,51),
                                            beltuse = rep(1,51),
                                            body_van = rep(0,51),
                                            body_suv = rep(0,51),
                                            body_pickup = rep(0,51),
                                            body_car = rep(1,51),
                                            multicoll = rep(0,51),
                                            pdof_front = rep(1,51),
                                            pdof_nearside = rep(0,51),
                                            pdof_farside = rep(0,51),
                                            pdof_rear = rep(0,51),
                                            type = rep('Female',51))
  
  female <- rbind(female, curr_df)
  
}

female$iss90_16_plus <- predict(fit_glm_all, newdata = female, type='response')

all_gender_data <- rbind(male, female)

ggplot(all_gender_data, aes(dv_mph, iss90_16_plus, group=female)) +
  geom_line(aes(linetype=type, color=type)) +
  transition_time(age) +
  labs(title = 'Age: {frame_time}', x = 'MPH', y = 'Probability of Severe Injury') +
  theme(legend.position = c(0.2,0.85)) +
  theme(legend.title = element_blank()) +
  theme(text = element_text(size=18))

# PDOF ----

front <- data.frame()

for (i in 15:95) {
  curr_df                     <- data.frame(age = rep(i, 51), 
                                            dv_mph = seq(10, 60, by=1),
                                            female = rep(0,51),
                                            beltuse = rep(1,51),
                                            body_van = rep(0,51),
                                            body_suv = rep(0,51),
                                            body_pickup = rep(0,51),
                                            body_car = rep(1,51),
                                            multicoll = rep(0,51),
                                            pdof_front = rep(1,51),
                                            pdof_nearside = rep(0,51),
                                            pdof_farside = rep(0,51),
                                            pdof_rear = rep(0,51),
                                            type = rep('Front',51))
  
  front <- rbind(front, curr_df)
  
}

front$iss90_16_plus <- predict(fit_glm_all, newdata = front, type='response')

nearside <- data.frame()

for (i in 15:95) {
  curr_df                     <- data.frame(age = rep(i, 51), 
                                            dv_mph = seq(10, 60, by=1),
                                            female = rep(0,51),
                                            beltuse = rep(1,51),
                                            body_van = rep(0,51),
                                            body_suv = rep(0,51),
                                            body_pickup = rep(0,51),
                                            body_car = rep(1,51),
                                            multicoll = rep(0,51),
                                            pdof_front = rep(0,51),
                                            pdof_nearside = rep(1,51),
                                            pdof_farside = rep(0,51),
                                            pdof_rear = rep(0,51),
                                            type = rep('Near side',51))
  
  nearside <- rbind(nearside, curr_df)
  
}

nearside$iss90_16_plus <- predict(fit_glm_all, newdata = nearside, type='response')

farside <- data.frame()

for (i in 15:95) {
  curr_df                     <- data.frame(age = rep(i, 51), 
                                            dv_mph = seq(10, 60, by=1),
                                            female = rep(0,51),
                                            beltuse = rep(1,51),
                                            body_van = rep(0,51),
                                            body_suv = rep(0,51),
                                            body_pickup = rep(0,51),
                                            body_car = rep(1,51),
                                            multicoll = rep(0,51),
                                            pdof_front = rep(0,51),
                                            pdof_nearside = rep(0,51),
                                            pdof_farside = rep(1,51),
                                            pdof_rear = rep(0,51),
                                            type = rep('Far side',51))
  
  farside <- rbind(farside, curr_df)
  
}

farside$iss90_16_plus <- predict(fit_glm_all, newdata = farside, type='response')

rear <- data.frame()

for (i in 15:95) {
  curr_df                     <- data.frame(age = rep(i, 51), 
                                            dv_mph = seq(10, 60, by=1),
                                            female = rep(0,51),
                                            beltuse = rep(1,51),
                                            body_van = rep(0,51),
                                            body_suv = rep(0,51),
                                            body_pickup = rep(0,51),
                                            body_car = rep(1,51),
                                            multicoll = rep(0,51),
                                            pdof_front = rep(0,51),
                                            pdof_nearside = rep(0,51),
                                            pdof_farside = rep(0,51),
                                            pdof_rear = rep(1,51),
                                            type = rep('Rear',51))
  
  rear <- rbind(rear, curr_df)
  
}

rear$iss90_16_plus <- predict(fit_glm_all, newdata = rear, type='response')

all_pdof_data <- rbind(front, nearside, farside, rear)

ggplot(all_pdof_data, aes(dv_mph, iss90_16_plus, group=type)) +
  geom_line(aes(linetype=type, color=type)) +
  transition_time(age) +
  labs(title = 'Age: {frame_time}', x = 'MPH', y = 'Probability of Severe Injury') +
  theme(legend.position = c(0.2,0.85)) +
  theme(legend.title = element_blank()) +
  theme(text = element_text(size=18))

# Body Type ----

van <- data.frame()

for (i in 15:95) {
  curr_df                     <- data.frame(age = rep(i, 51), 
                                            dv_mph = seq(10, 60, by=1),
                                            female = rep(0,51),
                                            beltuse = rep(1,51),
                                            body_van = rep(1,51),
                                            body_suv = rep(0,51),
                                            body_pickup = rep(0,51),
                                            body_car = rep(0,51),
                                            multicoll = rep(0,51),
                                            pdof_front = rep(1,51),
                                            pdof_nearside = rep(0,51),
                                            pdof_farside = rep(0,51),
                                            pdof_rear = rep(0,51),
                                            type = rep('Van',51))
  
  van <- rbind(van, curr_df)
  
}

van$iss90_16_plus <- predict(fit_glm_all, newdata = van, type='response')

suv <- data.frame()

for (i in 15:95) {
  curr_df                     <- data.frame(age = rep(i, 51), 
                                            dv_mph = seq(10, 60, by=1),
                                            female = rep(0,51),
                                            beltuse = rep(1,51),
                                            body_van = rep(0,51),
                                            body_suv = rep(1,51),
                                            body_pickup = rep(0,51),
                                            body_car = rep(0,51),
                                            multicoll = rep(0,51),
                                            pdof_front = rep(1,51),
                                            pdof_nearside = rep(0,51),
                                            pdof_farside = rep(0,51),
                                            pdof_rear = rep(0,51),
                                            type = rep('SUV',51))
  
  suv <- rbind(suv, curr_df)
  
}

suv$iss90_16_plus <- predict(fit_glm_all, newdata = suv, type='response')

pickup <- data.frame()

for (i in 15:95) {
  curr_df                     <- data.frame(age = rep(i, 51), 
                                            dv_mph = seq(10, 60, by=1),
                                            female = rep(0,51),
                                            beltuse = rep(1,51),
                                            body_van = rep(0,51),
                                            body_suv = rep(0,51),
                                            body_pickup = rep(1,51),
                                            body_car = rep(0,51),
                                            multicoll = rep(0,51),
                                            pdof_front = rep(1,51),
                                            pdof_nearside = rep(0,51),
                                            pdof_farside = rep(0,51),
                                            pdof_rear = rep(0,51),
                                            type = rep('Pickup',51))
  
  pickup <- rbind(pickup, curr_df)
  
}

pickup$iss90_16_plus <- predict(fit_glm_all, newdata = pickup, type='response')

car <- data.frame()

for (i in 15:95) {
  curr_df                     <- data.frame(age = rep(i, 51), 
                                            dv_mph = seq(10, 60, by=1),
                                            female = rep(0,51),
                                            beltuse = rep(1,51),
                                            body_van = rep(0,51),
                                            body_suv = rep(0,51),
                                            body_pickup = rep(0,51),
                                            body_car = rep(1,51),
                                            multicoll = rep(0,51),
                                            pdof_front = rep(1,51),
                                            pdof_nearside = rep(0,51),
                                            pdof_farside = rep(0,51),
                                            pdof_rear = rep(0,51),
                                            type = rep('Car',51))
  
  car <- rbind(car, curr_df)
  
}

car$iss90_16_plus <- predict(fit_glm_all, newdata = car, type='response')

all_body_data <- rbind(van, suv, pickup, car)

ggplot(all_body_data, aes(dv_mph, iss90_16_plus, group=type)) +
  geom_line(aes(linetype=type, color=type)) +
  transition_time(age) +
  labs(title = 'Age: {frame_time}', x = 'MPH', y = 'Probability of Severe Injury') +
  theme(legend.position = c(0.2,0.85)) +
  theme(legend.title = element_blank()) +
  theme(text = element_text(size=18))

# Belt use and number of events ----

beltedsingle <- data.frame()

for (i in 15:95) {
  curr_df                     <- data.frame(age = rep(i, 51), 
                                            dv_mph = seq(10, 60, by=1),
                                            female = rep(0,51),
                                            beltuse = rep(1,51),
                                            body_van = rep(0,51),
                                            body_suv = rep(0,51),
                                            body_pickup = rep(0,51),
                                            body_car = rep(1,51),
                                            multicoll = rep(0,51),
                                            pdof_front = rep(1,51),
                                            pdof_nearside = rep(0,51),
                                            pdof_farside = rep(0,51),
                                            pdof_rear = rep(0,51),
                                            type = rep('Belted, Single',51))
  
  beltedsingle <- rbind(beltedsingle, curr_df)
  
}

beltedsingle$iss90_16_plus <- predict(fit_glm_all, newdata = beltedsingle, type='response')

beltedmultiple <- data.frame()

for (i in 15:95) {
  curr_df                     <- data.frame(age = rep(i, 51), 
                                            dv_mph = seq(10, 60, by=1),
                                            female = rep(0,51),
                                            beltuse = rep(1,51),
                                            body_van = rep(0,51),
                                            body_suv = rep(0,51),
                                            body_pickup = rep(0,51),
                                            body_car = rep(1,51),
                                            multicoll = rep(1,51),
                                            pdof_front = rep(1,51),
                                            pdof_nearside = rep(0,51),
                                            pdof_farside = rep(0,51),
                                            pdof_rear = rep(0,51),
                                            type = rep('Belted, Multiple',51))
  
  beltedmultiple <- rbind(beltedmultiple, curr_df)
  
}

beltedmultiple$iss90_16_plus <- predict(fit_glm_all, newdata = beltedmultiple, type='response')

unbeltedsingle <- data.frame()

for (i in 15:95) {
  curr_df                     <- data.frame(age = rep(i, 51), 
                                            dv_mph = seq(10, 60, by=1),
                                            female = rep(0,51),
                                            beltuse = rep(0,51),
                                            body_van = rep(0,51),
                                            body_suv = rep(0,51),
                                            body_pickup = rep(0,51),
                                            body_car = rep(1,51),
                                            multicoll = rep(0,51),
                                            pdof_front = rep(1,51),
                                            pdof_nearside = rep(0,51),
                                            pdof_farside = rep(0,51),
                                            pdof_rear = rep(0,51),
                                            type = rep('Unbelted, Single',51))
  
  unbeltedsingle <- rbind(unbeltedsingle, curr_df)
  
}

unbeltedsingle$iss90_16_plus <- predict(fit_glm_all, newdata = unbeltedsingle, type='response')

unbeltedmultiple <- data.frame()

for (i in 15:95) {
  curr_df                     <- data.frame(age = rep(i, 51), 
                                            dv_mph = seq(10, 60, by=1),
                                            female = rep(0,51),
                                            beltuse = rep(0,51),
                                            body_van = rep(0,51),
                                            body_suv = rep(0,51),
                                            body_pickup = rep(0,51),
                                            body_car = rep(1,51),
                                            multicoll = rep(1,51),
                                            pdof_front = rep(1,51),
                                            pdof_nearside = rep(0,51),
                                            pdof_farside = rep(0,51),
                                            pdof_rear = rep(0,51),
                                            type = rep('Unbelted, Multiple',51))
  
  unbeltedmultiple <- rbind(unbeltedmultiple, curr_df)
  
}

unbeltedmultiple$iss90_16_plus <- predict(fit_glm_all, newdata = unbeltedmultiple, type='response')

all_beltimpact_data <- rbind(beltedsingle, beltedmultiple, unbeltedsingle, unbeltedmultiple)

ggplot(all_beltimpact_data, aes(dv_mph, iss90_16_plus, group=type)) +
  geom_line(aes(linetype=type, color=type)) +
  transition_time(age) +
  labs(title = 'Age: {frame_time}', x = 'MPH', y = 'Probability of Severe Injury') +
  theme(legend.position = c(0.2,0.85)) +
  theme(legend.title = element_blank()) +
  theme(text = element_text(size=18))