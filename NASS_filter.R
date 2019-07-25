# ----
#
# Christina Gancayco
#
# Read in NASS data and filter based on inclusion criteria from Kokonen et al 2011

setwd("~/Desktop/MVC/")
# Import libraries ----

library(dplyr)

# Load NASS data ----


load("AllNASS.rdata")

# Create dataframe for accident analysis ----

# Empty dataframe
NASS_analysis = occupants[,FALSE]

# Get collision variables
NASS_analysis <- occupants[, c("caseid",
                               "year",
                               "month",
                               "pdof1",
                               "pdof2",
                               "dvtotal",
                               "make",
                               "model",
                               "modelyr",
                               "rollover",
                               "towpar",
                               "vehtype",
                               "bodytype",
                               "bagdeply")]

# Occupant-specific variables
NASS_analysis <- cbind(NASS_analysis,
                       occupants[, c("manuse",
                                     "age",
                                     "sex",
                                     "height",
                                     "weight",
                                     "ejection",
                                     "treatmnt",
                                     "seatpos",
                                     "death",
                                     "glasgow")])

# Injury variables
NASS_analysis$iss90 <- occupants$iss


# Create collision and occupant flags ----

# Flag multiple impacts
NASS_analysis$multicoll <- ifelse(is.na(occupants$objcont2), 0 , 1)

# Flag rollovers
NASS_analysis$rolled <- ifelse(occupants$rollover > 0, 1, 0)

# Flag cars, vans, and trucks based on body type
bodycodes_car        <- c(1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 17)
bodycodes_suv        <- c(14, 15, 16, 19)
bodycodes_van        <- c(20, 21, 22, 23, 24, 25, 28, 29)
bodycodes_pickup     <- c(30, 31, 32, 33, 39, 74, 79)
bodycodes_truck      <- c(40, 41, 42, 45, 48, 49, 50, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 78, 79)
bodycodes_motorcycle <- c(80, 81)
bodycodes_other      <- c(13, 82, 88, 89, 90, 91, 92, 93, 97)
bodycodes_unknown    <- c(9, 47)

NASS_analysis$body_car        <- ifelse(NASS_analysis$bodytype %in% bodycodes_car, 1, 0)
NASS_analysis$body_suv        <- ifelse(NASS_analysis$bodytype %in% bodycodes_suv, 1, 0)
NASS_analysis$body_van        <- ifelse(NASS_analysis$bodytype %in% bodycodes_van, 1, 0)
NASS_analysis$body_pickup     <- ifelse(NASS_analysis$bodytype %in% bodycodes_pickup, 1, 0)
NASS_analysis$body_truck      <- ifelse(NASS_analysis$bodytype %in% bodycodes_truck, 1, 0)
NASS_analysis$body_motorcycle <- ifelse(NASS_analysis$bodytype %in% bodycodes_motorcycle, 1, 0)
NASS_analysis$body_other      <- ifelse(NASS_analysis$bodytype %in% bodycodes_other, 1, 0)
NASS_analysis$body_unknown    <- ifelse(NASS_analysis$bodytype %in% bodycodes_unknown, 1, 0)

# Create categories for PDOF
NASS_analysis$pdof_front <- ifelse((occupants$pdof1 > 315 & occupants$pdof1 <= 360) | (occupants$pdof1 < 45), 1, 0)
NASS_analysis$pdof_right <- ifelse(occupants$pdof1 >= 45 & occupants$pdof1 < 135, 1, 0)
NASS_analysis$pdof_rear  <- ifelse(occupants$pdof1 >= 135 & occupants$pdof1 < 225, 1, 0)
NASS_analysis$pdof_left  <- ifelse(occupants$pdof1 >= 225 & occupants$pdof1 < 315, 1, 0)

# Nearside/farside flags
NASS_analysis$seat_side <- occupants$seatpos %% 10
NASS_analysis$seat_row  <- floor(occupants$seatpos / 10)

NASS_analysis$pdof_nearside <- ifelse((NASS_analysis$pdof_left == 1 & NASS_analysis$seat_side == 1) | (NASS_analysis$pdof_right == 1 & NASS_analysis$seat_side == 3), 1, 0)
NASS_analysis$pdof_farside  <- ifelse((NASS_analysis$pdof_left == 1 | NASS_analysis$pdof_right == 1) & NASS_analysis$pdof_nearside == 0, 1, 0)

# Set ejection flag
NASS_analysis$eject <- ifelse(NASS_analysis$ejection %in% c(1, 2, 3), 1, 0)

# Set gender flag
NASS_analysis$female <- ifelse(NASS_analysis$sex == 1, 0, 1)

# Set belt use flag
NASS_analysis$beltuse <- ifelse(NASS_analysis$manuse == 4, 1, 0)

# Set fatal flag
NASS_analysis$fatal <- ifelse(NASS_analysis$death > 0, 1, 0)

# Store weights and stratifications

NASS_analysis$ratwgt       <- occupants$ratwgt
NASS_analysis$stratif      <- occupants$stratif
NASS_analysis$psu          <- occupants$psu
NASS_analysis$psustrat     <- occupants$psustrat
NASS_analysis$psuwgt       <- occupants$psuwgt

# Set ISS > 15 flag
NASS_analysis$iss90_16_plus <- ifelse(NASS_analysis$iss90 >= 16, 1, 0)

# Subset based on exclusion criteria ----

# 1. Remove cases with crash year < 2000 and model year < 2000
NASS_analysis_reduce <- subset(NASS_analysis, year >= 2000)
NASS_analysis_reduce <- subset(NASS_analysis_reduce, modelyr >= 2000)

# 2. Remove children
NASS_analysis_reduce <- subset(NASS_analysis_reduce, age >= 15)

# 3. Remove ejected occupants
#NASS_analysis_reduce <- subset(NASS_analysis_reduce, eject == 0)

# 4. Remove rollovers
NASS_analysis_reduce <- subset(NASS_analysis_reduce, rolled == 0)

# 5. Remove cases not involving car, SUV, van, or pickup
NASS_analysis_reduce <- subset(NASS_analysis_reduce, body_car == 1 | body_suv == 1 | body_pickup == 1 | body_van == 1)

# 6. Remove cases missing PDOF
NASS_analysis_reduce <- subset(NASS_analysis_reduce, pdof1 >= 0 & pdof1 <= 360)

# 7. Remove cases missing deltaV
NASS_analysis_reduce <- subset(NASS_analysis_reduce, !is.na(dvtotal))

# 8. Remove cases missing beltuse
NASS_analysis_reduce <- subset(NASS_analysis_reduce, !is.na(beltuse))

# 9. Remove cases that were not front row occupants
NASS_analysis_reduce <- subset(NASS_analysis_reduce, seatpos == 11 | seatpos == 13 | seatpos == 21 | seatpos == 23 | seatpos == 22)

# 10. New stuff
# Keep cases with deltaV >= 15mph or airbag deployment
NASS_analysis_reduce$dv_mph <- NASS_analysis_reduce$dvtotal * 0.621371
NASS_analysis_reduce        <- subset(NASS_analysis_reduce, dv_mph >= 15 | bagdeply == 1)
# Convert age to binary
NASS_analysis_reduce$age_45_plus <- ifelse(NASS_analysis_reduce$age >= 45, 1, 0)
# Age sweeps
#NASS_analysis_reduce$age_0_14 <- ifelse(NASS_analysis_reduce$age < 15, 1, 0)
NASS_analysis_reduce$age_15_19 <- ifelse(NASS_analysis_reduce$age >= 15 & NASS_analysis_reduce$age <= 19, 1, 0)
NASS_analysis_reduce$age_20_24 <- ifelse(NASS_analysis_reduce$age >= 20 & NASS_analysis_reduce$age <= 24, 1, 0)
NASS_analysis_reduce$age_25_29 <- ifelse(NASS_analysis_reduce$age >= 25 & NASS_analysis_reduce$age <= 29, 1, 0)
NASS_analysis_reduce$age_30_34 <- ifelse(NASS_analysis_reduce$age >= 30 & NASS_analysis_reduce$age <= 34, 1, 0)
NASS_analysis_reduce$age_35_39 <- ifelse(NASS_analysis_reduce$age >= 35 & NASS_analysis_reduce$age <= 39, 1, 0)
NASS_analysis_reduce$age_40_44 <- ifelse(NASS_analysis_reduce$age >= 40 & NASS_analysis_reduce$age <= 44, 1, 0)
NASS_analysis_reduce$age_45_49 <- ifelse(NASS_analysis_reduce$age >= 45 & NASS_analysis_reduce$age <= 49, 1, 0)
NASS_analysis_reduce$age_50_54 <- ifelse(NASS_analysis_reduce$age >= 50 & NASS_analysis_reduce$age <= 54, 1, 0)
NASS_analysis_reduce$age_55_59 <- ifelse(NASS_analysis_reduce$age >= 55 & NASS_analysis_reduce$age <= 59, 1, 0)
NASS_analysis_reduce$age_60_64 <- ifelse(NASS_analysis_reduce$age >= 60 & NASS_analysis_reduce$age <= 64, 1, 0)
NASS_analysis_reduce$age_65_69 <- ifelse(NASS_analysis_reduce$age >= 65 & NASS_analysis_reduce$age <= 69, 1, 0)
NASS_analysis_reduce$age_70_74 <- ifelse(NASS_analysis_reduce$age >= 70 & NASS_analysis_reduce$age <= 74, 1, 0)
NASS_analysis_reduce$age_75_plus <- ifelse(NASS_analysis_reduce$age >= 75, 1, 0)


# Write new dataset to file ----
write.csv(NASS_analysis_reduce, "NASS_filtered_06092019.csv")
