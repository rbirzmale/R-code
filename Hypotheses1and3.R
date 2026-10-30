##############################################################
##################### SETUP ##################################
##############################################################
#install.packages("tidyverse")

library(readxl)
library(tidyverse)
library(lm.beta)
# Set working directlyory & Open the data file
setwd("~/Desktop/RWorkingFolder")
DataFile <- read_excel("Dataset.xlsx")

######## Transform baseline fatigue variable from categorical to nominal  ###########
### WARNING: assumes the gap by 1 is identical in all cases ###

DataFile <- DataFile %>%
  mutate(Baseline_Fatigue_num = case_when(
    Baseline_Fatigue == "Not at all"  ~ 1,
    Baseline_Fatigue == "Slightly"    ~ 2,
    Baseline_Fatigue == "Moderately"  ~ 3,
    Baseline_Fatigue == "Very"        ~ 4,
    Baseline_Fatigue == "Extremely"   ~ 5   
  ))

#Center variables
DataFile <- DataFile %>%
  mutate(
    Baseline_Fatigue_num_CGM = Baseline_Fatigue_num  - mean(Baseline_Fatigue_num , na.rm = TRUE)
  )

DataFile <- DataFile %>%
  mutate(
    Conscientiousness_CGM = Conscientiousness - mean(Conscientiousness, na.rm = TRUE)
  )

######## Change how variables are treated (from character to numerical)  ###########
# lapply(DataFile, class) #Code to find out what class all variables are

DataFile$AveragePupilSize <- as.numeric(as.character(DataFile$AveragePupilSize))

################################################################ 
### New dataset with only relevant between-subject variables ### 
################################################################ 

DataFileShort <- DataFile %>%
  group_by(ParticipantID) %>% # treats each participant as a separate group
  summarise( 
    AveragePupilSize = mean(AveragePupilSize, na.rm = TRUE), #Leaves only the average Pupil Size from all 8 session for 1 participant 
    Baseline_Fatigue_num = first(Baseline_Fatigue_num_CGM), #Takes the first value of baseline fatigue (numreic) from the dataset (Between-subject (BS) variable)
    Conscientiousness = first(Conscientiousness_CGM), #Takes the first value of conscientiousness from the dataset (BS variable)
    .groups = "drop" #drops the participantID grouping done earlier
  ) %>%
  arrange(as.numeric(gsub("P", "", ParticipantID))) #Sorts the dataset by participant number

################################################################ 
### Hypothesis 1 regression analysis ########################### 
################################################################ 
# Hypothesis 1: Baseline fatigue relates positively to overall exerted cognitive
# effort (as measured by average pupil dilation) across all tasks.

model_hypothesis_1 <- lm(
  AveragePupilSize ~ Baseline_Fatigue_num, #Outcome = Averaged Pupil Size across all 8 sessions; Predictor = Baseline_Fatigue
  data = DataFileShort #Uses the dataset with the BS variables
)

#summary(model_hypothesis_1) #Provides output for hypothesis 1
confint(model_hypothesis_1, level = 0.95) 
model_beta1 <- lm.beta(model_hypothesis_1) 
summary(model_beta1)

################################################################ 
###  Hypothesis 3 regression analysis ##########################
################################################################ 
# Hypothesis 3: Conscientiousness moderates the relationship between baseline
# fatigue and overall exerted cognitive effort. When conscientiousness is high,
# the relationship is stronger.

model_hypothesis_3 <- lm(
  AveragePupilSize ~ Baseline_Fatigue_num * Conscientiousness, #Outcome = Averaged Pupil Size across all 8 sessions; Predictors = Baseline_Fatigue & Conscientiousness. * Marks interaction effect to test moderation
  data = DataFileShort #Uses the dataset with the BS variables
)

#summary(model_hypothesis_3) #Provides output for hypothesis 3
confint(model_hypothesis_3, level = 0.95)
model_beta3 <- lm.beta(model_hypothesis_3)
summary(model_beta3)
