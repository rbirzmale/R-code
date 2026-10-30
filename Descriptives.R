##############################################################
##################### DESCRIPTIVES ###########################
##############################################################

##################### SETUP ##################################
##############################################################
#install.packages("tidyverse")
library(psych)
library(readxl)
library(tidyverse)

# Set working directlyory & Open the data file
setwd("~/Desktop/RWorkingFolder")
Datafile <- read_excel("Dataset.xlsx")

#Turns it numeric
Datafile$AveragePupilSize <- as.numeric(as.character(Datafile$AveragePupilSize))

######## Transform baseline fatigue variable from categorical to nominal  ###########
### WARNING: assumes the gap by 1 is identical in all cases ###

Datafile <- Datafile %>%
  mutate(Baseline_Fatigue_num = case_when(
    Baseline_Fatigue == "Not at all"  ~ 1,
    Baseline_Fatigue == "Slightly"    ~ 2,
    Baseline_Fatigue == "Moderately"  ~ 3,
    Baseline_Fatigue == "Very"        ~ 4,
    Baseline_Fatigue == "Extremely"   ~ 5   
  ))

# Create one row per participant
# Average pupil size = mean across the 8 trials
# During-task fatigue = mean across the 8 trials

Descriptives <- Datafile %>%
  group_by(ParticipantID) %>%
  summarise(
    Baseline_Fatigue = first(Baseline_Fatigue_num),
    Conscientiousness = first(Conscientiousness),
    Age = first(Age),
    Gender = first(Gender),
    CognitiveFatigueDuringTask = mean(CognitiveFatigueDuringTask, na.rm = TRUE),
    AveragePupilSize = mean(AveragePupilSize, na.rm = TRUE),
    TaskDemand = mean(TaskDemand, na.rm = TRUE),
    TaskDuration = mean(TaskDuration, na.rm = TRUE),
    .groups = "drop"
  )


# Table output
Descriptives_Table <- tibble(
  Variable = c(
    "Age",
    "Baseline fatigue",
    "Cognitive fatigue during task",
    "Conscientiousness",
    "Average pupil size",
    "Task demand",
    "Task duration"
  ),
  
  M = c(
    mean(Descriptives$Age, na.rm = TRUE),
    mean(Descriptives$Baseline_Fatigue, na.rm = TRUE),
    mean(Descriptives$CognitiveFatigueDuringTask, na.rm = TRUE),
    mean(Descriptives$Conscientiousness, na.rm = TRUE),
    mean(Descriptives$AveragePupilSize, na.rm = TRUE),
    mean(Descriptives$TaskDemand, na.rm = TRUE),
    mean(Descriptives$TaskDuration, na.rm = TRUE)
  ),
  
  SD = c(
    sd(Descriptives$Age, na.rm = TRUE),
    sd(Descriptives$Baseline_Fatigue, na.rm = TRUE),
    sd(Descriptives$CognitiveFatigueDuringTask, na.rm = TRUE),
    sd(Descriptives$Conscientiousness, na.rm = TRUE),
    sd(Descriptives$AveragePupilSize, na.rm = TRUE),
    sd(Descriptives$TaskDemand, na.rm = TRUE),
    sd(Descriptives$TaskDuration, na.rm = TRUE)
  ),
  
  Min = c(
    min(Descriptives$Age, na.rm = TRUE),
    min(Descriptives$Baseline_Fatigue, na.rm = TRUE),
    min(Descriptives$CognitiveFatigueDuringTask, na.rm = TRUE),
    min(Descriptives$Conscientiousness, na.rm = TRUE),
    min(Descriptives$AveragePupilSize, na.rm = TRUE),
    min(Descriptives$TaskDemand, na.rm = TRUE),
    min(Descriptives$TaskDuration, na.rm = TRUE)
  ),
  
  Max = c(
    max(Descriptives$Age, na.rm = TRUE),
    max(Descriptives$Baseline_Fatigue, na.rm = TRUE),
    max(Descriptives$CognitiveFatigueDuringTask, na.rm = TRUE),
    max(Descriptives$Conscientiousness, na.rm = TRUE),
    max(Descriptives$AveragePupilSize, na.rm = TRUE),
    max(Descriptives$TaskDemand, na.rm = TRUE),
    max(Descriptives$TaskDuration, na.rm = TRUE)
  )
)

# Round to 3 decimal places
Descriptives_Table <- Descriptives_Table %>%
  mutate(
    M = round(M, 3),
    SD = round(SD, 3),
    Min = round(Min, 3),
    Max = round(Max, 3)
  )

# View table
Descriptives_Table

##############################################################
### Intercorrelations ########################################
##############################################################

# Select variables for correlation matrix
CorrelationData <- Descriptives %>%
  select(
    Baseline_Fatigue,
    CognitiveFatigueDuringTask,
    Conscientiousness,
    AveragePupilSize,
    TaskDemand
  )

# Pearson correlations
CorrelationMatrix <- cor(
  CorrelationData,
  use = "pairwise.complete.obs",
  method = "pearson"
)

# Round correlations to 3 decimal places
CorrelationMatrix <- round(CorrelationMatrix, 3)

# View correlation matrix
CorrelationMatrix

library(psych)

CorrelationData <- Descriptives %>%
  select(
    Baseline_Fatigue,
    CognitiveFatigueDuringTask,
    Conscientiousness,
    AveragePupilSize,
    TaskDemand
  )

CorrelationResults <- corr.test(
  CorrelationData,
  use = "pairwise",
  method = "pearson",
  adjust = "none"
)

CorrelationResults$r      # Correlation coefficients
CorrelationResults$p      # p-values

##############################################################
### Visuals ##################################################
##############################################################

ggplot(Datafile, aes(x = CognitiveFatigueDuringTask)) +
  geom_histogram(
    binwidth = 0.5,
    boundary = 0
  ) +
  labs(
    title = "Distribution of During-Task Cognitive Fatigue",
    x = "Cognitive Fatigue During Task",
    y = "Frequency"
  )

#Exact numbers
table(Datafile$CognitiveFatigueDuringTask, useNA = "ifany")

# Short datafile for baseline fatigue visualization

DataFileShort <- Datafile %>%
  group_by(ParticipantID) %>% # treats each participant as a separate group
  summarise( 
    Baseline_Fatigue_num = first(Baseline_Fatigue_num), #Takes the first value of baseline fatigue (numreic) from the dataset (Between-subject (BS) variable)
    .groups = "drop" #drops the participantID grouping done earlier
  ) %>%
  arrange(as.numeric(gsub("P", "", ParticipantID))) #Sorts the dataset by participant number


ggplot(DataFileShort, aes(x = Baseline_Fatigue_num)) +
  geom_histogram(
    binwidth = 1,
    boundary = 1
  ) +
  labs(
    title = "Distribution of Baseline Fatigue",
    x = "Baseline Fatigue",
    y = "Frequency"
  ) +
  scale_x_continuous(
    breaks = 1:5,
    labels = c("Not at all", "Slightly", "Moderately", "Very", "Extremely")
  )

#Exact numbers
table(DataFileShort$Baseline_Fatigue_num, useNA = "ifany")