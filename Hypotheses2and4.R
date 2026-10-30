#################################################################
##################### SETUP #####################################
#################################################################
#install.packages("r2mlm")
library(readxl)
library(tidyverse)
library(lme4)
library(lmerTest)


# Set working directory & Open the data file
setwd("~/Desktop/RWorkingFolder")
DataFile <- read_excel("Dataset.xlsx")

######## Change how variables are treated (from character to numerical)  ###########
# lapply(DataFile, class) #Code to find out what class all variables are

DataFile$AveragePupilSize <- as.numeric(as.character(DataFile$AveragePupilSize))

#Ensures Task and Difficulty are factors
DataFile <- DataFile %>%
  mutate(
    TaskType=factor(TaskType),
    TaskDifficulty=factor(TaskDifficulty)
  )

################################################################
### Variable modification ######################################
################################################################
# Create a CWC variable, used in Hypothesis 2 and 4
# For every participant, it subtracts their Cognitive fatigue score from the 
# participants overall Cognitive Fatigue score mean across all 8 sessions.
# Returns a value that indicates whether the participants Cognitive Fatigue 
# score is above, below or similar to the specific participant's average.
# Close to 0 means around average. Below 0 indicates below average, above 0 
# means above average.

DataFile <- DataFile %>%
  group_by(ParticipantID) %>%
  mutate(
    CognitiveFatigueDuringTask_CWC =
      CognitiveFatigueDuringTask -
      mean(CognitiveFatigueDuringTask, na.rm = TRUE) 
  ) %>%
  ungroup()

# Centers Conscientiousness around its grand mean, for easier interpratability
# Variable Used for hypothesis 4
DataFile <- DataFile %>%
  mutate(
    Conscientiousness_CGM = Conscientiousness - mean(Conscientiousness, na.rm = TRUE)
  )


################################################################
### Hypothesis 2 regression analysis ###########################
################################################################
# Hypothesis 2: During-task cognitive fatigue relates positively to exerted
# cognitive effort.
# Outcome = Average Pupil Size (WS)
# Predictors = Cognitive Fatigue During Task (CWC)

model_hypothesis_2 <- lmer(
  AveragePupilSize ~ # Outcome
    CognitiveFatigueDuringTask_CWC + # Predictor
    (1 | ParticipantID), # Random intercept: accounts for the fact that each 
  # participant has repeated observations and may have a different typical
  # pupil size
  data = DataFile
)

summary(model_hypothesis_2)
confint(model_hypothesis_2, level = 0.95)

################################################################
### Hypothesis 2 regression analysis (With Controls) ###########
################################################################
# Outcome = Average Pupil Size (WS)
# Predictors = Cognitive Fatigue During Task (CWC)
# Control predictors = Task Type (Stroop, Maths, N-Back, Slider);
# Control predictors = Task Difficulty (Hard, Easy)

model_hypothesis_2_Controls <- lmer(
  AveragePupilSize ~ # Outcome
    CognitiveFatigueDuringTask_CWC + # Predictor
    TaskType +
    TaskDifficulty +
    (1 | ParticipantID),
  data = DataFile
)

summary(model_hypothesis_2_Controls)
confint(model_hypothesis_2_Controls, level = 0.95)

################################################################
### Hypothesis 4 regression analysis ###########################
################################################################
# Hypothesis 4: Conscientiousness moderates the relationship between during-task
# cognitive fatigue and overall exerted cognitive effort. When conscientiousness
# is high, the relationship is stronger.
# Outcome = Average Pupil Size (WS)
# Predictors = Cognitive Fatigue During Task (CWC);
# Predictors = Conscientiousness (CGM)
# Interaction = Cognitive Fatigue During Task (CWC) * Conscientiousness

model_hypothesis_4 <- lmer(
  AveragePupilSize ~ # Outcome
    CognitiveFatigueDuringTask_CWC * Conscientiousness_CGM + #Predictors
    (1 | ParticipantID),
  data = DataFile
)

summary(model_hypothesis_4)
confint(model_hypothesis_4, level = 0.95)
################################################################
### Hypothesis 4 regression analysis (With Controls) ###########
################################################################
# Outcome = Average Pupil Size (WS)
# Predictors = Cognitive Fatigue During Task (CWC);
# Predictors = Conscientiousness (CGM)
# Interaction = Cognitive Fatigue During Task (CWC) * Conscientiousness (CGM)
# Control predictors = Task Type (Stroop, Maths, N-Back, Slider);
# Control predictors = Task Difficulty (Hard, Easy)

model_hypothesis_4_Controls <- lmer(
  AveragePupilSize ~ # Outcome
    CognitiveFatigueDuringTask_CWC * Conscientiousness_CGM + #Predictors
    TaskType +
    TaskDifficulty +
    (1 | ParticipantID),
  data = DataFile
)

summary(model_hypothesis_4_Controls)
confint(model_hypothesis_4_Controls, level = 0.95)
################################################################
### Exploratory analyses #######################################
################################################################

# create between subject variable for fatigue
#Calculate each participants mean fatigue across all tasks

DataFile <- DataFile %>%
  group_by(ParticipantID) %>%
  mutate(
    MeanDuringFatigue = mean(CognitiveFatigueDuringTask, na.rm = TRUE)
  ) %>%
  ungroup()

#Center it

DataFile <- DataFile %>%
  mutate(
    MeanDuringFatigue_CGM = MeanDuringFatigue  - mean(MeanDuringFatigue , na.rm = TRUE)
  )



# Adds centered cognitive fatigue as a predictor, to account for between-subject
# variability

# Hypothesis 2
model_hypothesis_2_exploratory <- lmer(
  AveragePupilSize ~ # Outcome
    CognitiveFatigueDuringTask_CWC + MeanDuringFatigue_CGM + # Predictor
    (1 | ParticipantID), # Random intercept: accounts for the fact that each 
  # participant has repeated observations and may have a different typical
  # pupil size
  data = DataFile
)

summary(model_hypothesis_2_exploratory)

# Conscientiousness is now a significant predictor
# Hypothesis 4 without controls
model_hypothesis_4 <- lmer(
  AveragePupilSize ~ # Outcome
    CognitiveFatigueDuringTask_CWC * Conscientiousness_CGM + 
    MeanDuringFatigue_CGM + #Predictors
    (1 | ParticipantID),
  data = DataFile
)

summary(model_hypothesis_4)

# Hypothesis 4 with controls
model_hypothesis_4_Controls <- lmer(
  AveragePupilSize ~ # Outcome
    CognitiveFatigueDuringTask_CWC * Conscientiousness_CGM + 
    MeanDuringFatigue_CGM + #Predictors
    TaskType +
    TaskDifficulty +
    (1 | ParticipantID),
  data = DataFile
)

summary(model_hypothesis_4_Controls)

