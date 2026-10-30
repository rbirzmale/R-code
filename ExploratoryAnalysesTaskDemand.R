
##############################################################
##### EXPLORATORY ANALYSES WITH TASK DEMANDS AS OUTCOME ######
##############################################################

##################### SETUP ##################################
##############################################################

library(readxl)
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(ggeffects)
library(pbkrtest)
library(lm.beta)

# Set working directory & Open the data file
setwd("~/Desktop/RWorkingFolder")
DataFile <- read_excel("Dataset.xlsx")


######## Change how variables are treated (from character to numerical)  ###########
# lapply(DataFile, class) #Code to find out what class all variables are
DataFile$TaskDemand <- as.numeric(as.character(DataFile$TaskDemand))

#Ensures Task and Difficulty are factors
DataFile <- DataFile %>%
  mutate(
    TaskType=factor(TaskType),
    TaskDifficulty=factor(TaskDifficulty)
  )

#Transform baseline fatigue variable from categorical to nominal
### WARNING: assumes the gap by 1 is identical in all cases ###
DataFile <- DataFile %>%
  mutate(Baseline_Fatigue_num = case_when(
    Baseline_Fatigue == "Not at all"  ~ 1,
    Baseline_Fatigue == "Slightly"    ~ 2,
    Baseline_Fatigue == "Moderately"  ~ 3,
    Baseline_Fatigue == "Very"        ~ 4,
    Baseline_Fatigue == "Extremely"   ~ 5   
  ))

#Center baseline fatigue 
DataFile <- DataFile %>%
  mutate(
    Baseline_Fatigue_num_CGM = Baseline_Fatigue_num  - mean(Baseline_Fatigue_num , na.rm = TRUE)
  )

# Center Conscientiousness
DataFile <- DataFile %>%
  mutate(
    Conscientiousness_CGM = Conscientiousness - mean(Conscientiousness, na.rm = TRUE)
  )

################################################################
############### Variable modification ##########################
################################################################

############### Within subject fatigue ###################
# Create a CWC variable
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

############### Between subject fatigue ###################
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

################################################################ 
# Baseline fatigue analyses ####################################
################################################################ 

### New dataset with only relevant between-subject variables ### 
# Used only for baseline fatigue between subject analyses #

DataFileShort <- DataFile %>%
  group_by(ParticipantID) %>% # treats each participant as a separate group
  summarise( 
    TaskDemand = mean(TaskDemand, na.rm = TRUE), #Leaves only the average Pupil Size from all 8 session for 1 participant 
    Baseline_Fatigue_num = first(Baseline_Fatigue_num_CGM), #Takes the first value of baseline fatigue (numreic) from the dataset (Between-subject (BS) variable)
    Conscientiousness_CGM = first(Conscientiousness_CGM), #Takes the first value of conscientiousness from the dataset (BS variable)
    .groups = "drop" #drops the participantID grouping done earlier
  ) %>%
  arrange(as.numeric(gsub("P", "", ParticipantID))) #Sorts the dataset by participant number

################################################################ 
# exploratory 1 (Exploratory. - task demand) regression analysis 


model_exploratory_1 <- lm(
  TaskDemand ~ Baseline_Fatigue_num, 
  data = DataFileShort #Uses the dataset with the BS variables
)

summary(model_exploratory_1) 

################################################################ 
#  exploratory 3 (Exploratory- task demand) regression analysis 


model_exploratory_3 <- lm(
  TaskDemand ~ Baseline_Fatigue_num * Conscientiousness_CGM, #Outcome = Averaged Pupil Size across all 8 sessions; Predictors = Baseline_Fatigue & Conscientiousness. * Marks interaction effect to test moderation
  data = DataFileShort #Uses the dataset with the BS variables
)

summary(model_exploratory_3) #Provides output for exploratory 3


################################################################ 
# Cogntive fatigue during task fatigue analyses ################
################################################################ 

################################################################
# exploratory 2 (exploratory - task demand) regression analysis 



model_exploratory_2 <- lmer(
  TaskDemand ~ # Outcome
    CognitiveFatigueDuringTask_CWC + # Predictor
    (1 | ParticipantID), # Random intercept: accounts for the fact that each 
  # participant has repeated observations and may have a different typical
  # pupil size
  data = DataFile
)

summary(model_exploratory_2)
plot(
  ggpredict(
    model_exploratory_2,
    terms = c(
      "CognitiveFatigueDuringTask_CWC"
    )
  )
) + ggtitle("exploratory 2")

################################################################
# exploratory 2 regression analysis (With Controls) (Exploratory - task demands)

# Outcome = Task demand (WS)
# Predictors = Cognitive Fatigue During Task (CWC)
# Control predictors = Task Type (Stroop, Maths, N-Back, Slider);
# Control predictors = Task Difficulty (Hard, Easy)

model_exploratory_2_Controls <- lmer(
  TaskDemand ~ # Outcome
    CognitiveFatigueDuringTask_CWC + # Predictor
    TaskType +
    TaskDifficulty +
    (1 | ParticipantID),
  data = DataFile
)

summary(model_exploratory_2_Controls)

# Create visual plots
plot(
  ggpredict(
    model_exploratory_2_Controls,
    terms = c(
      "CognitiveFatigueDuringTask_CWC",
      "TaskDifficulty [Easy, Hard]"
    )
  )
) + ggtitle("exploratory 2, control")

plot(
  ggpredict(
    model_exploratory_2_Controls,
    terms = c(
      "CognitiveFatigueDuringTask_CWC",
      "TaskType [Stroop, Slider, Mental Arithmetic, N-back]"
    )
  )
) + ggtitle("exploratory 2, control")


# Checks it with tasktype*Difficulty interaction
model_exploratory_2_ControlsInteraction <- lmer(
  TaskDemand ~ # Outcome
    CognitiveFatigueDuringTask_CWC + # Predictor
    TaskType*
    TaskDifficulty +
    (1 | ParticipantID),
  data = DataFile
)

summary(model_exploratory_2_ControlsInteraction)

# Visual plot
plot(
  ggpredict(
    model_exploratory_2_ControlsInteraction,
    terms = c(
      "CognitiveFatigueDuringTask_CWC",
      "TaskType [Stroop, Slider, Mental Arithmetic, N-back]",
      "TaskDifficulty [Easy, Hard]"
    )
  )
) + ggtitle("exploratory 2, control, interaction")

################################################################
##exploratory 4 regression analysis (Exploratory - task demands)

# Outcome = Task demand (WS)
# Predictors = Cognitive Fatigue During Task (CWC);
# Predictors = Conscientiousness (CGM)
# Interaction = Cognitive Fatigue During Task (CWC) * Conscientiousness

model_exploratory_4 <- lmer(
  TaskDemand ~ # Outcome
    CognitiveFatigueDuringTask_CWC * Conscientiousness_CGM + #Predictors
    (1 | ParticipantID),
  data = DataFile
)

summary(model_exploratory_4)

################################################################
# exploratory 4 regression analysis (With Controls) (Exploratory - task demands)

# Outcome = Task demand (WS)
# Predictors = Cognitive Fatigue During Task (CWC);
# Predictors = Conscientiousness (CGM)
# Interaction = Cognitive Fatigue During Task (CWC) * Conscientiousness (CGM)
# Control predictors = Task Type (Stroop, Maths, N-Back, Slider);
# Control predictors = Task Difficulty (Hard, Easy)

model_exploratory_4_Controls <- lmer(
  TaskDemand ~ # Outcome
    CognitiveFatigueDuringTask_CWC * Conscientiousness_CGM + #Predictors
    TaskType +
    TaskDifficulty +
    (1 | ParticipantID),
  data = DataFile
)

summary(model_exploratory_4_Controls)

### Simple slopes ###########

CON_mean <- mean(DataFile$Conscientiousness_CGM, na.rm = TRUE)
CON_SD <- sd(DataFile$Conscientiousness_CGM, na.rm = TRUE)


### exploratory 4 ###
simple_slopes4 <- emtrends(
  model_exploratory_4,
  ~ Conscientiousness_CGM,
  var = "CognitiveFatigueDuringTask_CWC",
  at = list(
    Conscientiousness_CGM = c(-CON_SD, 0, CON_SD)
  )
)

summary(simple_slopes4, infer = c(TRUE, TRUE))

plot(
  ggpredict(
    model_exploratory_4,
    terms = c(
      "CognitiveFatigueDuringTask_CWC",
      "Conscientiousness_CGM [-1,0,1]"
    )
  )
) + ggtitle("exploratory 4")

### exploratory 4 Controls ###
simple_slopes4E <- emtrends(
  model_exploratory_4_Controls,
  ~ Conscientiousness_CGM,
  var = "CognitiveFatigueDuringTask_CWC",
  at = list(
    Conscientiousness_CGM = c(-CON_SD, 0, CON_SD)
  )
)

summary(simple_slopes4E, infer = c(TRUE, TRUE))

plot(
  ggpredict(
    model_exploratory_4_Controls,
    terms = c(
      "CognitiveFatigueDuringTask_CWC",
      "Conscientiousness_CGM [-1,0,1]"
    )
  )
) + ggtitle("exploratory 4, control")



################################################################
### Adding CGM/Between-subject variability to the models #######

#### No Conscientiousness ######################################################

# exploratory 2
model_exploratory_2_CGM <- lmer(
  TaskDemand ~ # Outcome
    CognitiveFatigueDuringTask_CWC + MeanDuringFatigue_CGM + # Predictor
    (1 | ParticipantID), # Random intercept: accounts for the fact that each 
  # participant has repeated observations and may have a different typical
  # pupil size
  data = DataFile
)

summary(model_exploratory_2_CGM)


# exploratory 2 with controls
model_exploratory_2_CGM_controls <- lmer(
  TaskDemand ~ # Outcome
    CognitiveFatigueDuringTask_CWC + MeanDuringFatigue_CGM +
    + TaskType
    + TaskDifficulty +
    (1 | ParticipantID), # Random intercept: accounts for the fact that each 
  # participant has repeated observations and may have a different typical
  # pupil size
  data = DataFile
)

summary(model_exploratory_2_CGM_controls)
confint(model_exploratory_2_CGM_controls, level = 0.95)
#### With Conscientiousness ####################################################

# exploratory 4 without controls
model_CGM_4 <- lmer(
  TaskDemand ~ # Outcome
    CognitiveFatigueDuringTask_CWC * Conscientiousness_CGM + 
    MeanDuringFatigue_CGM + #Predictors
    (1 | ParticipantID),
  data = DataFile
)

summary(model_CGM_4)

# exploratory 4 with controls
model_CGM_4_Controls <- lmer(
  TaskDemand ~ # Outcome
    CognitiveFatigueDuringTask_CWC * Conscientiousness_CGM + 
    MeanDuringFatigue_CGM + #Predictors
    TaskType +
    TaskDifficulty +
    (1 | ParticipantID),
  data = DataFile
)

summary(model_CGM_4_Controls)
confint(model_CGM_4_Controls, level = 0.95)

# Simple slopes
simple_slopes_CGM4_Controls <- emtrends(
  model_CGM_4_Controls,
  ~ Conscientiousness_CGM,
  var = "CognitiveFatigueDuringTask_CWC",
  at = list(
    Conscientiousness_CGM = c(-CON_SD, 0, CON_SD)
  )
)

summary(simple_slopes_CGM4_Controls, infer = c(TRUE, TRUE), adjust = "holm")

# Visualisation for simple slopes
library(interactions)
interact_plot(model_CGM_4_Controls, pred = CognitiveFatigueDuringTask_CWC, modx = Conscientiousness_CGM) +
  labs(x = "Cognitive Fatigue During Task (WS)", y = "Task Demands")