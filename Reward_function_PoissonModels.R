# Set Working directory
> setwd("~/Documents/Research with Garcia/Smoking Cessation Paper")

########
# Load packages and library
library(ggplot2)
library(labeling)
library(np)
library(splines)
library(tibble)
install.packages("jtools")
library(jtools)
install.packages("broom")
install.packages("broom.mixed")
library(broom)
library(broom.mixed)
library(haven)
########



data <- read_dta("All_Waves_CessationPaper (1).dta")
columns <- names(data)

# Individuals in all 5 waves
all_waves_data <- data[data$wave_all == 31,]

#clean vars
all_waves_data$QuitAttempt_NasalSpray[all_waves_data$QuitAttempt_NasalSpray < 0] <- NA
all_waves_data$Last12_AttemptQuit[all_waves_data$Last12_AttemptQuit < 0] <- NA


# Initialize empty list for models

models <- list()

# Group the data by the id column
individual_data <- group_by(all_waves_data, PERSONID)

# Count the number of rows for each individual
row_counts <- summarize(individual_data, n = n())

# Check if any individual has more than 5 rows
multiple_rows <- row_counts[row_counts$n > 5,]

# Print the individuals with more than 5 rows
print(multiple_rows)

# Begin modeling

# Install and load the glm2 package if it's not already installed
install.packages("glm2")
library(glm2)


# Iterate through the waves
for (i in 1:5) {
  
  # Subset the data to include only rows for wave i
  wave_i_data <- all_waves_data[all_waves_data$wave == i,]
  
  # Fit the Poisson GLM model
  model <- glm(Addiction ~ Sex + EducAttainment + InsuranceStatus + Race_levels + IntentToQuit_nic + Last12_AttemptQuit + QuitAttempt_NicPill + QuitAttempt_NicGum + QuitAttempt_NicPatch + QuitAttempt_NasalSpray + QuitAttempt_NicInhaler + UsedECigs_helpquit + QuitAttempt_Therapy + Past12_FrequencyQuit, data = wave_i_data, family = poisson(link = "log"))
  
  # Store the model in the models list
  models[[i]] <- model
}

# Print the models
summary.glm(models[[2]])

# Initialize empty lists to store the variables
ct <- list()
dt <- list()
at <- list()
qt <- list()

# Iterate through the waves for what the variables should be. But, we can't regress a list (duh)
# so just add these manually to the regression above
for (i in 1:5) {
  # Subset the data to include only rows for wave i
  wave_i_data <- all_waves_data[all_waves_data$wave == i,]
  
  # Create the variable ct from the current smoking status variable
  ct[[i]] <- wave_i_data[,c("Ciguser", "hsi", "Cigs_smoked_daily_1yearago")]
  
  # Create the variable dt from the demographic variables
  dt[[i]] <- wave_i_data[, c("Sex", "EducAttainment", "InsuranceStatus", "Race_levels", "SexualOrientation")]
  
  # Create the variable at from the chosen action variable
  at[[i]] <- wave_i_data[, c("UsedPrescriptions_Quit", "UsedECigs_helpquit", "NoQuitAttempt", "QuitAttempt_NicInhaler", "UsedNicotinePatch", "QuitAttempt_NicPatch", "QuitAttempt_NasalSpray", "QuitAttempt_Therapy")]
  
  # Create the variable qt from the smoking intentions variable
  qt[[i]] <- wave_i_data[,c("IntentToQuit_nic", "Last12_AttemptQuit", "Last12_SlowAttempt", "Past12_FrequencyQuit")]
}

