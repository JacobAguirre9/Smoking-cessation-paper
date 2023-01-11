library(haven)
data <- read_dta("~/Downloads/All_Waves_CessationPaper (1).dta")
columns <- names(data)

wave1_only <- data[data$wave1 == 1 & data$wave2 == 0 & data$wave3 == 0 & data$wave4 == 0 & data$wave5 == 0,]
wave2_only <- data[data$wave1 == 0 & data$wave2 == 1 & data$wave3 == 0 & data$wave4 == 0 & data$wave5 == 0,]
wave3_only <- data[data$wave1 == 0 & data$wave2 == 0 & data$wave3 == 1 & data$wave4 == 0 & data$wave5 == 0,]
wave4_only <- data[data$wave1 == 0 & data$wave2 == 0 & data$wave3 == 0 & data$wave4 == 1 & data$wave5 == 0,]
wave5_only <- data[data$wave1 == 0 & data$wave2 == 0 & data$wave3 == 0 & data$wave4 == 0 & data$wave5 == 1,]




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
########


########
# Begin Writing Models for all data

PoissonModel <- glm(Addiction ~ Sex + EducAttainment + Last12_AttemptQuit + Length_Prescriptions + InsuranceStatus + Race_levels + NoQuitAttempt, data, family = poisson(link = "log"))

PoissonModel2 <- glm(Addiction ~ Last12_AttemptQuit + hsi + Past12_FrequencyQuit + UsedECigs_helpquit + QuitAttempt_Therapy + UsedPrescriptions_Quit + EverUseAlcohol, data, family = poisson(link = "log"))

PoissonModel3 <- glm(Addiction ~ hsi + SexualOrientation + Race_levels + Sex + EducAttainment + Last12_AttemptQuit + Past12_FrequencyQuit + UsedCigs_helpquit + QuitAttempt_Therapy + UsedPrescriptions_Quit + EverUseAlcohol, data, family = poisson(link="log"))

########
# Begin writing models, seperated by timewave. We will then plot the coefficients.
# Wave 1 Models
Wave1_PoissonModel <- glm(Addiction ~ Sex + EducAttainment + Last12_AttemptQuit + Length_Prescriptions + InsuranceStatus + Race_levels + NoQuitAttempt, wave1_only, family = poisson(link = "log"))
Wave1_PoissonModel2 <- glm(Addiction ~ Last12_AttemptQuit + hsi + Past12_FrequencyQuit + UsedECigs_helpquit + QuitAttempt_Therapy + UsedPrescriptions_Quit + EverUseAlcohol, wave1_only, family = poisson(link = "log"))
Wave1_PoissonModel3 <- glm(Addiction ~ hsi + SexualOrientation + Race_levels + Sex + EducAttainment + Last12_AttemptQuit + Past12_FrequencyQuit + UsedCigs_helpquit + QuitAttempt_Therapy + UsedPrescriptions_Quit + EverUseAlcohol, wave1_only, family = poisson(link="log"))

wave1model1_coef <- coef(Wave1_PoissonModel)
wave1model2_coef <- coef(Wave1_PoissonModel2)
wave1model3_coef <- coef(Wave1_PoissonModel3)

# Wave 2 Models
Wave2_PoissonModel <- glm(Addiction ~ Sex + EducAttainment + Last12_AttemptQuit + Length_Prescriptions + InsuranceStatus + Race_levels + NoQuitAttempt, wave2_only, family = poisson(link = "log"))
Wave2_PoissonModel2 <- glm(Addiction ~ Last12_AttemptQuit + hsi + Past12_FrequencyQuit + UsedECigs_helpquit + QuitAttempt_Therapy + UsedPrescriptions_Quit + EverUseAlcohol, wave2_only, family = poisson(link = "log"))
Wave2_PoissonModel3 <- glm(Addiction ~ hsi + SexualOrientation + Race_levels + Sex + EducAttainment + Last12_AttemptQuit + Past12_FrequencyQuit + UsedCigs_helpquit + QuitAttempt_Therapy + UsedPrescriptions_Quit + EverUseAlcohol, wave2_only, family = poisson(link="log"))

wave2model1_coef <- coef(Wave2_PoissonModel)
wave2model2_coef <- coef(Wave2_PoissonModel2)
wave2model3_coef <- coef(Wave2_PoissonModel3)

# Wave 3 Models 

Wave3_PoissonModel <- glm(Addiction ~ Sex + EducAttainment + Last12_AttemptQuit + Length_Prescriptions + InsuranceStatus + Race_levels + NoQuitAttempt, wave3_only, family = poisson(link = "log"))
# Above model seems to not converge >_> 
Wave3_PoissonModel2 <- glm(Addiction ~ Last12_AttemptQuit + hsi + Past12_FrequencyQuit + UsedECigs_helpquit + QuitAttempt_Therapy + UsedPrescriptions_Quit + EverUseAlcohol, wave3_only, family = poisson(link = "log"))
Wave3_PoissonModel3 <- glm(Addiction ~ hsi + SexualOrientation + Race_levels + Sex + EducAttainment + Last12_AttemptQuit + Past12_FrequencyQuit + UsedCigs_helpquit + QuitAttempt_Therapy + UsedPrescriptions_Quit + EverUseAlcohol, wave3_only, family = poisson(link="log"))

wave3model1_coef <- coef(Wave3_PoissonModel)
wave3model2_coef <- coef(Wave3_PoissonModel2)
wave3model3_coef <- coef(Wave3_PoissonModel3)

# Wave 4 Models

Wave4_PoissonModel <- glm(Addiction ~ Sex + EducAttainment + Last12_AttemptQuit + Length_Prescriptions + InsuranceStatus + Race_levels + NoQuitAttempt, wave4_only, family = poisson(link = "log"))
# Same issue as above, this model fails to converge. Data issue (?)
Wave4_PoissonModel2 <- glm(Addiction ~ Last12_AttemptQuit + hsi + Past12_FrequencyQuit + UsedECigs_helpquit + QuitAttempt_Therapy + UsedPrescriptions_Quit + EverUseAlcohol, wave4_only, family = poisson(link = "log"))
Wave4_PoissonModel3 <- glm(Addiction ~ hsi + SexualOrientation + Race_levels + Sex + EducAttainment + Last12_AttemptQuit + Past12_FrequencyQuit + UsedCigs_helpquit + QuitAttempt_Therapy + UsedPrescriptions_Quit + EverUseAlcohol, wave4_only, family = poisson(link="log"))

wave4model1_coef <- coef(Wave4_PoissonModel)
wave4model2_coef <- coef(Wave4_PoissonModel2)
wave4model3_coef <- coef(Wave4_PoissonModel3)

# Wave 5 Models

Wave5_PoissonModel <- glm(Addiction ~ Sex + EducAttainment + Last12_AttemptQuit + Length_Prescriptions + InsuranceStatus + Race_levels + NoQuitAttempt, wave5_only, family = poisson(link = "log"))
# Definitely data issue >_< look into this ! 
Wave5_PoissonModel2 <- glm(Addiction ~ Last12_AttemptQuit + hsi + Past12_FrequencyQuit + UsedECigs_helpquit + QuitAttempt_Therapy + UsedPrescriptions_Quit + EverUseAlcohol, wave5_only, family = poisson(link = "log"))
Wave5_PoissonModel3 <- glm(Addiction ~ hsi + SexualOrientation + Race_levels + Sex + EducAttainment + Last12_AttemptQuit + Past12_FrequencyQuit + UsedCigs_helpquit + QuitAttempt_Therapy + UsedPrescriptions_Quit + EverUseAlcohol, wave5_only, family = poisson(link="log"))

wave5model1_coef <- coef(Wave5_PoissonModel)
wave5model2_coef <- coef(Wave5_PoissonModel2)
wave5model3_coef <- coef(Wave5_PoissonModel3)
#######






########
# Begin analysing coefficients and rewards

plot_summs(PoissonModel, plot.distributions = TRUE, rescale.distributions = TRUE)

plot_summs(PoissonModel2, plot.distributions = TRUE, rescale.distributions = TRUE)

plot_summs(PoissonModel3, plot.distributions = TRUE, rescale.distributions = TRUE)

# plot regression coefficients for PoissonModel and PoissonModel2 and PoissonModel3
plot_summs(PoissonModel, PoissonModel2, PoissonModel3, scale = TRUE, exp = TRUE)


