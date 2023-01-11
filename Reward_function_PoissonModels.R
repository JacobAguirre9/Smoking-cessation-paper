library(haven)
data <- read_dta("~/Downloads/All_Waves_CessationPaper (1).dta")
columns <- names(data)

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
# Begin Writing Models

PoissonModel <- glm(data$Addiction ~ data$Sex + data$EducAttainment + data$Last12_AttemptQuit + data$Length_Prescriptions + data$InsuranceStatus + data$Race_levels + data$NoQuitAttempt)

PoissonModel2 <- glm(data$Addiction ~ data$Last12_AttemptQuit + data$Past12_FrequencyQuit + data$UsedECigs_helpquit + data$QuitAttempt_Therapy + data$UsedPrescriptions_Quit + data$EverUseAlcohol)

########

########
# Begin analysing coefficients and rewards

plot_summs(PoissonModel, plot.distributions = TRUE, rescale.distributions = TRUE)

plot_summs(PoissonModel2, plot.distributions = TRUE, rescale.distributions = TRUE)

########