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

PoissonModel <- glm(Addiction ~ Sex + EducAttainment + Last12_AttemptQuit + Length_Prescriptions + InsuranceStatus + Race_levels + NoQuitAttempt, data, family = poisson(link = "log"))

PoissonModel2 <- glm(Addiction ~ Last12_AttemptQuit + Past12_FrequencyQuit + UsedECigs_helpquit + QuitAttempt_Therapy + UsedPrescriptions_Quit + EverUseAlcohol, data, family = poisson(link = "log"))

########

########
# Begin analysing coefficients and rewards

plot_summs(PoissonModel, plot.distributions = TRUE, rescale.distributions = TRUE)

plot_summs(PoissonModel2, plot.distributions = TRUE, rescale.distributions = TRUE)

# plot regression coefficients for PoissonModel and PoissonModel2
plot_summs(PoissonModel, PoissonModel2, scale = TRUE, exp = TRUE)
interact_plot(PoissonModel2, pred = Addiction)

########