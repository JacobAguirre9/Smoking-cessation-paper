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

PoissonModel <- glm(Addiction ~ Sex + EducAttainment + Last12_AttemptQuit + Length_Prescriptions + InsuranceStatus + Race_levels + NoQuitAttempt)

########

########
# Begin analysing coefficients and rewards

plot_summs(PoissonModel, plot.distributions = TRUE, rescale.distributions = TRUE)

########