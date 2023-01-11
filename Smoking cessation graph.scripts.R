library(haven)
All_Waves_CessationPaper_1_ <- read_dta("~/Downloads/All_Waves_CessationPaper (1).dta")
View(All_Waves_CessationPaper_1_)
data <- All_Waves_CessationPaper_1_
rm(All_Waves_CessationPaper_1_)
Plot1data <- data[data$Addiction <=60, data$wave]
library(dplyr)
ggplot(data=Plot1data, aes(x=Addiction))+geom_histogram(binwidth = 3)
