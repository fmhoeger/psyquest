library(psychTestR)
library(psyquest)

battery(languages = "DE", questionnaires = c(psyquest::DAC(randomise = FALSE), psyquest::PAC()))
