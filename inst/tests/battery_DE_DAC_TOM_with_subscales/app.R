library(psychTestR)
library(psyquest)

battery(languages = "DE", questionnaires = c(psyquest::DAC(), psyquest::TOM(subscales=c("Stable", "Incremental"))))
