library(psychTestR)
library(psyquest)

battery(languages = "de", questionnaires = c(psyquest::DAC(), psyquest::TOM(subscales=c("Stable", "Incremental"))))
