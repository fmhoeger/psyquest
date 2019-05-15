DAC_dict_raw <- readRDS("data_raw/DAC_dict.RDS")
DAC_dict_raw_ext <- tibble(key = c("ENTER_ID", "CONTINUE", "RESULTS_SAVED", "CLOSE_BROWSER", "PAGE_HEADER"),
                           EN = c("Please enter your ID", "Continue", "Your results have been saved.", "You can close the browser window now.", "Question {{num_question}} out of {{test_length}}"),
                           DE = c("Bitte gebe deine ID ein", "Weiter", "Deine Ergebnisse wurden gespeichert", "Du kannst das Browserfenster jetzt schliessen", "Frage {{num_question}} von {{test_length}}"))
DAC_dict_raw <- bind_rows(DAC_dict_raw, DAC_dict_raw_ext)
DAC_dict <- psychTestR::i18n_dict$new(DAC_dict_raw)

usethis::use_data(DAC_dict, overwrite = TRUE)
