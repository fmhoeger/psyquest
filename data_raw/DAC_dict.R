DAC_dict_raw <- readRDS("data_raw/DAC_dict.RDS") %>%
  select(-key_type, -item_number)

DAC_dict_raw_ext <- tibble(key = c("ENTER_ID", "CONTINUE", "RESULTS_SAVED", "CLOSE_BROWSER", "PAGE_HEADER"),
                           EN = c("Please enter your ID", "Continue", "Your results have been saved.", "<br>You can close the browser window now.", "Question {{num_question}} out of {{test_length}}"),
                           DE = c("Bitte gebe Deine ID ein", "Weiter", "Deine Ergebnisse wurden gespeichert.", "<br>Du kannst das Browserfenster jetzt schließen.", "Frage {{num_question}} von {{test_length}}"))
DAC_dict_raw <- bind_rows(DAC_dict_raw, DAC_dict_raw_ext)
DAC_dict <- psychTestR::i18n_dict$new(DAC_dict_raw)

usethis::use_data(DAC_dict, overwrite = TRUE)
