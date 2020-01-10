psyquest_dict_raw <-
  map_dfr(list.files("./data/dicts/", full.names = T),
          function(x) {
            readRDS(x) %>% select(-key_type, -item_number) %>%
              filter(nchar(DE) != 0, nchar(EN) != 0)
          })

psyquest_dict_raw_ext <-
  tibble(
    key = c(
      "ENTER_ID",
      "CONTINUE",
      "RESULTS_SAVED",
      "CLOSE_BROWSER",
      "PAGE_HEADER"
    ),
    EN = c(
      "Please enter your ID",
      "Continue",
      "Your results have been saved.",
      "<br>You can close the browser window now.",
      "Question {{num_question}} out of {{test_length}}"
    ),
    DE = c(
      "Bitte gebe Deine ID ein",
      "Weiter",
      "Deine Ergebnisse wurden gespeichert.",
      "<br>Du kannst das Browserfenster jetzt schließen.",
      "Frage {{num_question}} von {{test_length}}"
    )
  )

psyquest_dict_raw <-
  bind_rows(psyquest_dict_raw, psyquest_dict_raw_ext)
psyquest_dict <- psychTestR::i18n_dict$new(psyquest_dict_raw)

usethis::use_data(psyquest_dict, overwrite = TRUE)
