#' DEG
#'
#' This function defines a DEG module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the DEG in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the DEG, consider using \code{\link{DEG_standalone}()}.
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#' This is also the label given to the results in the output file.
#' @param dict The psyquest dictionary used for internationalisation.
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' When no subscales are provided all subscales are selected.
#' @param language Language the questionnaire is rendered in.
#' @param ... Further arguments to be passed to \code{\link{DEG}()}.
#' @export
DEG <- function(label = "DEG",
                dict = psyquest::psyquest_dict,
                subscales = c(),
                language = "en",
                ...) {
  stopifnot(purrr::is_scalar_character(label))

  main_test_deg(
    label = label,
    items = get_items(label, subscales),
    subscales = subscales,
    language = language,
    offset = 1,
    arrange_vertically = TRUE
  )
}

main_test_deg <- function(label, items, subscales, language, offset = 1, arrange_vertically = TRUE) {
  prompt_id <- NULL
  prompt_ids <- items %>% pull(prompt_id)
  elts <- c()

  if ("TDEG_0001" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
        NAFC_page("q1",
                psychTestR::i18n("TDEG_0001_PROMPT"),
                sprintf("btn%d_text", 1:2),
                labels = purrr::map(sprintf("TDEG_0001_CHOICE%d", 1:2), psychTestR::i18n),
                arrange_vertically = FALSE,
                style = "min-width: 60px"
        )
      ),
      dict = psyquest::psyquest_dict
    ))
  }

  if ("TDEG_0002" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
        NAFC_page("q2",
                  psychTestR::i18n("TDEG_0002_PROMPT"),
                  sprintf("btn%d_text", 1:2),
                  labels = purrr::map(sprintf("TDEG_0002_CHOICE%d", 1:2), psychTestR::i18n),
                  arrange_vertically = FALSE,
                  style = "min-width: 60px",
                  on_complete = function(answer, state, ...) {
                    set_local("hearing_problems", answer, state)
                  }
        )
      ),
      dict = psyquest::psyquest_dict
    ))
  }

  if ("TDEG_0003" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      conditional(function(state, ...) get_local("hearing_problems", state) == "btn1_text",
        text_input_page("q3",
                        psychTestR::i18n("TDEG_0003_PROMPT"),
                        button_text = psychTestR::i18n("CONTINUE"))
        )),
      dict = psyquest::psyquest_dict
    ))
  }

  if ("TDEG_0004" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      NAFC_page("q4",
                psychTestR::i18n("TDEG_0004_PROMPT"),
                sprintf("btn%d_text", 1:4),
                labels = purrr::map(sprintf("TDEG_0004_CHOICE%d", 1:4), psychTestR::i18n),
                style = "min-width: 188px"
                )
      ),
      dict = psyquest::psyquest_dict
    ))
  }

  if ("TDEG_0006" %in% prompt_ids) {
    nationalities <- c("UK", "USA", "BULGARIA", "CHINA", "CUBA", "DOMINICAN_REPUBLIC", "EL_SALVADOR", "FRANCE", "GERMANY", "GUATEMALA", "INDIA", "IRELAND", "ITALY", "LITHUANIA", "MEXICO", "NETHERLANDS", "NIGERIA", "PAKISTAN", "PHILIPPINES", "POLAND", "PORTUGAL", "ROMANIA", "RUSSIAN_FEDERATION", "SOUTH_AFRICA", "SOUTH_KOREA", "SPAIN", "VIETNAM", "OTHER_NATIONALITY")
    nationality_acronyms <- c("UK", "US", "BG", "CN", "CU", "DO", "SV", "FR", "DE", "GT", "IN", "IE", "IT", "LT", "MX", "NL", "NG", "PK", "PH", "PL", "PT", "RO", "RU", "SA", "KR", "ES", "VN", "OTHER")
    if (language[1] == "de") {
      nationalities <- c("GERMAN", "AFGHAN", "ALGERIAN", "AMERICAN", "BRITISH", "BULGARIAN", "CHINESE", "FRENCH", "GREEK", "IRAQI", "IRANIAN", "ITALIAN", "CANADIAN", "KOSOVAN", "CROATIAN", "POLISH", "PORTUGUESE", "ROMANIAN", "RUSSIAN", "SENEGALESE", "SERBIAN", "SPANISH", "SYRIAN", "TURKISH", "BELORUSSIAN", "OTHER_NATIONALITY")
      nationality_acronyms <- c("DE", "AF", "DZ", "USA", "GB", "BG", "ZH", "FR", "GR", "IQ", "IR", "IT", "CA", "XK", "HR", "PL", "PT", "RO", "RU", "SN", "RS", "ES", "SY", "TR", "BY", "OTHER")
    }
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      dropdown_page("q5",
                psychTestR::i18n("TDEG_0006_PROMPT"),
                setNames(nationality_acronyms, purrr::map(nationalities, psychTestR::i18n)),
                next_button_text = psychTestR::i18n("CONTINUE"))
      ),
      dict = psyquest::psyquest_dict
    ))
  }

  if ("TDEG_0007" %in% prompt_ids) {
    countries <- c("UK", "USA", "BULGARIA", "CHINA", "CUBA", "DOMINICAN_REPUBLIC", "EL_SALVADOR", "FRANCE", "GERMANY", "GUATEMALA", "INDIA", "IRELAND", "ITALY", "LITHUANIA", "MEXICO", "NETHERLANDS", "NIGERIA", "PAKISTAN", "PHILIPPINES", "POLAND", "PORTUGAL", "ROMANIA", "RUSSIAN_FEDERATION", "SOUTH_AFRICA", "SOUTH_KOREA", "SPAIN", "VIETNAM", "OTHER_COUNTRY")
    country_codes <- c("UK", "US", "BG", "CN", "CU", "DO", "SV", "FR", "DE", "GT", "IN", "IE", "IT", "LT", "MX", "NL", "NG", "PK", "PH", "PL", "PT", "RO", "RU", "SA", "KR", "ES", "VN", "OTHER")
    if (language[1] == "de") {
      countries <- c("GERMANY", "AFGHANISTAN", "ALGERIA", "BULGARIA", "CHINA", "FRANCE", "GREECE", "UK", "IRAQ", "IRAN", "ITALY", "CANADA", "KOSOVO", "CROATIA", "POLAND", "PORTUGAL", "ROMANIA", "RUSSIAN_FEDERATION", "SENEGAL", "SERBIA", "SPAIN", "SYRIA", "TURKEY", "USA", "BELARUS", "OTHER_COUNTRY")
      country_codes <- c("DE", "AF", "DZ", "BG", "ZH", "FR", "GR", "GB", "IQ", "IR", "IT", "CA", "XK", "HR", "PL", "PT", "RO", "RU", "SN", "RS", "ES", "SY", "TR", "USA", "BY", "OTHER")
    }
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      dropdown_page("q6",
                psychTestR::i18n("TDEG_0007_PROMPT"),
                setNames(country_codes, purrr::map(countries, psychTestR::i18n)),
                next_button_text = psychTestR::i18n("CONTINUE"))
      ),
      dict = psyquest::psyquest_dict
    ))
  }

  if ("TDEG_0008" %in% prompt_ids) {
    languages <- c("ENGLISH", "ARABIC", "BULGARIAN", "CHINESE", "DUTCH", "FRENCH", "GERMAN", "HAITIAN", "HINDI", "ITALIAN", "KOREAN", "LITHUANIAN", "POLISH", "PORTUGUESE", "PUNJABI", "ROMANIAN", "RUSSIAN", "SPANISH", "TAGALOG", "VIETNAMESE", "OTHER_LANGUAGE")
    language_codes <- c("en", "ar", "bg", "zh", "nl", "fr", "de", "ht", "hi", "it", " ko", "lt", "pl", "pt", "pa", "ro", "ru", "es", "tl", "vi", "other")
    if (language[1] == "de") {
      languages <- c("GERMAN", "ALBANIAN", "ARABIC", "BULGARIAN", "CHINESE", "ENGLISH", "GREEK", "ITALIAN", "JAPANESE", "KURDISH", "PASHTO", "PERSIAN", "POLISH", "ROMANIAN", "RUSSIAN", "SERBIAN", "SPANISH", "TURKISH", "HUNGARIAN", "OTHER_LANGUAGE")
      language_codes <- c("de", "sq", "ar", "bg", "zh", "en", "el", "it", "ja", "ku", "ps", "fa", "pl", "ro", "ru", "sr", "es", "tr", "hu", "other")
    }
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      dropdown_page("q7",
                psychTestR::i18n("TDEG_0008_PROMPT"),
                setNames(language_codes, purrr::map(languages, psychTestR::i18n)),
                next_button_text = psychTestR::i18n("CONTINUE"))
      ),
      dict = psyquest::psyquest_dict
    ))
  }

  if ("TDEG_0009" %in% prompt_ids) {
    languages <- c("NONE", "ENGLISH", "ARABIC", "BULGARIAN", "CHINESE", "DUTCH", "FRENCH", "GERMAN", "HAITIAN", "HINDI", "ITALIAN", "KOREAN", "LITHUANIAN", "POLISH", "PORTUGUESE", "PUNJABI", "ROMANIAN", "RUSSIAN", "SPANISH", "TAGALOG", "VIETNAMESE", "OTHER_LANGUAGE")
    language_codes <- c("none", "en", "ar", "bg", "zh", "nl", "fr", "de", "ht", "hi", "it", " ko", "lt", "pl", "pt", "pa", "ro", "ru", "es", "tl", "vi", "other")
    if (language[1] == "de") {
      languages <- c("NONE", "GERMAN", "ALBANIAN", "ARABIC", "BULGARIAN", "CHINESE", "ENGLISH", "GREEK", "ITALIAN", "JAPANESE", "KURDISH", "PASHTO", "PERSIAN", "POLISH", "ROMANIAN", "RUSSIAN", "SERBIAN", "SPANISH", "TURKISH", "HUNGARIAN", "OTHER_LANGUAGE")
      language_codes <- c("none", "de", "sq", "ar", "bg", "zh", "en", "el", "it", "ja", "ku", "ps", "fa", "pl", "ro", "ru", "sr", "es", "tr", "hu", "other")
    }
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      dropdown_page("q8",
                psychTestR::i18n("TDEG_0009_PROMPT"),
                setNames(language_codes, purrr::map(languages, psychTestR::i18n)),
                next_button_text = psychTestR::i18n("CONTINUE"))
      ),
      dict = psyquest::psyquest_dict
    ))
  }

  if ("TDEG_0010" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      month_and_year_select_page("q9",
                psychTestR::i18n("TDEG_0010_PROMPT"))
      ),
      dict = psyquest::psyquest_dict
    ))
  }

  if ("TDEG_0011" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      NAFC_page("q10",
                psychTestR::i18n("TDEG_0011_PROMPT"),
                sprintf("btn%d_text", 1:3),
                labels = purrr::map(sprintf("TDEG_0011_CHOICE%d", 1:3), psychTestR::i18n),
                style = "min-width: 300px;"
                )
      ),
      dict = psyquest::psyquest_dict
    ))
  }

  if ("TDEG_0012" %in% prompt_ids) {
    elts <- psychTestR::join(elts, psychTestR::new_timeline(c(
      NAFC_page("q11",
                psychTestR::i18n("TDEG_0012_PROMPT"),
                sprintf("btn%d_text", 1:3),
                labels = purrr::map(sprintf("TDEG_0012_CHOICE%d", 1:3), psychTestR::i18n),
                style = "min-width: 162px;"
                )
      ),
      dict = psyquest::psyquest_dict
    ))
  }

  psychTestR::join(psychTestR::begin_module(label),
                   elts,
                   scoring(label, items, subscales),
                   psychTestR::end_module())
}

postprocess_deg <- function(subscale, results, scores) {
  if (subscale == "Type of Hearing Impairment") {
    if (results[["DEG"]][["q2"]] == "btn1_text") {
      results[["DEG"]][["q3"]]
    } else {
      ""
    }
  } else if (subscale == "Age") {
    min_year <- 2005
    month <- as.integer(results[["DEG"]][["q9"]][1]) - 1
    year <- as.numeric(results[["DEG"]][["q9"]][2]) - min_year
    cur_date <- Sys.Date()
    cur_year <- get_year(cur_date) - min_year
    cur_month <- get_month(cur_date) - 1
    (cur_year - year) * 12 + cur_month - month
  } else if (subscale == "Gender") {
    as.numeric(gsub("[^0-9]", "", results[["DEG"]][["q4"]]))
  } else if (subscale == "Nationality") {
    results[["DEG"]][["q5"]]
  } else if (subscale == "Country Formative Years") {
    results[["DEG"]][["q6"]]
  } else if (subscale == "First Language") {
    results[["DEG"]][["q7"]]
  } else if (subscale == "Second Language") {
    results[["DEG"]][["q8"]]
  } else if (subscale == "Handedness") {
    c(as.numeric(gsub("[^0-9]", "", results[["DEG"]][["q10"]])), as.numeric(gsub("[^0-9]", "", results[["DEG"]][["q11"]])))
  } else {
    mean(scores)
  }
}
