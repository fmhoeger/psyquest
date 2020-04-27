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
                language = language,
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
    elts <- c(elts, psychTestR::new_timeline(c(
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
    elts <- c(elts, psychTestR::new_timeline(c(
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
    elts <- c(elts, psychTestR::new_timeline(c(
      conditional(function(state, ...) get_local("hearing_problems", state) == "btn1_text",
        text_input_page("q3",
                        psychTestR::i18n("TDEG_0003_PROMPT"),
                        button_text = psychTestR::i18n("CONTINUE"))
        )),
      dict = psyquest::psyquest_dict
    ))
  }

  if ("TDEG_0004" %in% prompt_ids) {
    elts <- c(elts, psychTestR::new_timeline(c(
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
    nationalities <- c("BRITISH", "AFGHAN", "BULGARIAN", "CHINESE", "GERMAN", "GREEK", "IRANIAN", "IRAQI", "ITALIAN", "KOSOVAN", "POLISH", "ROMANIAN", "RUSSIAN", "SERBIAN", "SYRIAN", "TURKISH", "OTHER_NATIONALITY")
    nationality_acronyms <- c("UK", "AF", "BG", "ZH", "DE", "GR", "IR", "IQ", "IT", "XK", "PL", "RO", "RU", "RS", "SY", "TR", "OTHER")
    if (language == "DE") {
      nationalities <- c("GERMAN", "BRITISH", "AFGHAN", "BULGARIAN", "CHINESE", "GREEK", "IRANIAN", "IRAQI", "ITALIAN", "KOSOVAN", "POLISH", "ROMANIAN", "RUSSIAN", "SERBIAN", "SYRIAN", "TURKISH", "OTHER_NATIONALITY")
      nationality_acronyms <- c("DE", "UK", "AF", "BG", "ZH", "GR", "IR", "IQ", "IT", "XK", "PL", "RO", "RU", "RS", "SY", "TR", "OTHER")
    }
    elts <- c(elts, psychTestR::new_timeline(c(
      dropdown_page("q5",
                psychTestR::i18n("TDEG_0006_PROMPT"),
                setNames(nationality_acronyms, purrr::map(nationalities, psychTestR::i18n)),
                next_button_text = psychTestR::i18n("CONTINUE"))
      ),
      dict = psyquest::psyquest_dict
    ))
  }

  if ("TDEG_0007" %in% prompt_ids) {
    countries <- c("UK", "AFGHANISTAN", "BULGARIA", "CHINA", "GERMANY", "GREECE", "IRAN", "IRAQ", "ITALY", "KOSOVO", "POLAND", "ROMANIA", "RUSSIA", "SERBIA", "SYRIA", "TURKEY", "OTHER_COUNTRY")
    country_acronyms <- c("UK", "AF", "BG", "ZH", "DE", "GR", "IR", "IQ", "IT", "XK", "PL", "RO", "RU", "SR", "AR", "TR", "OTHER")
    if (language == "DE") {
      countries <- c("GERMANY", "UK", "AFGHANISTAN", "BULGARIA", "CHINA", "GREECE", "IRAN", "IRAQ", "ITALY", "KOSOVO", "POLAND", "ROMANIA", "RUSSIA", "SERBIA", "SYRIA", "TURKEY", "OTHER_COUNTRY")
      country_acronyms <- c("DE", "UK", "AF", "BG", "ZH", "GR", "IR", "IQ", "IT", "XK", "PL", "RO", "RU", "SR", "AR", "TR", "OTHER")
    }
    elts <- c(elts, psychTestR::new_timeline(c(
      dropdown_page("q6",
                psychTestR::i18n("TDEG_0007_PROMPT"),
                setNames(country_acronyms, purrr::map(countries, psychTestR::i18n)),
                next_button_text = psychTestR::i18n("CONTINUE"))
      ),
      dict = psyquest::psyquest_dict
    ))
  }

  if ("TDEG_0008" %in% prompt_ids) {
    languages <- c("ENGLISH", "ALBANIAN", "ARABIC", "BULGARIAN", "CHINESE", "FARSI/DARI", "GERMAN", "GREEK", "ITALIAN", "KURDISH", "PASHTO", "POLISH", "ROMANIAN", "RUSSIAN", "SERBIAN", "TURKISH", "OTHER_LANGUAGE")
    language_acronyms <- c("EN", "SQ", "AR", "BG", "ZH", "FS", "DE", "GR", "IT", "KU", "PS", "PL", "RO", "RU", "SR", "TR", "OTHER")
    if (language == "DE") {
      languages <- c("GERMAN", "ENGLISH", "ALBANIAN", "ARABIC", "BULGARIAN", "CHINESE", "FARSI/DARI", "GREEK", "ITALIAN", "KURDISH", "PASHTO", "POLISH", "ROMANIAN", "RUSSIAN", "SERBIAN", "TURKISH", "OTHER_LANGUAGE")
      language_acronyms <- c("DE", "EN", "SQ", "AR", "BG", "ZH", "FS", "GR", "IT", "KU", "PS", "PL", "RO", "RU", "SR", "TR", "OTHER")
    }
    elts <- c(elts, psychTestR::new_timeline(c(
      dropdown_page("q7",
                psychTestR::i18n("TDEG_0008_PROMPT"),
                setNames(language_acronyms, purrr::map(languages, psychTestR::i18n)),
                next_button_text = psychTestR::i18n("CONTINUE"))
      ),
      dict = psyquest::psyquest_dict
    ))
  }

  if ("TDEG_0009" %in% prompt_ids) {
    languages <- c("NONE", "ENGLISH", "ALBANIAN", "ARABIC", "BULGARIAN", "CHINESE", "FARSI/DARI", "GERMAN", "GREEK", "ITALIAN", "KURDISH", "PASHTO", "POLISH", "ROMANIAN", "RUSSIAN", "SERBIAN", "TURKISH", "OTHER_LANGUAGE")
    language_acronyms <- c("NONE", "EN", "SQ", "AR", "BG", "ZH", "FS", "DE", "GR", "IT", "KU", "PS", "PL", "RO", "RU", "SR", "TR", "OTHER")
    elts <- c(elts, psychTestR::new_timeline(c(
      dropdown_page("q8",
                psychTestR::i18n("TDEG_0009_PROMPT"),
                setNames(language_acronyms, purrr::map(languages, psychTestR::i18n)),
                next_button_text = psychTestR::i18n("CONTINUE"))
      ),
      dict = psyquest::psyquest_dict
    ))
  }

  if ("TDEG_0010" %in% prompt_ids) {
    elts <- c(elts, psychTestR::new_timeline(c(
      month_and_year_select_page("q9",
                psychTestR::i18n("TDEG_0010_PROMPT"))
      ),
      dict = psyquest::psyquest_dict
    ))
  }

  if ("TDEG_0011" %in% prompt_ids) {
    elts <- c(elts, psychTestR::new_timeline(c(
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
    elts <- c(elts, psychTestR::new_timeline(c(
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
    tolower(results[["DEG"]][["q7"]])
  } else if (subscale == "Second Language") {
    tolower(results[["DEG"]][["q8"]])
  } else if (subscale == "Handedness") {
    c(as.numeric(gsub("[^0-9]", "", results[["DEG"]][["q10"]])), as.numeric(gsub("[^0-9]", "", results[["DEG"]][["q11"]])))
  } else {
    mean(scores)
  }
}
