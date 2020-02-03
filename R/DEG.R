#' DEG
#'
#' This function defines a DEG module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the DEG in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the DEG,
#' consider using \code{\link{DEG_standalone}()}.
#' @param label (Character scalar) Label to give the DEG results in the output file.
#' @param dict The psyquest dictionary used for internationalisation.
#' @export
DEG <- function(label = "DEG",
                dict = psyquest::psyquest_dict) {
  stopifnot(purrr::is_scalar_character(label))

  main_test_deg(
    questionnaire = label,
    label = label,
    num_items = 1,
    offset = 1,
    arrange_vertically = TRUE
  )
}

main_test_deg <- function(questionnaire, label, num_items, offset = 1, arrange_vertically = TRUE) {
  elts <- c()
  elts <- c(elts, psychTestR::new_timeline(c(
    NAFC_page("q1",
              psychTestR::i18n("TDEG_0001_PROMPT"),
              sprintf("btn%d_text", 1:2),
              labels = purrr::map(sprintf("TDEG_0001_CHOICE%d", 1:2), psychTestR::i18n)
              )
    ),
    dict = psyquest::psyquest_dict
  ))
  elts <- c(elts, psychTestR::new_timeline(c(
    NAFC_page("q2",
              psychTestR::i18n("TDEG_0002_PROMPT"),
              sprintf("btn%d_text", 1:2),
              labels = purrr::map(sprintf("TDEG_0002_CHOICE%d", 1:2), psychTestR::i18n)
              )
    ),
    dict = psyquest::psyquest_dict
  ))
  elts <- c(elts, psychTestR::new_timeline(c(
    text_input_page("q3",
                    psychTestR::i18n("TDEG_0003_PROMPT"),
                    button_text = psychTestR::i18n("CONTINUE"))
    ),
    dict = psyquest::psyquest_dict
  ))
  elts <- c(elts, psychTestR::new_timeline(c(
    NAFC_page("q4",
              psychTestR::i18n("TDEG_0004_PROMPT"),
              sprintf("btn%d_text", 1:4),
              labels = purrr::map(sprintf("TDEG_0004_CHOICE%d", 1:4), psychTestR::i18n)
              )
    ),
    dict = psyquest::psyquest_dict
  ))

  elts <- c(elts, psychTestR::new_timeline(c(
    dropdown_page("q5",
              psychTestR::i18n("TDEG_0006_PROMPT"),
              purrr::map(c("BRITISH", "AFGHAN", "BULGARIAN", "CHINESE", "GERMAN", "GREEK", "IRANIAN", "IRAQI", "ITALIAN", "KOSOVAN", "POLISH", "ROMANIAN", "RUSSIAN", "SERBIAN", "SYRIAN", "TURKISH", "OTHER_NATIONALITY"), psychTestR::i18n),
              next_button_text = psychTestR::i18n("CONTINUE"))
    ),
    dict = psyquest::psyquest_dict
  ))
  elts <- c(elts, psychTestR::new_timeline(c(
    dropdown_page("q6",
              psychTestR::i18n("TDEG_0007_PROMPT"),
              purrr::map(c("UK", "AFGHANISTAN", "BULGARIA", "CHINA", "GERMANY", "GREECE", "IRAN", "IRAQ", "ITALY", "KOSOVO", "POLAND", "ROMANIA", "RUSSIA", "SERBIA", "SYRIA", "TURKEY", "OTHER_COUNTRY"), psychTestR::i18n),
              next_button_text = psychTestR::i18n("CONTINUE"))
    ),
    dict = psyquest::psyquest_dict
  ))
  elts <- c(elts, psychTestR::new_timeline(c(
    dropdown_page("q7",
              psychTestR::i18n("TDEG_0008_PROMPT"),
              purrr::map(c("ENGLISH", "ALBANIAN", "ARABIC", "BULGARIAN", "CHINESE", "FARSI/DARI", "GERMAN", "GREEK", "ITALIAN", "KURDISH", "PASHTO", "POLISH", "ROMANIAN", "RUSSIAN", "SERBIAN", "TURKISH", "OTHER_LANGUAGE"), psychTestR::i18n),
              next_button_text = psychTestR::i18n("CONTINUE"))
    ),
    dict = psyquest::psyquest_dict
  ))
  elts <- c(elts, psychTestR::new_timeline(c(
    dropdown_page("q8",
              psychTestR::i18n("TDEG_0009_PROMPT"),
              purrr::map(c("NONE", "ENGLISH", "ALBANIAN", "ARABIC", "BULGARIAN", "CHINESE", "FARSI/DARI", "GERMAN", "GREEK", "ITALIAN", "KURDISH", "PASHTO", "POLISH", "ROMANIAN", "RUSSIAN", "SERBIAN", "TURKISH", "OTHER_LANGUAGE"), psychTestR::i18n),
              next_button_text = psychTestR::i18n("CONTINUE"))
    ),
    dict = psyquest::psyquest_dict
  ))
  elts <- c(elts, psychTestR::new_timeline(c(
    month_and_year_select_page("q9",
              psychTestR::i18n("TDEG_0010_PROMPT"))
    ),
    dict = psyquest::psyquest_dict
  ))
  elts <- c(elts, psychTestR::new_timeline(c(
    NAFC_page("q10",
              psychTestR::i18n("TDEG_0011_PROMPT"),
              sprintf("btn%d_text", 1:3),
              labels = purrr::map(sprintf("TDEG_0011_CHOICE%d", 1:3), psychTestR::i18n)
              )
    ),
    dict = psyquest::psyquest_dict
  ))
  elts <- c(elts, psychTestR::new_timeline(c(
    NAFC_page("q11",
              psychTestR::i18n("TDEG_0012_PROMPT"),
              sprintf("btn%d_text", 1:3),
              labels = purrr::map(sprintf("TDEG_0012_CHOICE%d", 1:3), psychTestR::i18n)
              )
    ),
    dict = psyquest::psyquest_dict
  ))

  psychTestR::join(psychTestR::begin_module(label = questionnaire),
                   elts,
                   scoring(questionnaire),
                   psychTestR::end_module())
}
