library(psychTestR)
library(shiny)
source("R/utils.R")

#' MHE
#'
#' This function defines a MHE module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the MHE in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the MHE,
#' consider using \code{\link{MHE_standalone}()}.
#' @param label (Character scalar) Label to give the MHE results in the output file.
#' @param dict The psyquest dictionary used for internationalisation.
#' @export
MHE <- function(label = "MHE",
                dict = psyquest::psyquest_dict) {
  stopifnot(purrr::is_scalar_character(label))

  elts <-main_test_mhe(
    questionnaire = label,
    label = label,
    num_items = 1,
    offset = 1,
    arrange_vertically = TRUE
  )
}

main_test_mhe <- function(questionnaire, label, num_items, offset = 1, arrange_vertically = TRUE) {
  elts <- c()
  elts <- c(elts, psychTestR::new_timeline(c(
    NOMC_page("q1",
              psychTestR::i18n("TMHE_0001_PROMPT"),
              psychTestR::i18n("TMHE_0002_PROMPT"),
              list(psychTestR::i18n("TMHE_0002_CHOICE1"), psychTestR::i18n("TMHE_0002_CHOICE2")),
              list("choice1", "choice2"))
    ),
    dict = psyquest::psyquest_dict
  ))
  elts <- c(elts, psychTestR::new_timeline(c(
    NOMC_page("q2",
              psychTestR::i18n("TMHE_0001_PROMPT"),
              psychTestR::i18n("TMHE_0003_PROMPT"),
              list(psychTestR::i18n("TMHE_0003_CHOICE1"), psychTestR::i18n("TMHE_0003_CHOICE2")),
              list("choice1", "choice2"))
    ),
    dict = psyquest::psyquest_dict
  ))
  elts <- c(elts, psychTestR::new_timeline(c(
    NAFC_radiobuttons_page("q3",
              psychTestR::i18n("TMHE_0004_PROMPT"),
              psychTestR::i18n("TMHE_0005_PROMPT"),
              list(psychTestR::i18n("TMHE_0005_CHOICE1"),
                   psychTestR::i18n("TMHE_0005_CHOICE2"),
                   psychTestR::i18n("TMHE_0005_CHOICE3"),
                   psychTestR::i18n("TMHE_0005_CHOICE4")),
              list("choice1", "choice2", "choice3", "choice4"))
    ),
    dict = psyquest::psyquest_dict
  ))
  elts <- c(elts, psychTestR::new_timeline(c(
    NAFC_radiobuttons_page("q4",
              psychTestR::i18n("TMHE_0004_PROMPT"),
              psychTestR::i18n("TMHE_0006_PROMPT"),
              list(psychTestR::i18n("TMHE_0006_CHOICE1"),
                   psychTestR::i18n("TMHE_0006_CHOICE2"),
                   psychTestR::i18n("TMHE_0006_CHOICE3"),
                   psychTestR::i18n("TMHE_0006_CHOICE4")),
              list("choice1", "choice2", "choice3", "choice4"))
    ),
    dict = psyquest::psyquest_dict
  ))
  elts <- c(elts, psychTestR::new_timeline(c(
    NAFC_radiobuttons_page("q5",
              psychTestR::i18n("TMHE_0004_PROMPT"),
              psychTestR::i18n("TMHE_0007_PROMPT"),
              list(psychTestR::i18n("TMHE_0007_CHOICE1"),
                   psychTestR::i18n("TMHE_0007_CHOICE2"),
                   psychTestR::i18n("TMHE_0007_CHOICE3"),
                   psychTestR::i18n("TMHE_0007_CHOICE4")),
              list("choice1", "choice2", "choice3", "choice4"))
    ),
    dict = psyquest::psyquest_dict
  ))
  elts <- c(elts, psychTestR::new_timeline(c(
    NAFC_radiobuttons_page("q6",
              psychTestR::i18n("TMHE_0004_PROMPT"),
              psychTestR::i18n("TMHE_0008_PROMPT"),
              list(psychTestR::i18n("TMHE_0008_CHOICE1"),
                   psychTestR::i18n("TMHE_0008_CHOICE2"),
                   psychTestR::i18n("TMHE_0008_CHOICE3"),
                   psychTestR::i18n("TMHE_0008_CHOICE4")),
              list("choice1", "choice2", "choice3", "choice4"))
    ),
    dict = psyquest::psyquest_dict
  ))
  elts <- c(elts, psychTestR::new_timeline(c(
    NAFC_radiobuttons_page("q7",
              psychTestR::i18n("TMHE_0004_PROMPT"),
              psychTestR::i18n("TMHE_0009_PROMPT"),
              list(psychTestR::i18n("TMHE_0009_CHOICE1"),
                   psychTestR::i18n("TMHE_0009_CHOICE2"),
                   psychTestR::i18n("TMHE_0009_CHOICE3"),
                   psychTestR::i18n("TMHE_0009_CHOICE4")),
              list("choice1", "choice2", "choice3", "choice4"))
    ),
    dict = psyquest::psyquest_dict
  ))
  # for (i in 3:7) {
  #   j <- i + 2
  #   elts <- c(elts, psychTestR::new_timeline(c(
  #     NAFC_radiobuttons_page(paste0("q", i),
  #               psychTestR::i18n("TMHE_0004_PROMPT"),
  #               psychTestR::i18n(stringr::str_interp("TMHE_000${j}_PROMPT")),
  #               list(psychTestR::i18n(stringr::str_interp("TMHE_000${j}_CHOICE1")),
  #                    psychTestR::i18n(stringr::str_interp("TMHE_000${j}_CHOICE2")),
  #                    psychTestR::i18n(stringr::str_interp("TMHE_000${j}_CHOICE3")),
  #                    psychTestR::i18n(stringr::str_interp("TMHE_000${j}_CHOICE4"))),
  #               list("choice1", "choice2", "choice3", "choice4"))
  #     ),
  #     dict = psyquest::psyquest_dict
  #   ))
  # }

  psychTestR::join(psychTestR::begin_module(label = questionnaire),
                   elts,
                   scoring(questionnaire),
                   psychTestR::end_module())
}
