library(psychTestR)
library(shiny)
source("R/utils.R")

#' CCM
#'
#' This function defines a CCM module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the CCM in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the CCM,
#' consider using \code{\link{CCM_standalone}()}.
#' @param label (Character scalar) Label to give the CCM results in the output file.
#' @param dict The psyquest dictionary used for internationalisation.
#' @export
CCM <- function(label = "CCM",
                dict = psyquest::psyquest_dict) {
  stopifnot(purrr::is_scalar_character(label))

  elts <-main_test_ccm(
    questionnaire = label,
    label = label,
    num_items = 1,
    offset = 1,
    arrange_vertically = TRUE
  )
}

main_test_ccm <- function(questionnaire, label, num_items, offset = 1, arrange_vertically = TRUE) {
  elts <- c()
  elts <- c(elts, psychTestR::new_timeline(c(
    NOMC_page("q1",
              psychTestR::i18n("TCCM_0001_PROMPT"),
              "",
              list(psychTestR::i18n("TCCM_0001_CHOICE1"),
                   psychTestR::i18n("TCCM_0001_CHOICE2"),
                   psychTestR::i18n("TCCM_0001_CHOICE3"),
                   psychTestR::i18n("TCCM_0001_CHOICE4"),
                   psychTestR::i18n("TCCM_0001_CHOICE5"),
                   psychTestR::i18n("TCCM_0001_CHOICE6"),
                   psychTestR::i18n("TCCM_0001_CHOICE7"),
                   psychTestR::i18n("TCCM_0001_CHOICE8"),
                   psychTestR::i18n("TCCM_0001_CHOICE9")),
              list("choice1",
                   "choice2",
                   "choice3",
                   "choice4",
                   "choice5",
                   "choice6",
                   "choice7",
                   "choice8",
                   "choice9"))
    ),
    dict = psyquest::psyquest_dict
  ))

  elts <- c(elts, psychTestR::new_timeline(c(
    NAFC_radiobuttons_page("q2",
              psychTestR::i18n("TCCM_0002_PROMPT"),
              "",
              list(psychTestR::i18n("TCCM_0002_CHOICE1"),
                   psychTestR::i18n("TCCM_0002_CHOICE2"),
                   psychTestR::i18n("TCCM_0002_CHOICE3"),
                   psychTestR::i18n("TCCM_0002_CHOICE4"),
                   psychTestR::i18n("TCCM_0002_CHOICE5")),
              list("choice1", "choice2", "choice3", "choice4", "choice5"))
    ),
    dict = psyquest::psyquest_dict
  ))
  elts <- c(elts, psychTestR::new_timeline(c(
    NAFC_radiobuttons_page("q3",
              psychTestR::i18n("TCCM_0003_PROMPT"),
              "",
              list(psychTestR::i18n("TCCM_0003_CHOICE1"),
                   psychTestR::i18n("TCCM_0003_CHOICE2"),
                   psychTestR::i18n("TCCM_0003_CHOICE3"),
                   psychTestR::i18n("TCCM_0003_CHOICE4"),
                   psychTestR::i18n("TCCM_0003_CHOICE5")),
              list("choice1", "choice2", "choice3", "choice4", "choice5"))
    ),
    dict = psyquest::psyquest_dict
  ))
  elts <- c(elts, psychTestR::new_timeline(c(
    NAFC_radiobuttons_page("q4",
              psychTestR::i18n("TCCM_0004_PROMPT"),
              "",
              list(psychTestR::i18n("TCCM_0004_CHOICE1"),
                   psychTestR::i18n("TCCM_0004_CHOICE2"),
                   psychTestR::i18n("TCCM_0004_CHOICE3"),
                   psychTestR::i18n("TCCM_0004_CHOICE4"),
                   psychTestR::i18n("TCCM_0004_CHOICE5"),
                   psychTestR::i18n("TCCM_0004_CHOICE6"),
                   psychTestR::i18n("TCCM_0004_CHOICE7")),
              list("choice1", "choice2", "choice3", "choice4", "choice5", "choice6", "choice7"))
    ),
    dict = psyquest::psyquest_dict
  ))
  elts <- c(elts, psychTestR::new_timeline(c(
    NAFC_radiobuttons_page("q5",
              psychTestR::i18n("TCCM_0005_PROMPT"),
              "",
              list(psychTestR::i18n("TCCM_0005_CHOICE1"),
                   psychTestR::i18n("TCCM_0005_CHOICE2"),
                   psychTestR::i18n("TCCM_0005_CHOICE3"),
                   psychTestR::i18n("TCCM_0005_CHOICE4"),
                   psychTestR::i18n("TCCM_0005_CHOICE5"),
                   psychTestR::i18n("TCCM_0005_CHOICE6"),
                   psychTestR::i18n("TCCM_0005_CHOICE7")),
              list("choice1", "choice2", "choice3", "choice4", "choice5", "choice6", "choice7"))
    ),
    dict = psyquest::psyquest_dict
  ))

  psychTestR::join(psychTestR::begin_module(label = questionnaire),
                   elts,
                   scoring(questionnaire),
                   psychTestR::end_module())
}
