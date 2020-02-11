library(psychTestR)
library(shiny)
source("R/utils.R")

#' SES
#'
#' This function defines a SES module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the SES in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the SES,
#' consider using \code{\link{SES_standalone}()}.
#' @param label (Character scalar) Label to give the SES results in the output file.
#' @param dict The psyquest dictionary used for internationalisation.
#' @param ... Further arguments to be passed to \code{\link{SES}()}.
#' @export
SES <- function(label = "SES",
                dict = psyquest::psyquest_dict,
                ...) {
  stopifnot(purrr::is_scalar_character(label))

  elts <-main_test_ses(
    questionnaire = label,
    label = label,
    num_items = 1,
    offset = 1,
    arrange_vertically = TRUE
  )
}

main_test_ses <- function(questionnaire, label, num_items, offset = 1, arrange_vertically = TRUE) {
  elts <- c()
  elts <- c(elts, psychTestR::new_timeline(c(
    NAFC_radiobuttons_page("q1",
      "",
      psychTestR::i18n("TSES_0001_PROMPT"),
      list(psychTestR::i18n("TSES_0001_CHOICE1"),
           psychTestR::i18n("TSES_0001_CHOICE2"),
           psychTestR::i18n("TSES_0001_CHOICE3"),
           psychTestR::i18n("TSES_0001_CHOICE4"),
           psychTestR::i18n("TSES_0001_CHOICE5"),
           psychTestR::i18n("TSES_0001_CHOICE6"),
           psychTestR::i18n("TSES_0001_CHOICE7")),
      list("choice1", "choice2", "choice3", "choice4", "choice5", "choice6", "choice7"))
    ),
    dict = psyquest::psyquest_dict
  ))

  elts <- c(elts, psychTestR::new_timeline(c(
    NAFC_radiobuttons_page("q2",
      "",
      psychTestR::i18n("TSES_0002_PROMPT"),
      list(psychTestR::i18n("TSES_0002_CHOICE1"),
           psychTestR::i18n("TSES_0002_CHOICE2"),
           psychTestR::i18n("TSES_0002_CHOICE3"),
           psychTestR::i18n("TSES_0002_CHOICE4"),
           psychTestR::i18n("TSES_0002_CHOICE5"),
           psychTestR::i18n("TSES_0002_CHOICE6"),
           psychTestR::i18n("TSES_0002_CHOICE7")),
      list("choice1", "choice2", "choice3", "choice4", "choice5", "choice6", "choice7"))
    ),
    dict = psyquest::psyquest_dict
  ))

  elts <- c(elts, psychTestR::new_timeline(c(
    NAFC_radiobuttons_page("q3",
      psychTestR::i18n("TSES_0003_PROMPT"),
      psychTestR::i18n("TSES_0004_PROMPT"),
      list(psychTestR::i18n("TSES_0004_CHOICE1"),
           psychTestR::i18n("TSES_0004_CHOICE2"),
           psychTestR::i18n("TSES_0004_CHOICE3")),
      list("choice1", "choice2", "choice3"),
      on_complete = function(answer, state, ...) {
                 set_local("branch", answer, state)
      }
    )),
    dict = psyquest::psyquest_dict
  ))

  elts <- c(elts, psychTestR::new_timeline(c(
    conditional(function(state, ...) get_local("branch", state) == "choice1",
        NAFC_radiobuttons_page("q5",
          "",
          psychTestR::i18n("TSES_0006_PROMPT"),
          list(psychTestR::i18n("TSES_0006_CHOICE1"),
               psychTestR::i18n("TSES_0006_CHOICE2")),
          list("choice1", "choice2"))
    ),
    conditional(function(state, ...) get_local("branch", state) == "choice2",
        NAFC_radiobuttons_page("q4",
          "",
          psychTestR::i18n("TSES_0005_PROMPT"),
          list(psychTestR::i18n("TSES_0005_CHOICE1"),
               psychTestR::i18n("TSES_0005_CHOICE2")),
          list("choice1", "choice2"))
    )),
    dict = psyquest::psyquest_dict
  ))

  elts <- c(elts, psychTestR::new_timeline(c(
    NAFC_radiobuttons_page("q6",
      "",
      psychTestR::i18n("TSES_0007_PROMPT"),
      list(psychTestR::i18n("TSES_0007_CHOICE1"),
           psychTestR::i18n("TSES_0007_CHOICE2"),
           psychTestR::i18n("TSES_0007_CHOICE3"),
           psychTestR::i18n("TSES_0007_CHOICE4"),
           psychTestR::i18n("TSES_0007_CHOICE5"),
           psychTestR::i18n("TSES_0007_CHOICE6"),
           psychTestR::i18n("TSES_0007_CHOICE7"),
           psychTestR::i18n("TSES_0007_CHOICE8")),
      list("choice1", "choice2", "choice3", "choice4", "choice5", "choice6", "choice7", "choice8"))
    ),
    dict = psyquest::psyquest_dict
  ))

  psychTestR::join(psychTestR::begin_module(label = questionnaire),
                   elts,
                   scoring(questionnaire),
                   psychTestR::end_module())
}
