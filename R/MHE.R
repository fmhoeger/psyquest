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
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' When no subscales are provided all subscales are selected.
#' @param ... Further arguments to be passed to \code{\link{MHE}()}.
#' @export
MHE <- function(label = "MHE",
                dict = psyquest::psyquest_dict,
                subscales = c(),
                ...) {
  stopifnot(purrr::is_scalar_character(label))

  elts <-main_test_mhe(
    questionnaire = label,
    label = label,
    items = get_items(label, subscales),
    offset = 1,
    arrange_vertically = TRUE
  )
}

main_test_mhe <- function(questionnaire, label, items, offset = 1, arrange_vertically = TRUE) {
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
              list("choice1", "choice2", "choice3", "choice4"),
              failed_validation_message = psychTestR::i18n("CHOOSE_ANSWER"))
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
              list("choice1", "choice2", "choice3", "choice4"),
              failed_validation_message = psychTestR::i18n("CHOOSE_ANSWER"))
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
              list("choice1", "choice2", "choice3", "choice4"),
              failed_validation_message = psychTestR::i18n("CHOOSE_ANSWER"))
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
              list("choice1", "choice2", "choice3", "choice4"),
              failed_validation_message = psychTestR::i18n("CHOOSE_ANSWER"))
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
              list("choice1", "choice2", "choice3", "choice4"),
              failed_validation_message = psychTestR::i18n("CHOOSE_ANSWER"))
    ),
    dict = psyquest::psyquest_dict
  ))

  psychTestR::join(psychTestR::begin_module(label = questionnaire),
                   elts,
                   scoring(questionnaire, items),
                   psychTestR::end_module())
}

postprocess_mhe <- function(values, scores) {
  sum_parents = nchar(toString(values[1])) + nchar(toString(values[2]))
  scores_map <- psyquest::scoring_maps[["MHE"]]

  raws <- list()
  raws[["ability"]] <- scores_map[scores_map$score == sum_parents,]$raw
  raws[["encourage"]] <- values[6]
  raws[["support"]] <- values[7]

  score_stats <- data.frame(id     = c("ability", "encourage", "support"),
                            mean   = c(0.1143363, 3.156951, 2.769058),
                            sd     = c(0.5637453, 1.116791, 1.258868),
                            weight = c(0.57,      0.87,     0.88))

  value <- 0
  for(var in names(raws)){
    weight <- score_stats[score_stats$id == var, "weight"][1]
    mean <- score_stats[score_stats$id == var, "mean"][1]
    sd <- score_stats[score_stats$id == var, "sd"][1]
    score <- weight * (raws[[var]] - mean) / sd
    value <- value + score
  }
  value
}
