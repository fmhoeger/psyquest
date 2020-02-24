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
#' @param ... Further arguments to be passed to \code{\link{CCM}()}.
#' @export
CCM <- function(label = "CCM",
                dict = psyquest::psyquest_dict,
                ...) {
  stopifnot(purrr::is_scalar_character(label))

  main_test_ccm(
    questionnaire = label,
    label = label,
    num_items = 1,
    offset = 1,
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
                   "choice9"),
              force_answer = TRUE,
              failed_validation_message = psychTestR::i18n("CHOOSE_AT_LEAST_ONE_ANSWER"))
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
              list("choice1", "choice2", "choice3", "choice4", "choice5"),
              failed_validation_message = psychTestR::i18n("CHOOSE_ANSWER"))
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
              list("choice1", "choice2", "choice3", "choice4", "choice5"),
              failed_validation_message = psychTestR::i18n("CHOOSE_ANSWER"))
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
              list("choice1", "choice2", "choice3", "choice4", "choice5", "choice6", "choice7"),
              failed_validation_message = psychTestR::i18n("CHOOSE_ANSWER"))
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
              list("choice1", "choice2", "choice3", "choice4", "choice5", "choice6", "choice7"),
              failed_validation_message = psychTestR::i18n("CHOOSE_ANSWER"))
    ),
    dict = psyquest::psyquest_dict
  ))

  psychTestR::join(psychTestR::begin_module(label = questionnaire),
                   elts,
                   scoring(questionnaire),
                   psychTestR::end_module())
}

postprocess_ccm <- function(subscale, results, scores) {
  if (subscale == "General") {
    count_q1 = length(strsplit(results[["CCM"]][["q1"]], ",")[[1]])
    scores_map <- psyquest::scoring_maps[["CCM"]]
    mapped_value_q1 <- scores_map[scores_map$score == count_q1,]$raw
    values <- c(mapped_value_q1, as.numeric(gsub("[^0-9]", "", results[["CCM"]][["q4"]])), as.numeric(gsub("[^0-9]", "", results[["CCM"]][["q5"]])))

    weights <- c(0.8, 0.88, 0.91)
    means <- c(-1.32900, 1.97, 2.254)
    sds <- c(1.801666, 1.25149, 1.43215)
    value = sum((values - means) * weights / sds)
  } else {
    value = mean(scores)
  }
}
