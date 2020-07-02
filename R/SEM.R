#' SEM
#'
#' This function defines a SEM module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the SEM in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the SEM,
#' consider using \code{\link{SEM_standalone}()}.
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#' This is also the label given to the results in the output file.
#' @param dict (i18n_dict) The psyquest dictionary used for internationalisation.
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are \code{"Attentiveness"}, \code{"Behavioral Engagement"}, \code{"Cognitive Strategy Use"}, \code{"Education"}, \code{"Emotional Engagement"}, \code{"School belonging"}, \code{"School Compliance"}, \code{"Self-regulated Learning"}, and \code{"Valuing of School Education"}.
#' If no subscales are provided all subscales are selected.
#' @param feedback_page (Function) Defines a feedback page function for displaying
#' the results to the participant at the end of the test. Defaults to NULL.
#' @param ... Further arguments to be passed to \code{\link{SEM}()}.
#' @export
SEM <- function(label = "SEM",
                dict = psyquest::psyquest_dict,
                subscales = c(),
                feedback_page = NULL,
                ...) {
  stopifnot(purrr::is_scalar_character(label))

  main_test(
    label = label,
    items = get_items(label, subscales = subscales),
    subscales = subscales,
    offset = 1,
    feedback_page = feedback_page,
    arrange_vertically = FALSE
  )
}
