#' TOI
#'
#' This function defines a TOI module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the TOI in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the TOI,
#' consider using \code{\link{TOI_standalone}()}.
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#' This is also the label given to the results in the output file.
#' @param dict (i18n_dict) The psyquest dictionary used for internationalisation.
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are \code{"Goals Choice"}, and \code{"Theory of Inteligence"}.
#' If no subscales are provided all subscales are selected.
#' @param feedback_page (Function) Defines a feedback page function for displaying
#' the results to the participant at the end of the test. Defaults to NULL.
#' @param ... Further arguments to be passed to \code{\link{TOI}()}.
#' @export
TOI <- function(label = "TOI",
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
    arrange_vertically = TRUE,
    button_style = "min-width: 326px"
  )
}
