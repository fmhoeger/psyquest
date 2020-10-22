#' SMP(STOMP)
#'
#' This function defines a SMP module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the STOMP in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the SMP,
#' consider using \code{\link{SMP_standalone}()}.
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#' This is also the label given to the results in the output file.
#' @param dict (i18n_dict) The psyquest dictionary used for internationalisation.
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are \code{"Reflective & Complex"}, \code{"Intense & Rebellious"},
#' \code{"Upbeat & Conventional"}, and \code{"Energetic & Rhythmic"}.
#' If no subscales are provided all subscales are selected.
#' @param ... Further arguments to be passed to \code{\link{SMP}()}.
#' @export

SMP <- function(label = "SMP",
                dict = psyquest::psyquest_dict,
                subscales = c(),
                ...) {
  stopifnot(purrr::is_scalar_character(label))

  main_test(
    label = label,
    items = get_items(label,
                      subscales = subscales),
    subscales = subscales,
    offset = 1,
    arrange_vertically = TRUE,
    button_style = "min-width: 284px"
  )
}
