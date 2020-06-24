#' HOP
#'
#' This function defines a HOP module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the HOP in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the HOP,
#' consider using \code{\link{HOP_standalone}()}.
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#' This is also the label given to the results in the output file.
#' @param dict (i18n_dict) The psyquest dictionary used for internationalisation.
#' @param ... Further arguments to be passed to \code{\link{HOP}()}.
#' @export
HOP <- function(label = "HOP",
                dict = psyquest::psyquest_dict,
                ...) {
  stopifnot(purrr::is_scalar_character(label))

  main_test(
    label = label,
    items = get_items(label),
    offset = 1,
    arrange_vertically = TRUE,
    button_style = "min-width: 200px"
  )
}
